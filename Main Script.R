# Load libraries
library(tidyverse)
library(here)
library(purrr)
library(stringr)
library(did)
library(lubridate)
library(zoo)        # for rolling sums

## This script classifies firms by a rolling 12-month annualized revenue

# === LOAD AND PREP DATA ===

# 0. Load CODIMP metadata
impuesto_codes <- read_delim(
  here("Documents/El Salvador Research/project/data/raw/impuesto.csv"),
  delim = ";", show_col_types = FALSE
) %>%
  rename(CODIMP = CODIGO) %>%
  mutate(
    CODIMP = as.character(CODIMP),
    NOMBRE = toupper(NOMBRE)
  )

# 1. Load ACTECO → industry mapping (2-digit codes)
actividad <- read_delim(
  here("Documents/El Salvador Research/project/data/raw/actividadEconomica.csv"),
  delim = ";", show_col_types = FALSE
) %>%
  rename(CODIGO = CODIGO, INDUSTRY_NAME = NOMBRE) %>%
  mutate(CODIGO = as.character(CODIGO)) %>%
  filter(str_length(CODIGO) == 2)

# 2. Function to load and clean raw CSVs
load_and_clean <- function(path) {
  read_delim(path, delim = ";", show_col_types = FALSE) %>%
    set_names(~ gsub('"', '', .x)) %>%
    mutate(across(c(ACTECO, CODIMP, MUNICIPIO), ~ str_remove_all(as.character(.), '"'))) %>%
    mutate(
      ANIO      = as.integer(ANIO),
      MES       = as.integer(MES),
      # pad ACTECO to 6 digits, MUNICIPIO to 4 digits (left side with "0")
      ACTECO    = str_pad(ACTECO,    width = 6, side = "left", pad = "0"),
      MUNICIPIO = str_pad(MUNICIPIO, width = 4, side = "left", pad = "0"),
      VALOR     = as.numeric(VALOR)
    )
}

# 3. Load raw tax data
files <- c(
  "DGT_IMPUESTO1616094232377-0112-2021.csv",
  "DGT_IMPUESTO1648495403637-012022-122022.csv",
  "DGT_IMPUESTO1707167152598-122023-122023.csv",
  "DGT_IMPUESTO1738685926586-122024-122024.csv",
  "DGT_IMPUESTO1749758290810-042025-042025.csv"
) %>%
  map_chr(~ here("Documents/El Salvador Research/project/data/raw", .x))

raw_data <- map_dfr(files, load_and_clean)

# 4. Build df_class with firm_id and industry
df_class <- raw_data %>%
  mutate(
    firm_id = str_c(ACTECO, MUNICIPIO, sep = "_"),
    code2d  = str_sub(ACTECO, 1, 2)
  ) %>%
  left_join(actividad, by = c("code2d" = "CODIGO")) %>%
  mutate(industry = if_else(is.na(INDUSTRY_NAME), "Other", INDUSTRY_NAME))

# === BUILD MONTHLY PANEL ===

monthly_data <- df_class %>%
  group_by(firm_id, ANIO, MES, MUNICIPIO) %>%
  summarise(
    revenue0 = sum(VALOR, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    year_month = as.integer(paste0(ANIO, sprintf("%02d", MES))),
    date       = ymd(paste0(ANIO, "-", MES, "-01"))
  )

# Balance panel for diagnostics
all_firms  <- unique(monthly_data$firm_id)
all_months <- sort(unique(monthly_data$year_month))
balanced   <- expand_grid(firm_id = all_firms, year_month = all_months)

monthly_data <- balanced %>%
  left_join(monthly_data, by = c("firm_id", "year_month")) %>%
  mutate(
    ANIO     = as.integer(substr(year_month, 1, 4)),
    MES      = as.integer(substr(year_month, 5, 6)),
    revenue0 = replace_na(revenue0, 0),
    remitted = as.integer(revenue0 > 0),
    revenue  = revenue0 + 1,
    Y        = log(revenue)
  )

# === ROLLING 12-MONTH CLASSIFICATION ===

monthly_data <- monthly_data %>%
  arrange(firm_id, date) %>%
  group_by(firm_id) %>%
  mutate(
    # sum of last-12 revenue0 (partial windows allowed)
    rev_12m     = rollapplyr(revenue0, width = 12, FUN = sum, partial = TRUE, na.rm = TRUE),
    # count of months observed in that window
    months_obs  = rollapplyr(!is.na(revenue0), width = 12, FUN = sum, partial = TRUE),
    # annualize
    rev_annual  = rev_12m * 12 / months_obs,
    # size cutoff
    taxpayer_size = case_when(
      rev_annual > 1e6  ~ "large",
      rev_annual > 1e5  ~ "medium",
      TRUE              ~ "small"
    )
  ) %>%
  ungroup()

# === TREATMENT TIMING & PANEL STRUCTURE ===

monthly_data <- monthly_data %>%
  arrange(year_month) %>%
  mutate(
    treatment_date       = case_when(
      taxpayer_size == "large"  ~ 202307,
      taxpayer_size == "medium" ~ 202310,
      taxpayer_size == "small"  ~ 202407
    ),
    period               = as.integer(factor(year_month)),
    first_treated_period = match(treatment_date, unique(year_month)),
    id                   = as.integer(factor(firm_id)),
    municipality         = MUNICIPIO
  )

final_for_did <- monthly_data %>%
  filter(!is.na(first_treated_period))

# === EVENT STUDY SETUP ===

cs_data <- final_for_did %>%
  filter(taxpayer_size %in% c("small","medium","large")) %>%
  mutate(group_id = if_else(taxpayer_size == "small", first_treated_period, 0))

# 1. ATT on Log Revenue
att_small_revenue <- att_gt(
  yname         = "Y",
  tname         = "period",
  idname        = "id",
  gname         = "group_id",
  data          = cs_data,
  control_group = "notyettreated",
  panel         = TRUE,
  clustervars   = "id"
)

dyn_revenue_6 <- aggte(
  att_small_revenue,
  type  = "dynamic",
  min_e = -12,
  max_e = 12,
  na.rm = TRUE
)

# Plot dynamic ATT: Log Revenue
plot_rev <- tibble(
  event_time = dyn_revenue_6$egt,
  estimate   = dyn_revenue_6$att.egt,
  se         = dyn_revenue_6$se.egt
) %>%
  mutate(
    lower = estimate - 1.96*se,
    upper = estimate + 1.96*se
  ) %>%
  ggplot(aes(event_time, estimate)) +
  geom_line() + geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Dynamic ATT: Log Revenue") +
  theme_minimal()

# 2. ATT on Remittance
att_small_remit <- att_gt(
  yname         = "remitted",
  tname         = "period",
  idname        = "id",
  gname         = "group_id",
  data          = cs_data,
  control_group = "notyettreated",
  panel         = FALSE,
  clustervars   = "id"
)

dyn_remit_6 <- aggte(att_small_remit, type = "dynamic", min_e = -24, max_e = 12, na.rm = TRUE)

# Plot dynamic ATT: Remittance
plot_remit <- tibble(
  event_time = dyn_remit_6$egt,
  estimate   = dyn_remit_6$att.egt,
  se         = dyn_remit_6$se.egt
) %>%
  mutate(
    lower = estimate - 1.96*se,
    upper = estimate + 1.96*se
  ) %>%
  ggplot(aes(event_time, estimate)) +
  geom_line() + geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Dynamic ATT: Remittance") +
  theme_minimal()

# Display plots
print(plot_rev)
print(plot_remit)

# Diagnostic: Total revenue by size
monthly_data %>%
  group_by(date, taxpayer_size) %>%
  summarise(total_revenue = sum(revenue0, na.rm = TRUE), .groups = "drop") %>%
  filter(date >= as.Date("2023-01-01")) %>%
  ggplot(aes(date, total_revenue, color = taxpayer_size)) +
  geom_line() + geom_point() +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
  labs(title = "Total Monthly Revenue by Taxpayer Size") +
  theme_minimal()

# === EXTENSIVE MARGIN EVENT STUDY (overall remittance) ===
# dynamic ATT on extensive margin for all firms (vs not-yet-treated)
att_all_remit <- att_gt(
  yname         = "remitted",
  tname         = "period",
  idname        = "id",
  gname         = "first_treated_period",
  data          = monthly_data,
  control_group = "notyettreated",
  panel         = FALSE,
  clustervars   = "id"
)

dyn_all_remit <- aggte(att_all_remit, type = "dynamic", min_e = -12, max_e = 12, na.rm = TRUE)

# Plot dynamic ATT: Extensive Margin
tibble(
  event_time = dyn_all_remit$egt,
  estimate   = dyn_all_remit$att.egt,
  se         = dyn_all_remit$se.egt
) %>%
  mutate(lower = estimate - 1.96*se, upper = estimate + 1.96*se) %>%
  ggplot(aes(event_time, estimate)) +
  geom_line() + geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Dynamic ATT: Extensive Margin (Remittance)") +
  theme_minimal()

# === ORIGINAL EVENT STUDIES: LARGE vs SMALL ===
ls_data <- monthly_data %>%
  filter(taxpayer_size %in% c("large", "small")) %>%
  mutate(group_id = if_else(taxpayer_size == "large", first_treated_period, 0))

# 1a) ATT on Log Revenue for Large vs Small
att_ls_rev <- att_gt(
  yname         = "Y",
  tname         = "period",
  idname        = "id",
  gname         = "group_id",
  data          = ls_data,
  control_group = "notyettreated",
  panel         = FALSE,
  clustervars   = "id"
)

dyn_ls_rev <- aggte(
  att_ls_rev,
  type  = "dynamic",
  min_e = -12,
  max_e = 12,
  na.rm = TRUE
)

tibble(
  event_time = dyn_ls_rev$egt,
  estimate   = dyn_ls_rev$att.egt,
  se         = dyn_ls_rev$se.egt
) %>%
  mutate(lower = estimate - 1.96*se, upper = estimate + 1.96*se) %>%
  ggplot(aes(event_time, estimate)) +
  geom_line() + geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Dynamic ATT: Log Revenue (Large vs Small)") +
  theme_minimal()

# 1b) ATT on Remittance for Large vs Small
att_ls_remit <- att_gt(
  yname         = "remitted",
  tname         = "period",
  idname        = "id",
  gname         = "group_id",
  data          = ls_data,
  control_group = "notyettreated",
  panel         = FALSE,
  clustervars   = "id"
)
att_gt(
  yname         = "remitted",
  tname         = "period",
  idname        = "id",
  gname         = "group_id",
  data          = ls_data,
  control_group = "notyettreated",
  panel         = FALSE,
  clustervars   = "id"
)

dyn_ls_remit <- aggte(
  att_ls_remit,
  type  = "dynamic",
  min_e = -12,
  max_e = 12,
  na.rm = TRUE
)

tibble(
  event_time = dyn_ls_remit$egt,
  estimate   = dyn_ls_remit$att.egt,
  se         = dyn_ls_remit$se.egt
) %>%
  mutate(lower = estimate - 1.96*se, upper = estimate + 1.96*se) %>%
  ggplot(aes(event_time, estimate)) +
  geom_line() + geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Dynamic ATT: Remittance (Large vs Small)") +
  theme_minimal()

# === ORIGINAL EVENT STUDIES: MEDIUM vs SMALL ===
ms_data <- monthly_data %>%
  filter(taxpayer_size %in% c("medium", "small")) %>%
  mutate(group_id = if_else(taxpayer_size == "medium", first_treated_period, 0))

# 2a) ATT on Log Revenue for Medium vs Small
att_ms_rev <- att_gt(
  yname         = "Y",
  tname         = "period",
  idname        = "id",
  gname         = "group_id",
  data          = ms_data,
  control_group = "notyettreated",
  panel         = FALSE,
  clustervars   = "id"
)

dyn_ms_rev <- aggte(
  att_ms_rev,
  type  = "dynamic",
  min_e = -12,
  max_e = 12,
  na.rm = TRUE
)

tibble(
  event_time = dyn_ms_rev$egt,
  estimate   = dyn_ms_rev$att.egt,
  se         = dyn_ms_rev$se.egt
) %>%
  mutate(lower = estimate - 1.96*se, upper = estimate + 1.96*se) %>%
  ggplot(aes(event_time, estimate)) +
  geom_line() + geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Dynamic ATT: Log Revenue (Medium vs Small)") +
  theme_minimal()

# 2b) ATT on Remittance for Medium vs Small
att_ms_remit <- att_gt(
  yname         = "remitted",
  tname         = "period",
  idname        = "id",
  gname         = "group_id",
  data          = ms_data,
  control_group = "notyettreated",
  panel         = FALSE,
  clustervars   = "id"
)

dyn_ms_remit <- aggte(
  att_ms_remit,
  type  = "dynamic",
  min_e = -12,
  max_e = 12,
  na.rm = TRUE
)

tibble(
  event_time = dyn_ms_remit$egt,
  estimate   = dyn_ms_remit$att.egt,
  se         = dyn_ms_remit$se.egt
) %>%
  mutate(lower = estimate - 1.96*se, upper = estimate + 1.96*se) %>%
  ggplot(aes(event_time, estimate)) +
  geom_line() + geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Dynamic ATT: Remittance (Medium vs Small)") +
  theme_minimal()






### ENTRY‐DATE DIAGNOSTIC

library(ggplot2)

# (a) Find each firm's first month with revenue>0
entry_df <- monthly_data %>%
  filter(revenue0 > 0) %>%
  group_by(firm_id) %>%
  summarise(
    entry_period = min(period),
    .groups = "drop"
  )

# (b) Merge back and compute event‐time of entry
entry_diagnostics <- final_for_did %>%
  distinct(firm_id, taxpayer_size, period, first_treated_period) %>%
  left_join(entry_df, by = "firm_id") %>%
  mutate(
    entry_event = entry_period - first_treated_period
  )

# (c) Tabulate by size‐group how many firms enter near rollout
entry_summary <- entry_diagnostics %>%
  group_by(taxpayer_size) %>%
  summarise(
    total_firms   = n(),  
    near_rollout  = sum(abs(entry_event) <= 6, na.rm = TRUE),
    pct_near_roll = near_rollout / total_firms,
    .groups = "drop"
  )

print(entry_summary)
#> # A tibble: 3 × 4
#>   taxpayer_size total_firms near_rollout pct_near_roll
#>   <chr>                <int>        <int>         <dbl>
#> 1 large                  XXX          XXX          0.XX
#> 2 medium                 XXX          XXX          0.XX
#> 3 small                  XXX          XXX          0.XX

# (d) Optional: a little histogram of entry_event by group
ggplot(entry_diagnostics, aes(x = entry_event, fill = taxpayer_size)) +
  geom_histogram(binwidth = 1, position = "dodge", color = "white") +
  labs(
    title = "Distribution of Firm Entry Times Relative to Rollout",
    x     = "Entry Event‐Time (months)",
    y     = "Number of Firms",
    fill  = "Size Group"
  ) +
  theme_minimal()



# === PLOT: Number of Firms per Industry ===

# 1. Count unique firms in each industry
industry_counts <- df_class %>%
  select(firm_id, industry) %>%
  distinct() %>%
  count(industry, name = "num_firms") %>%
  arrange(desc(num_firms))

# 2. Bar chart: firms per industry
ggplot(industry_counts, aes(x = reorder(industry, num_firms), y = num_firms)) +
  geom_col() +
  geom_text(aes(label = num_firms), hjust = -0.2, color = "black") +
  coord_flip() +
  labs(
    title = "Number of Firms per Industry",
    x     = "Industry",
    y     = "Number of Firms"
  ) +
  theme_minimal()



# Firm count
df_class %>% summarise(n_firms = n_distinct(firm_id)) %>% print()
