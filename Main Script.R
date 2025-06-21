# Load libraries
library(tidyverse)
library(here)
library(purrr)
library(stringr)
library(did)
library(lubridate)
library(zoo)        # for rolling sums

## This script: 1) builds monthly panel, 2) classifies firms at a fixed baseline in 2022, 3) runs CS event studies (intensive & extensive margins)

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

# Balance panel
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

# === ROLLING WINDOW (for diagnostics only) ===
monthly_data <- monthly_data %>%
  arrange(firm_id, date) %>%
  group_by(firm_id) %>%
  mutate(
    rev_12m     = rollapplyr(revenue0, width = 12, FUN = sum, partial = TRUE, na.rm = TRUE),
    months_obs  = rollapplyr(!is.na(revenue0), width = 12, FUN = sum, partial = TRUE),
    rev_annual  = rev_12m * 12 / months_obs
  ) %>%
  ungroup()

# === BASELINE CLASSIFICATION at June 2022 ===
baseline_period <- 202206
baseline_size <- monthly_data %>%
  filter(year_month == baseline_period) %>%
  transmute(
    firm_id,
    taxpayer_size = case_when(
      rev_annual > 1e6  ~ "large",
      rev_annual > 1e5  ~ "medium",
      TRUE              ~ "small"
    )
  )

# Merge static size back
monthly_data <- monthly_data %>%
  select(-rev_12m, -months_obs, -rev_annual) %>%
  left_join(baseline_size, by = "firm_id")

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
    id                   = as.integer(factor(firm_id))
  )

final_for_did <- monthly_data %>% filter(!is.na(first_treated_period))

# === EVENT STUDY SETUP (intensive margin) ===
cs_data <- final_for_did %>%
  filter(taxpayer_size %in% c("small","medium","large")) %>%
  mutate(group_id = first_treated_period)

# 1) ATT on log revenue
att_rev <- att_gt(
  yname         = "Y",
  tname         = "period",
  idname        = "id",
  gname         = "group_id",
  data          = cs_data,
  control_group = "notyettreated",
  panel         = FALSE,
  clustervars   = "id"
)
dyn_rev <- aggte(att_rev, type = "dynamic", min_e = -12, max_e = 12, na.rm = TRUE)

# 2) ATT on remittance
att_remit <- att_gt(
  yname         = "remitted",
  tname         = "period",
  idname        = "id",
  gname         = "group_id",
  data          = cs_data,
  control_group = "notyettreated",
  panel         = FALSE,
  clustervars   = "id"
)
dyn_remit <- aggte(att_remit, type = "dynamic", min_e = -12, max_e = 12, na.rm = TRUE)

# === EXTENSIVE MARGIN: NEW-FIRM ENTRY ===
# 1) first entry period per firm
entry_info <- monthly_data %>%
  filter(revenue0 > 0) %>%
  group_by(firm_id) %>%
  summarise(first_entry_period = min(period), .groups = "drop")

# 2) indicator new_entry
extensive_data <- monthly_data %>%
  left_join(entry_info, by = "firm_id") %>%
  mutate(new_entry = as.integer(period == first_entry_period))

# 3) run CS on new_entry
att_entry <- att_gt(
  yname         = "new_entry",
  tname         = "period",
  idname        = "id",
  gname         = "first_treated_period",
  data          = extensive_data,
  control_group = "notyettreated",
  panel         = FALSE,
  clustervars   = "id"
)
dyn_entry <- aggte(att_entry, type = "dynamic", min_e = -12, max_e = 12, na.rm = TRUE)

# === PLOT RESULTS ===

# 1) Plot dynamic ATT: Log Revenue (intensive margin)
plot_intensive_rev <- tibble(
  event_time = dyn_rev$egt,
  estimate   = dyn_rev$att.egt,
  se         = dyn_rev$se.egt
) %>%
  mutate(
    lower = estimate - 1.96 * se,
    upper = estimate + 1.96 * se
  ) %>%
  ggplot(aes(event_time, estimate)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Dynamic ATT: Log Revenue (Intensive Margin)",
    x     = "Months Since Treatment",
    y     = "ATT"
  ) +
  theme_minimal()
print(plot_intensive_rev)

# 2) Plot dynamic ATT: Remittance (intensive margin)
plot_intensive_remit <- tibble(
  event_time = dyn_remit$egt,
  estimate   = dyn_remit$att.egt,
  se         = dyn_remit$se.egt
) %>%
  mutate(
    lower = estimate - 1.96 * se,
    upper = estimate + 1.96 * se
  ) %>%
  ggplot(aes(event_time, estimate)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Dynamic ATT: Remittance (Intensive Margin)",
    x     = "Months Since Treatment",
    y     = "ATT"
  ) +
  theme_minimal()
print(plot_intensive_remit)

# 3) Plot dynamic ATT: New-Firm Entry (extensive margin)
plot_extensive_entry <- tibble(
  event_time = dyn_entry$egt,
  estimate   = dyn_entry$att.egt,
  se         = dyn_entry$se.egt
) %>%
  mutate(
    lower = estimate - 1.96 * se,
    upper = estimate + 1.96 * se
  ) %>%
  ggplot(aes(event_time, estimate)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Dynamic ATT: First-Time Firm Entry (Extensive Margin)",
    x     = "Months Since Treatment",
    y     = "ATT"
  ) +
  theme_minimal()
print(plot_extensive_entry)
