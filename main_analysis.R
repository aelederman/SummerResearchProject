# Load libraries
library(tidyverse)
library(here)
library(purrr)
library(stringr)
library(lubridate)
library(zoo)
library(ggplot2)
library(did)
library(scales)
library(patchwork)

# =============================================================
# Title: E-Invoicing in El Salvador: VAT Revenue & Compliance
# Author: Adam Lederman
# Description: Data prep, grouping, and event study estimation
# Last Updated: 7/21/2025
# =============================================================

# === LOAD METADATA ===
# Tax codes
impuesto_codes <- read_delim(
  here("data","raw", "impuesto.csv"),
  delim = ";", show_col_types = FALSE
) %>%
  rename(CODIMP = CODIGO) %>%
  mutate(CODIMP = as.character(CODIMP),
         NOMBRE = toupper(NOMBRE))

# Industry lookup
actividad <- read_delim(
  here("data","raw","actividadEconomica.csv"),
  delim = ";", show_col_types = FALSE
) %>%
  set_names(~ str_remove_all(.x, '"')) %>%
  rename(CODIGO = CODIGO, INDUSTRY_NAME = NOMBRE) %>%
  mutate(CODIGO = as.character(CODIGO)) %>%
  filter(str_length(CODIGO) == 2)

# === DATA LOADER ===
load_and_clean <- function(path) {
  read_delim(path, delim = ";", show_col_types = FALSE) %>%
    set_names(~ str_remove_all(.x, '"')) %>%
    mutate(across(c(ACTECO, CODIMP, MUNICIPIO, DEPARTAMENTO),
                  ~ str_trim(str_remove_all(as.character(.), '"')))) %>%
    mutate(
      ANIO         = as.integer(ANIO),
      MES          = as.integer(MES),
      ACTECO       = str_pad(ACTECO, width = 6, side = "left", pad = "0"),
      DEPARTAMENTO = str_pad(DEPARTAMENTO, width = 2, side = "left", pad = "0"),
      MUNICIPIO    = if_else(
        str_detect(MUNICIPIO, "^[0-9]{1,2}$"),
        str_c(DEPARTAMENTO, str_pad(MUNICIPIO, width = 2, side = "left", pad = "0")),
        str_pad(MUNICIPIO, width = 4, side = "left", pad = "0")
      ),
      VALOR = as.numeric(VALOR)
    )
}

# === LOAD & FILTER TAX DATA ===
# IMPORTANT: Comment out last file if not including 2025 data
files <- c(
  "DGT_IMPUESTO1616094232377-0112-2021.csv",
  "DGT_IMPUESTO1648495403637-012022-122022.csv",
  "DGT_IMPUESTO1707167152598-122023-122023.csv",
  "DGT_IMPUESTO1738685926586-122024-122024.csv" #,
  #"DGT_IMPUESTO1750427962354-052025-052025.csv"
) %>%
  map_chr(~ here("data","raw", .x))
raw_data <- map_dfr(files, load_and_clean)


# === FIRM PANEL SETUP ===
df_class <- raw_data %>%
  mutate(
    firm_id = str_c(ACTECO, MUNICIPIO, sep = "_"),
    code2d  = str_sub(ACTECO, 1, 2)
  ) %>%
  left_join(actividad, by = c("code2d" = "CODIGO")) %>%
  mutate(industry = coalesce(INDUSTRY_NAME, "Other"))

# === BUILD PANEL ===
panel_obs <- df_class %>%
  group_by(firm_id, ANIO, MES) %>%
  summarise(
    revenue0  = sum(VALOR, na.rm = TRUE),
    MUNICIPIO = first(MUNICIPIO),
    .groups   = "drop"
  ) %>%
  mutate(
    year_month = ANIO * 100 + MES,
    date       = ymd(paste(ANIO, MES, 1, sep = "-"))
  )

all_firms  <- unique(panel_obs$firm_id)
all_months <- sort(unique(panel_obs$year_month))

monthly_data <- expand_grid(firm_id = all_firms, year_month = all_months) %>%
  left_join(panel_obs, by = c("firm_id","year_month")) %>%
  left_join(
    panel_obs %>% distinct(firm_id, MUNICIPIO) %>% rename(MUNICIPIO_fm = MUNICIPIO),
    by = "firm_id"
  ) %>%
  mutate(
    ANIO        = floor(year_month/100),
    MES         = year_month %% 100,
    MUNICIPIO   = coalesce(MUNICIPIO, MUNICIPIO_fm),
    revenue0    = replace_na(revenue0, 0),
    remitted    = as.integer(revenue0 > 0),
    revenue     = revenue0 + 1,
    Y           = log(revenue),
    date        = ymd(paste(ANIO, MES, 1, sep = "-"))
  ) %>%
  select(-MUNICIPIO_fm)

# === PRE-TREATMENT GROUPS ===
baseline_end <- ymd("2023-01-01")

first_obs <- panel_obs %>%
  group_by(firm_id) %>%
  summarise(first_date = min(date), .groups = "drop")

firm_baseline <- panel_obs %>%
  filter(date <= baseline_end) %>%
  group_by(firm_id) %>%
  summarise(
    avg_rev_pre = mean(revenue0, na.rm = TRUE) * 12,
    months_obs  = sum(revenue0 > 0, na.rm = TRUE),
    .groups     = "drop"
  )

# Strict grouping logic, includes post-baseline entrants in data visualization
df_class <- df_class %>%
  left_join(first_obs, by = "firm_id") %>%
  left_join(firm_baseline, by = "firm_id") %>%
  mutate(
    size_group_strict = case_when(
      first_date  > baseline_end ~ "no_baseline",
      avg_rev_pre > 1e6         ~ "large",
      avg_rev_pre > 1e5         ~ "medium",
      TRUE                       ~ "small"
    )
  )

# Classic grouping logic, excludes post-baseline entrants for event study (safest)
classic_baseline <- panel_obs %>%
  filter(date <= baseline_end) %>%
  group_by(firm_id) %>%
  summarise(
    avg_rev_pre_classic = mean(revenue0, na.rm = TRUE) * 12,
    .groups = "drop"
  )
df_class <- df_class %>%
  left_join(classic_baseline, by = "firm_id") %>%
  mutate(
    size_group_classic = case_when(
      is.na(avg_rev_pre_classic)    ~ NA_character_,  # drop post-baseline firms
      avg_rev_pre_classic > 1e6     ~ "large",
      avg_rev_pre_classic > 1e5     ~ "medium",
      TRUE                           ~ "small"
    )
  )

# === ASSIGN 10-GROUPS BASED ON PRE-TREATMENT REVENUE ===
target_sizes <- tibble(
  group_id    = paste0("G", 1:10),
  target_size = c(233,239,249,345,657,701,704,4982,14286,15000)
)
firm_groups <- df_class %>%
  distinct(firm_id, avg_rev_pre) %>%
  filter(!is.na(avg_rev_pre)) %>%
  arrange(desc(avg_rev_pre)) %>%
  mutate(row_id = row_number()) %>%
  mutate(
    group_id = cut(
      row_id,
      breaks         = c(0, cumsum(target_sizes$target_size)),
      labels         = target_sizes$group_id,
      include.lowest = TRUE
    )
  ) %>%
  select(firm_id, group_id)

# === FILTER FOR VAT REVENUE ONLY ===
vat_codes <- impuesto_codes %>%
  filter(str_detect(NOMBRE, "IVA")) %>%
  pull(CODIMP)

df_class <- df_class %>%
  filter(CODIMP %in% vat_codes)

# === DEFINE GROUP TIMING ===
group_timing <- tibble(
  group_id      = paste0("G", 1:10),
  notified_date = ymd(c(
    "2023-02-01","2023-05-01","2023-08-01","2023-12-01",
    "2024-02-01","2024-05-01","2024-08-01","2024-12-01",
    "2025-02-01","2025-05-01"
  )),
  treat_date    = ymd(c(
    "2023-07-01","2023-10-01","2024-01-01","2024-04-01",
    "2024-07-01","2024-10-01","2025-01-15","2025-04-01",
    "2025-07-01","2025-10-01"
  ))
)

firm_info <- firm_groups %>% left_join(group_timing, by = "group_id")
df_class <- df_class %>% left_join(firm_info, by = "firm_id")

# === MERGE INTO PANEL AND JOIN GROUPS ===
monthly_data <- monthly_data %>%
  mutate(
    size_group_classic = df_class$size_group_classic[match(firm_id, df_class$firm_id)],
    size_group_strict  = df_class$size_group_strict[match(firm_id, df_class$firm_id)]
  ) %>%
  left_join(firm_info, by = "firm_id") %>%
  mutate(
    notified_ym     = year(notified_date)*100 + month(notified_date),
    treat_ym        = year(treat_date)*100 + month(treat_date),
    period          = as.integer(factor(year_month, sort(unique(year_month)))),
    notified_period = match(notified_ym, sort(unique(year_month))),
    treat_period    = match(treat_ym,    sort(unique(year_month))),
    id              = as.integer(factor(firm_id))
  )

# === DROP OCTOBER 2023 AS MISSING ===
monthly_data <- monthly_data %>%
  filter(!(ANIO == 2023 & MES == 10))

# === FIRM-LEVEL EVENT STUDIES ===
# Dynamic Event Study ATT of log(revenue)
att_global <- att_gt(
  yname         = "Y",
  tname         = "period",
  idname        = "id",
  gname         = "treat_period",
  data          = filter(monthly_data, !is.na(size_group_classic)),
  control_group = "notyettreated",
  panel         = TRUE,
  clustervars   = "id",
  anticipation  = 5
)
dyn_global <- aggte(att_global, type = "dynamic", min_e = -12, max_e = Inf)

# Print summary stats
summary(dyn_global)
write_csv(tidy(dyn_global),here("data","analysis","summary_attrevenue.csv"))

attrevenue <- ggdid(dyn_global) +
  labs(
    title = "Dynamic Event Study: ATT of log(revenue)",
    x     = "Event Time (months)",
    y     = "ATT (log revenue)"
  ) +
  theme_minimal()

ggsave(
  filename = here("data","analysis","attrevenue.png"),
  plot = attrevenue,
  width = 6.5, height = 3.25, units = "in", dpi = 300
)

# Dynamic Event Study ATT of probability remitting
att_rem_c <- att_gt(
  yname         = "remitted",
  tname         = "period",
  idname        = "id",
  gname         = "treat_period",
  data          = filter(monthly_data, !is.na(size_group_classic)),
  control_group = "notyettreated",
  panel         = TRUE,
  clustervars   = "id"
)
dyn_rem_c <- aggte(att_rem_c, type = "dynamic", min_e = -12, max_e = Inf)

# Print summary stats
summary(dyn_rem_c)
write_csv(tidy(dyn_rem_c), here("data","analysis","summary_attremittance.csv"))

attremittance <- ggdid(dyn_rem_c) +
  labs(
    title = "ATT on Remittance",
    x     = "Event Time (months)",
    y     = "ATT Pr(remitted)"
  ) +
  theme_minimal()

ggsave(
  filename = here("data","analysis","attremittance.png"),
  plot = attremittance,
  width = 6.5, height = 3.25, units = "in", dpi = 300
)

# Total Monthly VAT Revenue by Group
tool_data <- monthly_data %>%
  mutate(
    group_id      = firm_info$group_id[match(firm_id, firm_info$firm_id)],
    notified_date = firm_info$notified_date[match(firm_id, firm_info$firm_id)],
    treat_date    = firm_info$treat_date[match(firm_id, firm_info$firm_id)]
  ) %>%
  filter(!is.na(group_id)) %>%
  group_by(group_id, date) %>%
  summarise(
    total_rev     = sum(revenue0, na.rm = TRUE),
    notified_date = first(notified_date),
    treat_date    = first(treat_date),
    .groups       = "drop"
  )

monthlyvat <- ggplot(tool_data, aes(x = date, y = total_rev)) +
  geom_line() +
  geom_vline(aes(xintercept = notified_date), linetype = "dashed") +
  geom_vline(aes(xintercept = treat_date),    linetype = "solid") +
  labs(
    title    = "Monthly VAT Revenue by Group",
    subtitle = "Dashed = notification date; Solid = treatment date",
    x        = "Date",
    y        = "Total Revenue (USD$)"
  ) +
  scale_y_continuous(labels = comma) +
  facet_wrap(~group_id, scales = "free_y", ncol = 2, drop = FALSE) +
  theme_minimal()

ggsave(
  filename = here("data","analysis","monthlyvat.png"),
  plot = monthlyvat,
  width = 6.5, height = 3.25, units = "in", dpi = 300
)

# === GROUP-INDUSTRY-MONTH EVENT STUDIES ===

# Prepare data
monthly_data <- monthly_data %>%
  left_join(df_class %>% distinct(firm_id, industry), by = "firm_id")

group_industry_data <- monthly_data %>%
  filter(!is.na(group_id), !is.na(industry)) %>%
  group_by(group_id, industry, date) %>%
  summarise(total_rev = sum(revenue0, na.rm = TRUE),
            prop_remit = mean(remitted, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(
    Y_agg = log(total_rev + 1),
    month = as.integer(factor(date, levels = sort(unique(date))))
  )

# Add treatment period info
group_timing_trim <- group_timing %>%
  mutate(period = as.integer(factor(treat_date, levels = sort(unique(monthly_data$date))))) %>%
  rename(g = period)

agg_data <- group_industry_data %>%
  left_join(group_timing_trim %>% select(group_id, g), by = "group_id") %>%
  mutate(id = as.integer(factor(paste(group_id, industry))))

# Dynamic ATT on log revenue
agg_att <- att_gt(
  yname = "Y_agg",
  tname = "month",
  idname = "id",
  gname = "g",
  data = agg_data,
  control_group = "notyettreated",
  panel = TRUE,
  clustervars = "id",
  anticipation = 5
)
agg_dyn <- aggte(agg_att, type = "dynamic", min_e = -12, max_e = Inf)
summary(agg_dyn)
write_csv(tidy(agg_dyn), here("data","analysis","summary_grouprevenue.csv"))

grouprevenue <- ggdid(agg_dyn) +
  labs(
    title = "ATT on Log Aggregated VAT Revenue (Group-Industry-Month)",
    x = "Event Time (Months)",
    y = "ATT (log total revenue)"
  ) +
  theme_minimal()

ggsave(
  filename = here("data","analysis","grouprevenue.png"),
  plot = grouprevenue,
  width = 6.5, height = 3.25, units = "in", dpi = 300
)

# Dynamic ATT on Pr(remitting)
agg_remit_att <- att_gt(
  yname = "prop_remit",
  tname = "month",
  idname = "id",
  gname = "g",
  data = agg_data,
  control_group = "notyettreated",
  panel = TRUE,
  clustervars = "id",
  anticipation = 5
)
agg_remit_dyn <- aggte(agg_remit_att, type = "dynamic", min_e = -12, max_e = Inf)
summary(agg_remit_dyn)
write_csv(tidy(agg_remit_dyn), here("data","analysis","summary_groupremittance.csv"))

groupremitting <- ggdid(agg_remit_dyn) +
  labs(
    title = "ATT on Probability of Remitting (Group-Industry-Month)",
    x = "Event Time (Months)",
    y = "ATT Pr(remitted)"
  ) +
  theme_minimal()

ggsave(
  filename = here("data","analysis","groupremittance.png"),
  plot = groupremitting,
  width = 6.5, height = 3.25, units = "in", dpi = 300
)

# Make sure industry and group_id are joined into df_class
firm_counts <- df_class %>%
  filter(!is.na(group_id), !is.na(industry)) %>%
  group_by(group_id, industry) %>%
  summarise(
    n_firms = n_distinct(firm_id),
    .groups = "drop"
  ) %>%
  arrange(group_id, desc(n_firms))


# Log-Revenue by Top-20 Industries
top20_inds <- df_class %>%
  count(industry, sort = TRUE) %>%
  slice_head(n = 20) %>%
  pull(industry)

for (ind in top20_inds) {
  df_i_cl <- monthly_data %>%
    filter(!is.na(size_group_classic), industry == ind)
  att_cl <- att_gt(
    yname         = "Y",
    tname         = "period",
    idname        = "id",
    gname         = "treat_period",
    data          = df_i_cl,
    control_group = "notyettreated",
    panel         = TRUE,
    clustervars   = "id"
  )
  dyn_cl <- aggte(att_cl, type = "dynamic", min_e = -12, max_e = Inf)
  p <- ggdid(dyn_cl) +
    labs(
      title = paste("Log Revenue ATT classic –", ind),
      x     = "Event Time (months)",
      y     = "ATT (log revenue)"
    ) +
    theme_minimal()
  ggsave(
    filename = here("data","analysis",
                    paste0("LogRevenue_ATT_", str_replace_all(ind, "[^A-Za-z0-9]", "_"), ".png")),
    plot     = p,
    width    = 6.5, height = 3.25, units = "in", dpi = 300
  )
}
