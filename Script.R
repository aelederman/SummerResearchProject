# Load libraries
library(tidyverse)
library(here)
library(purrr)
library(stringr)
library(lubridate)
library(zoo)
library(ggplot2)
library(did)


### This code is the best working code so far (6/26). It has concise and full data cleaning with two, accurate grouping logics
###   ...classic = excludes firms that enter post-treatment, strict = includes firms that enter post-treatment in a no_baseline taxpayer size group

# === 1) LOAD METADATA ===
# 1a) Tax codes
impuesto_codes <- read_delim(
  here("Documents/El Salvador Research/project/data/raw/impuesto.csv"),
  delim = ";", show_col_types = FALSE
) %>%
  rename(CODIMP = CODIGO) %>%
  mutate(CODIMP = as.character(CODIMP),
         NOMBRE = toupper(NOMBRE))

# 1b) Industry lookup (2-digit ACTECO → name)
actividad <- read_delim(
  here("Documents/El Salvador Research/project/data/raw/actividadEconomica.csv"),
  delim = ";", show_col_types = FALSE
) %>%
  set_names(~ str_remove_all(.x, '"')) %>%
  rename(CODIGO = CODIGO, INDUSTRY_NAME = NOMBRE) %>%
  mutate(CODIGO = as.character(CODIGO)) %>%
  filter(str_length(CODIGO) == 2)

# === 2) CSV LOADER WITH MUNICIPIO CLEANING ===
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

# === 3) LOAD & FILTER TAX FILES ===
files <- c(
  "DGT_IMPUESTO1616094232377-0112-2021.csv",
  "DGT_IMPUESTO1648495403637-012022-122022.csv",
  "DGT_IMPUESTO1707167152598-122023-122023.csv",
  "DGT_IMPUESTO1738685926586-122024-122024.csv",
  "DGT_IMPUESTO1750427962354-052025-052025.csv"
) %>%
  map_chr(~ here("Documents/El Salvador Research/project/data/raw", .x))
raw_data <- map_dfr(files, load_and_clean)

# === 4) BUILD FIRM-LEVEL CROSSWALK ===
df_class <- raw_data %>%
  mutate(
    firm_id = str_c(ACTECO, MUNICIPIO, sep = "_"),
    code2d  = str_sub(ACTECO, 1, 2)
  ) %>%
  left_join(actividad, by = c("code2d" = "CODIGO")) %>%
  mutate(industry = coalesce(INDUSTRY_NAME, "Other"))

# === 5) MUNICIPIO NAMES ===
municipio_lookup <- tribble(
  ~MUNICIPIO, ~municipio_name,
  # ... lookup table ...
)

# === 5b) FILTER FOR VAT REVENUE ONLY ===
vat_codes <- impuesto_codes %>%
  filter(str_detect(NOMBRE, "IVA")) %>%
  pull(CODIMP)

df_class <- df_class %>%
  filter(CODIMP %in% vat_codes)

# === 6) BUILD BALANCED MONTHLY PANEL ===
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
  select(-MUNICIPIO_fm) %>%
  left_join(municipio_lookup, by = "MUNICIPIO") %>%
  mutate(municipio_name = coalesce(municipio_name, "Unknown"))

# === 7) PRE-TREATMENT SIZE GROUPS ===
baseline_end <- ymd("2023-06-30")

# strict grouping logic (new)
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

# classic grouping logic (old, excluding post-baseline entrants)
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

# === 8) ASSIGN 10-GROUPS BASED ON PRE-TREATMENT REVENUE ===
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

# === 9) DEFINE GROUP TIMING ===
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
# 10) BUILD ONE-ROW-PER-FIRM LOOKUP
firm_info <- firm_groups %>% left_join(group_timing, by = "group_id")
# 11) MERGE INTO df_class
df_class <- df_class %>% left_join(firm_info, by = "firm_id")

# 12) MERGE INTO PANEL AND JOIN GROUPS
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

# 13) BASIC EVENT STUDY (classic grouping only)
att_1 <- att_gt(
  yname         = "Y",
  tname         = "period",
  idname        = "id",
  gname         = "treat_period",
  data          = filter(monthly_data, !is.na(size_group_classic)),
  control_group = "notyettreated",
  panel         = FALSE,
  clustervars   = "id"
)

# plot ATT (classic)
ggdid(att_1) +
  labs(
    title = "Callaway–Sant’Anna: ATT of log(revenue) (Classic Groups)",
    x     = "Months Since Treatment",
    y     = "ATT (log revenue)"
  ) +
  theme_minimal()

att_2 <- att_gt(
  yname         = "Y",
  tname         = "period",
  idname        = "id",
  gname         = "notified_period",
  data          = filter(monthly_data, !is.na(size_group_classic)),
  control_group = "notyettreated",
  panel         = FALSE,
  clustervars   = "id"
)

# plot ATT (classic)
ggdid(att_1) +
  labs(
    title = "Callaway–Sant’Anna: ATT of log(revenue) after notification (Classic Groups)",
    x     = "Months Since Notified",
    y     = "ATT (log revenue)"
  ) +
  theme_minimal()

# 14) PLOT TOTAL REVENUE BY CLASSIC SIZE GROUP
rev_by_size <- monthly_data %>%
  filter(!is.na(size_group_classic)) %>%
  group_by(date, size_group_classic) %>%
  summarise(total_rev = sum(revenue0, na.rm = TRUE), .groups = "drop") %>%
  tidyr::complete(date, size_group_classic, fill = list(total_rev = 0))

ggplot(rev_by_size, aes(x = date, y = total_rev, color = size_group_classic)) +
  geom_line(size = 1) +
  labs(
    title = "Total Monthly VAT Revenue by Classic Size Group",
    x     = "Date",
    y     = "Total Revenue (SV₡)",
    color = "Size (Classic)"
  ) +
  theme_minimal()

# 15) PLOT TOTAL REVENUE BY STRICT SIZE GROUP
rev_by_size <- monthly_data %>%
  filter(!is.na(size_group_strict)) %>%
  group_by(date, size_group_strict) %>%
  summarise(total_rev = sum(revenue0, na.rm = TRUE), .groups = "drop") %>%
  tidyr::complete(date, size_group_strict, fill = list(total_rev = 0))

ggplot(rev_by_size, aes(x = date, y = total_rev, color = size_group_strict)) +
  geom_line(size = 1) +
  labs(
    title = "Total Monthly VAT Revenue by Classic Size Group",
    x     = "Date",
    y     = "Total Revenue (SV₡)",
    color = "Size (Classic)"
  ) +
  theme_minimal()





# 16) MORE EVENT STUDIES

# === TACK-ON: CUSTOM EVENT STUDIES WITH ADJUSTABLE WINDOWS ===

library(did)
library(ggplot2)
library(dplyr)

# -- 0) Adjustable event‐time window --
pre_event  <- -12      # months before treatment
post_event <- Inf      # months after treatment (Inf = all)

# -- 1) Ensure industry is in your panel if not already --
monthly_data <- monthly_data %>%
  left_join(
    df_class %>% distinct(firm_id, industry),
    by = "firm_id"
  )

# -- 2) CLASSIC SAMPLE: Log‐Revenue, Large vs Medium & Medium vs Small --
classic_pairs <- list(
  c("large","medium"),
  c("medium","small")
)

for (grp in classic_pairs) {
  lbl  <- paste(grp, collapse = "_vs_")
  df_i <- monthly_data %>%
    filter(!is.na(size_group_classic),
           size_group_classic %in% grp)
  att  <- att_gt(
    yname         = "Y",
    tname         = "period",
    idname        = "id",
    gname         = "treat_period",
    data          = df_i,
    control_group = "notyettreated",
    panel         = FALSE,
    clustervars   = "id"
  )
  dyn  <- aggte(
    att, type = "dynamic",
    min_e = pre_event, max_e = post_event
  )
  print(
    ggdid(dyn) +
      labs(
        title = paste("Log Revenue ATT (classic):", lbl),
        x     = "Event Time (months)",
        y     = "ATT (log revenue)"
      ) +
      theme_minimal()
  )
}

# -- 3) STRICT SAMPLE: Log‐Revenue, Small vs No_Baseline --
df_strict <- monthly_data %>%
  filter(size_group_strict %in% c("small","no_baseline"))
att_s <- att_gt(
  yname         = "Y",
  tname         = "period",
  idname        = "id",
  gname         = "treat_period",
  data          = df_strict,
  control_group = "notyettreated",
  panel         = FALSE,
  clustervars   = "id"
)
dyn_s <- aggte(att_s, type = "dynamic", min_e = pre_event, max_e = post_event)
print(
  ggdid(dyn_s) +
    labs(
      title = "Log Revenue ATT (strict): small vs no_baseline",
      x     = "Event Time (months)",
      y     = "ATT (log revenue)"
    ) +
    theme_minimal()
)

# -- 4) Log‐Revenue by Top-10 Industries, for Classic & Strict --
top10_inds <- df_class %>%
  count(industry, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(industry)

for (ind in top10_inds) {
  # classic
  df_i_cl <- monthly_data %>%
    filter(!is.na(size_group_classic), industry == ind)
  att_cl <- att_gt(
    yname         = "Y",
    tname         = "period",
    idname        = "id",
    gname         = "treat_period",
    data          = df_i_cl,
    control_group = "notyettreated",
    panel         = FALSE,
    clustervars   = "id"
  )
  dyn_cl <- aggte(att_cl, type = "dynamic", min_e = pre_event, max_e = post_event)
  print(
    ggdid(dyn_cl) +
      labs(
        title = paste("Log Revenue ATT classic –", ind),
        x     = "Event Time",
        y     = "ATT (log rev)"
      ) +
      theme_minimal()
  )
  
  # strict
  df_i_st <- monthly_data %>%
    filter(!is.na(size_group_strict), industry == ind)
  att_st <- att_gt(
    yname         = "Y",
    tname         = "period",
    idname        = "id",
    gname         = "treat_period",
    data          = df_i_st,
    control_group = "notyettreated",
    panel         = FALSE,
    clustervars   = "id"
  )
  dyn_st <- aggte(att_st, type = "dynamic", min_e = pre_event, max_e = post_event)
  print(
    ggdid(dyn_st) +
      labs(
        title = paste("Log Revenue ATT strict –", ind),
        x     = "Event Time",
        y     = "ATT (log rev)"
      ) +
      theme_minimal()
  )
}

# -- 5) Remittance Event‐Study for Classic & Strict Samples --

# classic sample
att_rem_c <- att_gt(
  yname         = "remitted",
  tname         = "period",
  idname        = "id",
  gname         = "treat_period",
  data          = filter(monthly_data, !is.na(size_group_classic)),
  control_group = "notyettreated",
  panel         = FALSE,
  clustervars   = "id"
)
dyn_rem_c <- aggte(att_rem_c, type = "dynamic", min_e = pre_event, max_e = post_event)
print(
  ggdid(dyn_rem_c) +
    labs(
      title = "ATT on Remittance (classic)",
      x     = "Event Time",
      y     = "ATT Pr(remitted)"
    ) +
    theme_minimal()
)

# strict sample
att_rem_s <- att_gt(
  yname         = "remitted",
  tname         = "period",
  idname        = "id",
  gname         = "treat_period",
  data          = filter(monthly_data, !is.na(size_group_strict)),
  control_group = "notyettreated",
  panel         = FALSE,
  clustervars   = "id"
)
dyn_rem_s <- aggte(att_rem_s, type = "dynamic", min_e = pre_event, max_e = post_event)
print(
  ggdid(dyn_rem_s) +
    labs(
      title = "ATT on Remittance (strict)",
      x     = "Event Time",
      y     = "ATT Pr(remitted)"
    ) +
    theme_minimal()
)
