# Load libraries
library(tidyverse)
library(here)
library(did)
library(ggplot2)

### DATA CLEANING ###

# 0. Read the CODIMP codes to keep from impuesto.csv file
impuesto_codes <- read_delim(
  here("Documents/El Salvador Research/project/data/raw/impuesto.csv"),
  delim = ";",
  show_col_types = FALSE
) %>%
  rename(CODIMP = CODIGO) %>%
  mutate(
    CODIMP = as.character(CODIMP),
    NOMBRE = toupper(NOMBRE)
  )

# 0b. Only keep those rows whose NOMBRE indicates VAT
keep_codes <- impuesto_codes %>%
  filter(str_detect(NOMBRE, "IVA")) %>%
  pull(CODIMP)

# 1. Load-and-clean helper
load_and_clean <- function(path) {
  df <- read_delim(here(path), delim = ";", show_col_types = FALSE)
  names(df) <- gsub('"', '', names(df))
  df %>%
    mutate(
      ACTECO     = as.character(ACTECO),
      cell_id    = paste0(ACTECO, "_", MUNICIPIO),
      year_month = as.numeric(paste0(ANIO, sprintf("%02d", MES))),
      date       = as.Date(paste0(year_month, "01"), format = "%Y%m%d"),
      CODIMP     = as.character(CODIMP)
    )
}

# 2. Read all raw CSVs and bind them together
files <- list(
  "INGRESOS-2017.csv",
  "INGRESOS-2018.csv",
  "2019Final.csv",
  "Ingresos-2020.csv",
  "DGT_IMPUESTO1616094232377-0112-2021.csv",
  "DGT_IMPUESTO1648495403637-012022-122022.csv",
  "DGT_IMPUESTO1707167152598-122023-122023.csv",
  "DGT_IMPUESTO1738685926586-122024-122024.csv",
  "DGT_IMPUESTO1746719935401-032025-032025.csv"
)

raw_data <- map_dfr(
  files,
  ~ load_and_clean(file.path("~/Documents/El Salvador Research/project/data/raw", .x))
)


## CHECK 1/4: track one random cell_id right after binding raw_data ##
set.seed(42)
track_id <- raw_data %>% slice_sample(n = 1) %>% pull(cell_id)
cat("Tracking cell_id =", track_id, "\n")
raw_data %>% filter(cell_id == track_id) %>% head() %>% print()


# Filter to keep only the CODIMP codes that are affected by e-invoicing
raw_data <- raw_data %>%
  filter(CODIMP %in% keep_codes)

## CHECK 2/4: check after CODIMP filter ##
cat("After CODIMP filter, rows for", track_id, "=", nrow(filter(raw_data, cell_id == track_id)), "\n")
raw_data %>% filter(cell_id == track_id) %>% head() %>% print()


# 3. Group each firm's monthly payments into rows of firm-month
monthly_data <- raw_data %>%
  group_by(cell_id, ANIO, MES) %>%
  summarise(
    revenue = sum(VALOR, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  mutate(
    year_month = as.numeric(paste0(ANIO, sprintf("%02d", MES))),
    date       = as.Date(paste0(year_month, "01"), format = "%Y%m%d")
  )


## CHECK 3/4: after summarize ##
cat("Post‐summarize for", track_id, ":\n")
monthly_data %>%
  filter(cell_id == track_id) %>%
  select(ANIO, MES, revenue) %>%
  arrange(ANIO, MES) %>%
  head(12) %>%
  print()


# 3b. Fill in missing firm-months with 0 revenue
full_firm_months <- expand_grid(
  cell_id    = unique(monthly_data$cell_id),
  year_month = sort(unique(monthly_data$year_month))
) %>%
  mutate(date = as.Date(paste0(year_month, "01"), format = "%Y%m%d"))

# 3c. Extensive‐margin dummy + add‐1 + log
monthly_data <- full_firm_months %>%
  left_join(monthly_data, by = c("cell_id", "year_month", "date")) %>%
  mutate(
    revenue0  = replace_na(revenue, 0),          # original revenue
    remitted  = if_else(revenue0 > 0, 1, 0),     # 1 if any VAT paid this month
    revenue   = revenue0 + 1,                    # shift to avoid log(0)
    Y         = log(revenue),                    # outcome
    ANIO      = year(date),
    MES       = month(date)
  )


## CHECK 4/4: after filling zeros, log & remitted ##
cat("After filling & remitted for", track_id, ":\n")
monthly_data %>%
  filter(cell_id == track_id) %>%
  select(year_month, revenue0, remitted, revenue, Y) %>%
  arrange(year_month) %>%
  head(12) %>%
  print()


# Continue with original code
monthly_data <- monthly_data %>%
  mutate(
    taxpayer_size = case_when(
      revenue0 > 1e6 ~ "large",
      revenue0 > 1e5 ~ "medium",
      TRUE           ~ "small"
    ),
    treatment_date = case_when(
      taxpayer_size == "large"  ~ 202307,
      taxpayer_size == "medium" ~ 202310,
      TRUE                      ~ 202407
    )
  )

# 4. Convert calendar code into a consecutive integer index “period”
all_months <- sort(unique(monthly_data$year_month))

monthly_data <- monthly_data %>%
  mutate(
    period               = match(year_month, all_months),
    first_treated_period = match(treatment_date, all_months)
  )

# 5. Filter out any rows with NA in first_treated_period
monthly_data <- monthly_data %>%
  filter(!is.na(first_treated_period))

# 6. Create a treatment indicator
monthly_data <- monthly_data %>%
  mutate(treat = if_else(period >= first_treated_period, 1, 0))

# 7. Assign a unique numeric id
monthly_data <- monthly_data %>%
  mutate(id = as.integer(factor(cell_id))) %>%
  filter(!(taxpayer_size == "small" & year_month == 202310))

# 8. Final panel for DiD (including remitted)
final_for_did <- monthly_data %>%
  select(id, period, first_treated_period, Y, treat, remitted)



### CALLAWAY SANT'ANNA EVENT STUDY DiD ON REVENUE ###
example_attgt <- att_gt(
  yname = "Y",
  tname = "period",
  idname = "id",
  gname = "first_treated_period",
  data = final_for_did,
  control_group = "notyettreated",
  panel = FALSE
)

# Summarize and plot results
summary(example_attgt)
ggdid(example_attgt) +
  labs(
    title = "Event Study: Callaway & Sant'Anna DiD on VAT Log Revenue",
    x     = "Periods Relative to Treatment",
    y     = "ATT (log revenue)"
  ) +
  theme_minimal()



### EVENT STUDY ON DYNAMIC WINDOW (-6 MONTHS) ###

# 1) Aggregate dynamic ATTs from t = –6 up through the maximum lag
dyn_fullpost <- aggte(
  example_attgt,
  type  = "dynamic",
  min_e = -6,
  na.rm = TRUE      # drop any missing ATT(g,e)
)

# 2) Inspect the aggregated results
summary(dyn_fullpost)

# 3) Plot
ggdid(dyn_fullpost) +
  labs(
    title = "Event Study: 6 Months Before & All Months After Treatment",
    x     = "Periods Relative to Treatment",
    y     = "ATT (log revenue)"
  ) +
  theme_minimal()



### EVENT STUDY ON EXTENSIVE MARGIN ###
ext_attgt <- att_gt(
  yname         = "remitted",
  tname         = "period",
  idname        = "id",
  gname         = "first_treated_period",
  data          = final_for_did,
  control_group = "notyettreated",
  panel         = FALSE
)

# Summarize & plot
summary(ext_attgt)
ggdid(ext_attgt) +
  labs(
    title = "Event Study on VAT Remittance",
    y     = "ATT on Probability of Remitting"
  )



### SHARE OF FIRMS REMITTING AROUND MANDATE ###
monthly_data %>%
  filter(period >= first_treated_period - 6,
         period <= first_treated_period + 6) %>%
  group_by(period, taxpayer_size) %>%
  summarise(remit_rate = mean(remitted), .groups = "drop") %>%
  ggplot(aes(x = period, y = remit_rate, color = taxpayer_size)) +
  geom_line(size = 1) +
  geom_vline(xintercept = match(202307, all_months), linetype = "dashed", color = "red") +
  geom_vline(xintercept = match(202310, all_months), linetype = "dashed", color = "green") +
  geom_vline(xintercept = match(202407, all_months), linetype = "dashed", color = "blue") +
  labs(
    title = "Share of Firms Remitting VAT ±6 Months Around Mandate",
    x     = "Period",
    y     = "Fraction Remitting"
  ) +
  theme_minimal()



### EVENT STUDY BY INDUSTRY ###

# Sector mapping from first digit of ACTECO
sector_map <- c(
  "0" = "Not classified",
  "1" = "Agriculture, livestock, forestry, fishing",
  "2" = "Mining and quarrying",
  "3" = "Manufacturing",
  "4" = "Electricity, gas, water, waste",
  "5" = "Construction",
  "6" = "Wholesale and retail trade",
  "7" = "Transportation and storage",
  "8" = "Accommodation and food services",
  "9" = "Information, finance, real estate, education, health, etc."
)

library(stringr)

# 1) Build a mini‐panel with sector info
sector_panel <- monthly_data %>%
  mutate(
    id     = as.integer(factor(cell_id)),
    sector = recode(str_sub(cell_id, 1, 1), !!!sector_map)
  ) %>%
  select(id, sector, period, first_treated_period, Y, treat)

# 2) Loop over each sector; require ≥2 distinct treatment timings
for(sec in unique(sector_panel$sector)) {
  df_s <- filter(sector_panel, sector == sec)
  
  if (n_distinct(df_s$first_treated_period) < 2) {
    message("Skipping sector '", sec, "': only one treatment timing")
    next
  }
  
  # estimate CS‐DiD
  att_s <- att_gt(
    yname         = "Y",
    tname         = "period",
    idname        = "id",
    gname         = "first_treated_period",
    data          = df_s,
    control_group = "notyettreated",
    panel         = FALSE
  )
  
  # aggregate to ±6 leads/lags, dropping any missing ATTs
  dyn_s <- aggte(
    att_s,
    type  = "dynamic",
    min_e = -6,
    max_e =  6,
    na.rm = TRUE       # <-- drop missing values
  )
  
  # plot
  print(
    ggdid(dyn_s) +
      labs(
        title = paste0("VAT Event Study by Industry: ", sec),
        x     = "Periods Relative to Mandate",
        y     = "ATT (log revenue)"
      ) +
      theme_minimal()
  )
}



### (FAILED) PARALLEL TRENDS : MATCHED FIRMS EVENT STUDY ###

# #library(dplyr)
# 
# # for clarity, work off a copy
# #df <- monthly_data
#
# only need periods before treatment
# pre_trend <- df %>%
#   filter(period < first_treated_period) %>%
#   group_by(id) %>%
#   summarise(
#     slope = coef(lm(Y ~ period, data = cur_data()))[2],
#     .groups = "drop"
#   )
# 
# # treated firms (ever treated)
# treated_ids <- unique(df$id[df$treat==1])
# # controls: firms treated strictly after the max post-treatment period,
# # or never treated in your sample
# control_ids <- setdiff(pre_trend$id, treated_ids)
# 
# # Combine slopes with treatment periods
# pre_trend2 <- pre_trend %>%
#   left_join(
#     monthly_data %>% distinct(id, first_treated_period),
#     by = "id"
#   )
# 
# # Find the maximum treatment period
# max_g <- max(pre_trend2$first_treated_period, na.rm = TRUE)
# 
# # For each treated firm (not in the last cohort), match to one later-treated firm
# matches <- pre_trend2 %>%
#   filter(first_treated_period < max_g) %>%  # drop the final cohort
#   rowwise() %>%
#   mutate(
#     control = {
#       this_g     <- first_treated_period
#       this_slope <- slope
#       # only firms treated after this one
#       cand <- pre_trend2 %>% filter(first_treated_period > this_g)
#       # pick the id whose slope is closest
#       cand$id[ which.min(abs(cand$slope - this_slope)) ]
#     }
#   ) %>%
#   ungroup() %>%
#   select(treated = id, control)
# 
# # Subset your panel to matched firms
# matched_ids <- unique(c(matches$treated, matches$control))
# matched_df  <- monthly_data %>% filter(id %in% matched_ids)
# 
# # Re-run CS-DiD on the matched sample
# matched_attgt <- att_gt(
#   yname         = "Y",
#   tname         = "period",
#   idname        = "id",
#   gname         = "first_treated_period",
#   data          = matched_df,
#   control_group = "notyettreated",
#   panel         = FALSE
# )
# 
# summary(matched_attgt)
# ggdid(matched_attgt)



### TWFE ###

#install.packages("data.table", dependencies = TRUE)
#install.packages("ggfixest")
library(fixest)
library(ggfixest)

# 1. Estimate your static TWFE DiD
twfe_mod <- feols(
  Y ~ treat | id + period,
  data    = final_for_did,
  cluster = ~id
)

# 2. Coefficient plot of the single DiD effect
ggcoefplot(
  twfe_mod,
  n         = 1,                     # only show the 'treat' coefficient
  conf_level = 0.95,                 # 95% CI
  dot_args   = list(size = 4),       # make the point larger
  whisker_args = list(size = 1)      # thicker error bars
) +
  labs(
    title = "TWFE DiD: Average Effect of VAT Mandate",
    x     = "Estimate (log revenue)",
    y     = ""
  ) +
  theme_minimal()

# 3. Estimate a dynamic/event‐study version in a manageable +- 6 period window
final_sub <- final_for_did %>%
  mutate(rel_period = period - first_treated_period) %>%
  filter(rel_period >= -6, rel_period <= 6)

es_mod <- feols(
  Y ~ i(rel_period, treat, ref = -1),
  fixef = c("id", "period"),
  data = final_sub,
  cluster = ~id,
  lean = TRUE
)

# 4. Event‐study plot of the leads & lags
ggiplot(
  es_mod,
  ref.line   = -1,      # vertical line at the omitted (k = -1) period
  geom_style = "ribbon" # shaded CI bands
) +
  labs(
    title = "TWFE Event Study: Dynamic Effects of VAT Mandate",
    x     = "Periods since treatment",
    y     = "Effect on log revenue"
  ) +
  theme_minimal()


### Two‐Stage Manual IV: LATE of remitting on log‐revenue ###

# 1. First stage: how much does the mandate move remitting?
first_stage <- feols(
  remitted ~ treat      # endogenous only instrumented by treat
  | id + period,      # firm & time FE
  data    = final_for_did,
  cluster = ~id
)
summary(first_stage)

# 1b. Add the fitted (predicted) values to your panel
final_for_did <- final_for_did %>%
  mutate(remitted_hat = predict(first_stage, newdata = final_for_did))

# 2. Second stage: effect of that exogenized remitting on log‐revenue
tsls_mod <- feols(
  Y ~ remitted_hat      # now exogenous regressor
  | id + period,      # same FE
  data    = final_for_did,
  cluster = ~id
)
summary(tsls_mod)

# 3. Plot the LATE
ggcoefplot(
  tsls_mod,
  n            = 1,          # only the remitted_hat coefficient
  conf_level   = 0.95,
  dot_args     = list(size = 4),
  whisker_args = list(size = 1)
) +
  labs(
    title = "IV Estimate: Effect of VAT Remittance on Log Revenue",
    x     = "LATE estimate (log revenue)",
    y     = ""
  ) +
  theme_minimal()

# 3b. Plot remitting
final_for_did %>%
  group_by(period) %>%
  summarise(remitted_rate = mean(remitted), .groups = "drop") %>%
  ggplot(aes(x = period, y = remitted_rate)) +
  geom_line(size = 1, color = "blue") +
  labs(
    title = "Share of Firms Remitting VAT Over Time",
    x     = "Period",
    y     = "Share Remitting"
  ) +
  theme_minimal()



### PLACEBO TEST ###

# 1. Build the full placebo panel (6 periods before the real treatment)
pw <- 6
placebo_for_did <- monthly_data %>%
  mutate(
    first_treated_placebo = first_treated_period - pw,           # shift treatment 6 periods earlier
    treat_placebo        = if_else(period >= first_treated_placebo, 1, 0),
    id                   = as.integer(factor(cell_id))
  ) %>%
  filter(!is.na(first_treated_placebo))                         # drop any firms where shift goes before your sample

# 2. Estimate placebo event‐study on the full panel
placebo_attgt <- att_gt(
  yname         = "Y",                       # log‐revenue outcome
  tname         = "period",
  idname        = "id",
  gname         = "first_treated_placebo",
  data          = placebo_for_did,
  control_group = "notyettreated",
  panel         = FALSE
)

# 3. Aggregate to a dynamic (leads/lags) window of ±6
dyn_placebo <- aggte(
  placebo_attgt,
  type  = "dynamic",
  min_e = -pw,
  max_e =  pw,
  na.rm = TRUE
)

# 4. Plot the ±6 placebo event‐study
ggdid(dyn_placebo) +
  labs(
    title = paste0("Placebo Event Study (±", pw, " periods)"),
    x     = "Periods Relative to Fake Mandate",
    y     = "ATT (log revenue, placebo)"
  ) +
  theme_minimal()


### PLOT NUMBER OF FIRMS BY INDUSTRY ###

monthly_data %>%
  filter(ANIO == 2024) %>%
  distinct(cell_id) %>%
  mutate(
    acteco_code  = str_extract(cell_id, "^[^_]+"),
    acteco_digit = str_sub(str_pad(acteco_code, width = 2, pad = "0"), 1, 1),
    sector       = recode(acteco_digit, !!!sector_map)
  ) %>%
  count(sector, sort = TRUE) %>%
  ggplot(aes(x = reorder(sector, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.1, size = 4) +
  labs(
    title = "Number of Unique Firms by Industry in 2024",
    x     = "Industry Sector",
    y     = "Number of Firms"
  ) +
  theme_minimal()


# Plot VAT revenue over time for each group, with vertical lines for mandatory dates
monthly_data %>%
  group_by(date, taxpayer_size) %>%
  summarise(total_rev = sum(revenue0), .groups = "drop") %>%
  ggplot(aes(x = date, y = total_rev, color = taxpayer_size)) +
  geom_line(size = 1) +
  geom_vline(xintercept = as.Date("2023-07-01"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.Date("2023-10-01"), linetype="dashed", color="green") +
  geom_vline(xintercept = as.Date("2024-07-01"), linetype="dashed", color="blue") +
  labs(
    title = "Total Monthly VAT Revenue by Firm Size with Treatment Dates",
    x     = "Date",
    y     = "Total Revenue"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot total VAT revenue
monthly_data %>%
  group_by(date) %>%
  summarise(total_rev = sum(revenue0), .groups = "drop") %>%
  ggplot(aes(x = date, y = total_rev)) +
  geom_line(size = 1, color = "darkblue") +
  geom_vline(xintercept = as.Date("2023-07-01"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.Date("2023-10-01"), linetype="dashed", color="green") +
  geom_vline(xintercept = as.Date("2024-07-01"), linetype="dashed", color="blue") +
  labs(
    title = "Total Monthly VAT Revenue with Treatment Dates",
    x     = "Date",
    y     = "Total Revenue"
  ) +
  theme_minimal()
