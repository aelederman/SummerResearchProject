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

# Only keep those rows whose NOMBRE indicates VAT
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
      CODIMP     = as.character(CODIMP)  # ensure CODIMP is character
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

# Filter to keep only the CODIMP codes that are affected by e-invoicing
raw_data <- raw_data %>%
  filter(CODIMP %in% keep_codes)

head(raw_data)
count(raw_data)

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
    date = as.Date(paste0(year_month, "01"), format = "%Y%m%d")
  )

# 3b. Fill in missing firm-months with 0 revenue
full_firm_months <- expand_grid(
  cell_id = unique(monthly_data$cell_id),
  year_month = sort(unique(monthly_data$year_month))
) %>%
  mutate(date = as.Date(paste0(year_month, "01"), format = "%Y%m%d"))

monthly_data <- full_firm_months %>%
  left_join(monthly_data, by = c("cell_id", "year_month", "date")) %>%
  mutate(
    revenue = replace_na(revenue, 0),
    revenue = revenue + 1,        # add 1 to avoid log(0)
    Y = log(revenue),
    ANIO = year(date),
    MES = month(date)
  )

# Continue with original code
monthly_data <- monthly_data %>%
  mutate(
    taxpayer_size = case_when(
      revenue > 1e6 ~ "large",
      revenue > 1e5 ~ "medium",
      TRUE          ~ "small"
    ),
    treatment_date = case_when(
      taxpayer_size == "large"  ~ 202307,
      taxpayer_size == "medium" ~ 202310,
      TRUE                      ~ 202407
    )
  )

# 4. Convert calendar code (202301, 202302, …) into a consecutive integer index “period”
all_months <- sort(unique(monthly_data$year_month))

monthly_data <- monthly_data %>%
  mutate(
    period = match(year_month, all_months),
    first_treated_period = match(treatment_date, all_months)
  )

# check
table(monthly_data$treatment_date, monthly_data$first_treated_period)

# 5. Filter out any rows with NA in first_treated_period
monthly_data <- monthly_data %>%
  filter(!is.na(first_treated_period))

# 6. Create a treatment indicator for easier visuals
monthly_data <- monthly_data %>%
  mutate(treat = if_else(period >= first_treated_period, 1, 0))

# 7. Assign a unique numeric id for each cell_id
monthly_data <- monthly_data %>%
  mutate(id = as.integer(factor(cell_id)))

# Check how many units, how many periods
n_distinct(monthly_data$id)
n_distinct(monthly_data$period)

# 8. Final panel ready for DiD
final_for_did <- monthly_data %>%
  select(id, period, first_treated_period, Y, treat)

# Drop small firms in 2023.10 (October 2023)
monthly_data_fixed <- monthly_data %>%
  filter(!(taxpayer_size == "small" & year_month == 202310))

# Rebuild final_for_did on filtered data
final_for_did <- monthly_data_fixed %>%
  select(id, period, first_treated_period, Y, treat)

### CALLAWAY SANT'ANNA EVENT STUDY DiD ###
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
ggdid(example_attgt)

# Plot total (selected) payments over time
monthly_data %>%
  group_by(date) %>%
  summarise(total_rev = sum(revenue), .groups = "drop") %>%
  ggplot(aes(x = date, y = total_rev)) +
  geom_line(size = 1, color = "darkblue") +
  geom_vline(xintercept = as.Date("2023-07-01"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.Date("2023-10-01"), linetype="dashed", color="blue") +
  geom_vline(xintercept = as.Date("2024-07-01"), linetype="dashed", color="green") +
  labs(
    title = "Total Monthly Internal Transactions Revenue (All Firms) with Treatment Dates",
    x = "Date",
    y = "Total Revenue"
  ) +
  theme_minimal()
