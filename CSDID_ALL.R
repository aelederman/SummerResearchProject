# Load libraries
library(tidyverse)
library(here)
library(did)
library(ggplot2)

### DATA CLEANING ###

# 1. Load-and-clean helper
load_and_clean <- function(path) {
  df <- read_delim(here(path), delim = ";", show_col_types = FALSE)
  names(df) <- gsub('"', '', names(df))
  df %>%
    mutate(
      ACTECO     = as.character(ACTECO),
      cell_id    = paste0(ACTECO, "_", MUNICIPIO),
      year_month = as.numeric(paste0(ANIO, sprintf("%02d", MES))),
      date       = as.Date(paste0(year_month, "01"), format = "%Y%m%d")
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
    # Classify firm size
    taxpayer_size = case_when(
      revenue > 1e6 ~ "large",
      revenue > 1e5 ~ "medium",
      TRUE          ~ "small"
    ),
    
    # Assign first‐treatment month in calendar format YYYYMM
    # large → July 2023  (202307)
    # medium → October 2023 (202310)
    # small → July 2024  (202407)
    treatment_date = case_when(
      taxpayer_size == "large"  ~ 202307,
      taxpayer_size == "medium" ~ 202310,
      TRUE                      ~ 202407
    ),
    
    # Re‐create year_month (just to be safe)
    year_month  = as.numeric(paste0(ANIO, sprintf("%02d", MES))),
    date        = as.Date(paste0(year_month, "01"), format = "%Y%m%d"),
    Y           = log1p(revenue)                   # the outcome
  )

#check
head(monthly_data)
count(monthly_data)

# 4. Convert calendar code (202301, 202302, …) into a consecutive integer index “period”
all_months <- sort(unique(monthly_data$year_month))

monthly_data <- monthly_data %>%
  mutate(
    period = match(year_month, all_months),       # 1,2,3,… for each unique year_month
    first_treated_period = match(treatment_date, all_months)         # the index = cohort's first‐treated period
  )

#check
table(monthly_data$treatment_date, monthly_data$first_treated_period)

# 5. Filter out any rows with NA in first_treated_period
monthly_data <- monthly_data %>%
  filter(!is.na(first_treated_period))

# 6. Create a treatment indicator for easier visuals
monthly_data <- monthly_data %>%
  mutate(
    treat = if_else(period >=first_treated_period, 1, 0)
  )

# 7. Assign a unique numeric id for each cell_id
monthly_data <- monthly_data %>%
  mutate(
    id = as.integer(factor(cell_id))  # unique id for each cell_id
  )

  # Check how many units, how many periods
  n_distinct(monthly_data$id)
  n_distinct(monthly_data$period)

# 8. Final panel ready for DiD

final_for_did <- monthly_data %>%
  select(
    id,                      # unique id for each cell
    period,                  # 1,2,3,... for each month
    first_treated_period,    # first treated period index
    Y,                       # the outcome (log revenue)
    treat                    # 0/1 indicator for treatment (technically optional)
    # ... plus any covariates later, like industry, tax payment type, etc.
  )

head(final_for_did)



### CLEANING DATA ERROR FOR SMALL GROUP IN 10.2023 [MEDIUM GROUP CUTOFF] ###

library(dplyr)

# 1. Compute the average log‐revenue (Y) among *all* small firms in Nov and Dec 2023
avg_nov_dec_small <- monthly_data %>%
  filter(
    taxpayer_size == "small",
    year_month %in% c(202311, 202312)
  ) %>%
  summarise(
    avg_Y_nov_dec = mean(Y, na.rm = TRUE)
  ) %>%
  pull(avg_Y_nov_dec)

# 2. Fix October 2023 (202310) for every small‐firm row:
monthly_data_fixed <- monthly_data %>%
  mutate(
    Y = if_else(
      taxpayer_size == "small" & year_month == 202310,
      avg_nov_dec_small,  # replace with the Nov/Dec small‐firm average
      Y                  # otherwise keep original Y
    )
  )

final_for_did <- monthly_data_fixed %>%
  select(
    id,                      # unique id for each cell
    period,                  # 1,2,3,... for each month
    first_treated_period,    # first treated period index
    Y,                       # the outcome (log revenue)
    treat                    # 0/1 indicator for treatment (technically optional)
    # ... plus any covariates later, like industry, tax payment type, etc.
  )


### CALLAWAY SANT'ANNA EVENT STUDY DiD ###
#To use raw data, change final_for_did back to monthly_data
#To use data cleaned for 10.2023 small group error, change final_for did back to monthly_data_imputed
example_attgt <- att_gt(
  yname = "Y",                          # outcome variable
  tname = "period",                     # time index
  idname = "id",                         # unit id
  gname = "first_treated_period",        # treatment group index
  data = final_for_did,                  # the panel data
  control_group = "notyettreated",        # use "notyettreated" for Callaway-Sant'Anna
  panel = FALSE
)

# Summarize and plot results
summary(example_attgt)
ggdid(example_attgt)




### UNDERSTANDING THE DATA ###

#plot revenue over time for large group
ggplot(monthly_data %>% filter(taxpayer_size == "large"), aes(x = date, y = revenue)) +
  geom_line() +
  labs(title = "Monthly Revenue for Large Firms", x = "Date", y = "Revenue") +
  theme_minimal()
#plot revenue over time for medium group
ggplot(monthly_data %>% filter(taxpayer_size == "medium"), aes(x = date, y = revenue)) +
  geom_line() +
  labs(title = "Monthly Revenue for Medium Firms", x = "Date", y = "Revenue") +
  theme_minimal()
#plot revenue over time for small group
ggplot(monthly_data %>% filter(taxpayer_size == "small"), aes(x = date, y = revenue)) +
  geom_line() +
  labs(title = "Monthly Revenue for Small Firms", x = "Date", y = "Revenue") +
  theme_minimal()

#plot total revenue over time for all groups with treatment dates
monthly_data %>%
  group_by(date, taxpayer_size) %>%
  summarise(total_rev = sum(revenue), .groups = "drop") %>%
  ggplot(aes(x = date, y = total_rev, color = taxpayer_size)) +
  geom_line(size = 1) +
  geom_vline(xintercept = as.Date("2023-07-01"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.Date("2023-10-01"), linetype="dashed", color="blue") +
  geom_vline(xintercept = as.Date("2024-07-01"), linetype="dashed", color="green") +
  labs(
    title = "Total Monthly Revenue by Firm Size with Treatment Dates",
    x = "Date",
    y = "Total Revenue"
  ) +
  theme_minimal()

#plot total revenue, not distinguished by groups
monthly_data %>%
  group_by(date) %>%
  summarise(total_rev = sum(revenue), .groups = "drop") %>%
  ggplot(aes(x = date, y = total_rev)) +
  geom_line(size = 1, color = "darkblue") +
  geom_vline(xintercept = as.Date("2023-07-01"), linetype="dashed", color="red") +
  geom_vline(xintercept = as.Date("2023-10-01"), linetype="dashed", color="blue") +
  geom_vline(xintercept = as.Date("2024-07-01"), linetype="dashed", color="green") +
  labs(
    title = "Total Monthly Revenue (All Firms) with Treatment Dates",
    x = "Date",
    y = "Total Revenue"
  ) +
  theme_minimal()


monthly_data %>%
  filter(taxpayer_size == "small", year_month == 202310) %>%
  summarise(
    n = n(),
    min_rev = min(revenue),
    max_rev = max(revenue),
    mean_rev = mean(revenue)
  )
