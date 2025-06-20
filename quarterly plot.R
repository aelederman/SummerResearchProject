# Load libraries
library(tidyverse)
library(here)
library(did)
library(ggplot2)
library(lubridate)
library(dplyr)


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

# Only keep those rows whose NOMBRE indicates Internal Transactions
keep_codes <- impuesto_codes %>%
  filter(str_detect(NOMBRE, "IVA")) %>% #|TRANSFERENCIA DE BIENES MUEBLES|RETENCION|RENTA")) %>%
  pull(CODIMP)

# 1. Load-and-clean helper
load_and_clean <- function(path) {
  df <- read_delim(here(path), delim = ";", show_col_types = FALSE)
  names(df) <- gsub('"', '', names(df))
  df %>%
    mutate(
      ACTECO    = as.character(ACTECO),
      cell_id   = paste0(ACTECO, "_", MUNICIPIO),
      CODIMP    = as.character(CODIMP),
      ANIO      = as.integer(ANIO),
      MES       = as.integer(MES),
      quarter   = ceiling(MES / 3),
      year_quarter = paste0(ANIO, "Q", quarter)
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

# 3. Aggregate firm-quarter revenue
quarterly_data <- raw_data %>%
  group_by(cell_id, year_quarter) %>%
  summarise(revenue = sum(VALOR, na.rm = TRUE), .groups = "drop")

# 3b. Fill in missing firm-quarters with 0 revenue
all_quarters <- sort(unique(quarterly_data$year_quarter))
full_firm_quarters <- expand_grid(
  cell_id = unique(quarterly_data$cell_id),
  year_quarter = all_quarters
)

quarterly_data <- full_firm_quarters %>%
  left_join(quarterly_data, by = c("cell_id", "year_quarter")) %>%
  mutate(
    revenue = replace_na(revenue, 0),
    revenue = revenue + 1,   # avoid log(0)
    Y = log(revenue)
  )

# 3c. Assign taxpayer size and treatment quarter
quarterly_data <- quarterly_data %>%
  mutate(
    taxpayer_size = case_when(
      revenue > 3e6 ~ "large",
      revenue > 3e5 ~ "medium",
      TRUE          ~ "small"
    ),
    treatment_quarter = case_when(
      taxpayer_size == "large"  ~ "2023Q3",
      taxpayer_size == "medium" ~ "2023Q4",
      TRUE                      ~ "2024Q3"
    )
  )

# 4. Convert year_quarter to period number
all_periods <- tibble(year_quarter = sort(unique(quarterly_data$year_quarter))) %>%
  mutate(period = row_number())

quarterly_data <- quarterly_data %>%
  left_join(all_periods, by = "year_quarter") %>%
  left_join(all_periods %>% rename(treatment_quarter = year_quarter, first_treated_period = period),
            by = "treatment_quarter")

# 5. Filter out rows with NA in first_treated_period
quarterly_data <- quarterly_data %>%
  filter(!is.na(first_treated_period))

# 6. Create treatment dummy
quarterly_data <- quarterly_data %>%
  mutate(treat = if_else(period >= first_treated_period, 1, 0))

# 7. Assign unique id per firm
quarterly_data <- quarterly_data %>%
  mutate(id = as.integer(factor(cell_id)))

# 8. Final panel for DiD
final_for_did <- quarterly_data %>%
  select(id, period, first_treated_period, Y, treat)

# 9. Drop small firms in 2023Q4
final_for_did <- final_for_did %>%
  anti_join(
    quarterly_data %>%
      filter(taxpayer_size == "small", year_quarter == "2023Q4") %>%
      select(id, period),
    by = c("id", "period")
  )



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


#plot results
ggdid(example_attgt) +
  labs(
    title = "Impact of E-Invoicing on Revenue",
    x = "Periods (quarters)",
    y = "Average Revenue (log scale)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 1. Build a proper date and sum to get monthly total revenue
monthly_data <- raw_data %>%
  mutate(
    # ANIO and MES are already integers from your cleaning step
    date = make_date(ANIO, MES, 1)
  ) %>%
  group_by(date) %>%
  summarise(
    total_rev = sum(VALOR, na.rm = TRUE),
    .groups   = "drop"
  )

# 2. Plot
monthly_data %>%
  ggplot(aes(x = date, y = total_rev)) +
  geom_line(size = 1, color = "darkblue") +
  geom_vline(xintercept = as.Date("2023-07-01"), linetype = "dashed", color = "red")   +
  geom_vline(xintercept = as.Date("2023-10-01"), linetype = "dashed", color = "blue")  +
  geom_vline(xintercept = as.Date("2024-07-01"), linetype = "dashed", color = "green") +
  labs(
    title = "Total Monthly Internal Transactions Revenue (All Firms) with Treatment Dates",
    x     = "Date",
    y     = "Total Revenue"
  ) +
  theme_minimal() +
  theme(
    plot.title  = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


