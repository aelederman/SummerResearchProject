### TWO WAY FIXED EFFECTS TO CHECK NEGATIVE RESULTS ###

# Load libraries
library(tidyverse)
library(here)
library(ggplot2)
library(lubridate)
# for TWFE 
# install.packages("fixest")
library(fixest)

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
  filter(str_detect(NOMBRE, "IVA")) %>%
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

# 10. TWFE DiD Estimation
twfe_model <- feols(
  Y ~ treat | id + period,
  data    = final_for_did,
  cluster = ~id
)

summary(twfe_model)



### PLOT ###

# install.packages("broom")  # if you don’t have it
library(broom)

coef_df <- tidy(twfe_model) %>%
  filter(term == "treat") %>% 
  mutate(
    lower = estimate - 1.96*std.error,
    upper = estimate + 1.96*std.error
  )

ggplot(coef_df, aes(x = term, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  labs(
    x     = NULL,
    y     = "Estimate (log-revenue)",
    title = "TWFE DiD: Treatment Effect"
  ) +
  theme_minimal()

# Attach fitted values
pred_df <- final_for_did %>%
  mutate(
    fitted_Y = predict(twfe_model, newdata = .),
    group    = if_else(treat == 1, "Treated", "Control")
  ) %>%
  group_by(period, group) %>%
  summarise(
    actual   = mean(Y),
    fitted   = mean(fitted_Y),
    .groups  = "drop"
  )

ggplot(pred_df, aes(x = period)) +
  geom_line(aes(y = actual, color = group), linetype = "solid") +
  geom_line(aes(y = fitted, color = group), linetype = "dashed") +
  labs(
    x     = "Period",
    y     = "Avg log-revenue",
    color = "",
    title = "Actual vs Fitted Trends by Group",
    subtitle = "Solid = actual; dashed = TWFE fit"
  ) +
  theme_minimal()


# Extract all FEs as a list
fe_list   <- fixef(twfe_model)

# Pull out the time‐effects
time_fe   <- fe_list[["period"]]

# Turn into a tibble for plotting
time_fe_df <- tibble(
  period  = as.integer(names(time_fe)),
  time_fe = unname(time_fe)
)

# Plot
library(ggplot2)
ggplot(time_fe_df, aes(x = period, y = time_fe)) +
  geom_line() +
  geom_point() +
  labs(
    x     = "Period",
    y     = "Estimated Time FE",
    title = "TWFE DiD: Time Fixed Effects"
  ) +
  theme_minimal()

# Extract one row per firm
treatment_dates <- final_for_did %>%
  group_by(id) %>%
  summarise(first_treated_period = min(period[treat==1])) 

# Histogram of treatment dates
library(ggplot2)
ggplot(treatment_dates, aes(x = first_treated_period)) +
  geom_histogram(binwidth = 1, colour = "white") +
  labs(
    x = "Period of Mandated E-Invoicing Rollout",
    y = "Number of Firms",
    title = "Distribution of Treatment Dates Across Firms"
  ) +
  theme_minimal()


### HEATMAP USING SAMPLE TO CONFIRM ACCURATE FIRM TREATMENT DATES ###

set.seed(42)
sample_ids <- sample(unique(final_for_did$id), 1000)

plot_df <- final_for_did %>%
  filter(id %in% sample_ids) %>%
  mutate(treated = treat==1)

ggplot(plot_df, aes(x = period, y = factor(id), fill = treated)) +
  geom_tile() +
  scale_fill_manual(values = c("grey90","steelblue"), name = "Post-Treatment") +
  labs(
    x = "Period",
    y = "Firm (sample)",
    title = "Firm-Level Adoption Timeline of E-Invoicing"
  ) +
  theme_minimal()
