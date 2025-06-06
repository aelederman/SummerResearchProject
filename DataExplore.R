### ORGANIZE CELL-LEVEL DATA FROM 2023 AND 2024 ###

# Load required libraries
library(tidyverse)
library(here)

# Load data with semicolon delimiter
year1 <- read_delim(here("~/Documents/El Salvador Research/project/data/raw/DGT_IMPUESTO1707167152598-122023-122023.csv"),
                    delim = ";")
year2 <- read_delim(here("~/Documents/El Salvador Research/project/data/raw/DGT_IMPUESTO1738685926586-122024-122024.csv"),
                    delim = ";")

# Combine data from both years
data_all <- bind_rows(year1, year2)

# Create a unique cell identifier by combining ATECO code and region
data_all <- data_all %>%
  mutate(cell_id = paste0(ACTECO, "_", MUNICIPIO))

# Summarize revenue by cell, year, and month
data_all <- data_all %>%
  group_by(cell_id, ANIO, MES) %>%
  summarise(revenue = sum(VALOR, na.rm = TRUE)) %>%
  ungroup()

# Add placeholder column for treatment status (to fill later)
data_all <- data_all %>%
  mutate(treatment = NA_integer_)

# Save final combined dataset to CSV
write_csv(data_all, here("combined_data.csv"))

# View first few rows to confirm
print(head(data_all))

# Calculate revenue growth
# Filter 2023 data
rev_2023 <- data_all %>%
  filter(ANIO == 2023) %>%
  select(cell_id, revenue) %>%
  rename(revenue_2023 = revenue)

# Average revenue for each cell across 2024
rev_2024 <- data_all %>%
  filter(ANIO == 2024) %>%
  group_by(cell_id) %>%
  summarise(revenue_2024 = mean(revenue, na.rm = TRUE)) %>%
  ungroup()

# Merge to calculate growth
revenue_growth <- left_join(rev_2023, rev_2024, by = "cell_id") %>%
  mutate(growth = (revenue_2024 - revenue_2023) / revenue_2023 * 100)

# Filter out cells with no revenue in 2024
revenue_growth_clean <- revenue_growth %>%
  filter(!is.na(revenue_2024))

## The Problem Child: 60% of cells in 2024 dataset have revenue in both 12.2023 and 2024

# View top few rows
print(head(revenue_growth_clean))

# Plot
library(ggplot2)
library(scales)

# Filter out extreme outliers for clearer plot
# Adjust the cutoffs (500, -100) as needed
revenue_growth_clean <- revenue_growth %>%
  filter(!is.na(growth)) %>%
  filter(growth < 500, growth > -100)

# Create histogram of growth rates
ggplot(revenue_growth_clean, aes(x = growth)) +
  geom_histogram(bins = 50, fill = "blue", alpha = 0.7) +
  scale_x_continuous(labels = label_comma()) +
  labs(title = "Revenue Growth from 2023 to 2024 (Filtered)",
       x = "Growth (%)",
       y = "Number of Cells") +
  theme_minimal()

## Found Revenue Growth, data centered at zero, skewed right


### ROBUSTNESS CHECK ###

# Mean revenue for balanced cells in 2024
mean_revenue_2024_balanced <- data_all %>%
  filter(ANIO == 2024, cell_id %in% revenue_growth_clean$cell_id) %>%
  summarise(mean_revenue = mean(revenue, na.rm = TRUE)) %>%
  pull(mean_revenue)

# Mean revenue for all cells in 2024
mean_revenue_2024_all <- data_all %>%
  filter(ANIO == 2024) %>%
  summarise(mean_revenue = mean(revenue, na.rm = TRUE)) %>%
  pull(mean_revenue)

# Create a comparison tibble
comparison_data <- tibble(
  Category = c("All Cells (2024)", "Balanced Cells (2024)"),
  Mean_Revenue_2024 = c(mean_revenue_2024_all, mean_revenue_2024_balanced)
)

# Plot mean revenue comparison
ggplot(comparison_data, aes(x = Category, y = Mean_Revenue_2024, fill = Category)) +
  geom_col(alpha = 0.7) +
  labs(title = "Mean Revenue in 2024: All Cells vs. Balanced Cells",
       y = "Mean Revenue",
       x = NULL) +
  theme_minimal() +
  theme(legend.position = "none")

## Found that filtered/balanced cells (excluding outliers) have lower mean revenue than all cells

# Statistical Test for Robustness Check
# Create numeric vectors for revenue in 2024
revenue_2024_all <- data_all %>%
  filter(ANIO == 2024) %>%
  pull(revenue)

revenue_2024_balanced <- data_all %>%
  filter(ANIO == 2024, cell_id %in% revenue_growth_clean$cell_id) %>%
  pull(revenue)

# Perform two-sample t-test
t_test_result <- t.test(revenue_2024_all, revenue_2024_balanced, alternative = "two.sided")
print(t_test_result)

## Found statistically significant difference in mean revenue between all cells and balanced cells in 2024


# Proportion of balanced cells to all cells in 2024
n_both_years <- sum(!is.na(revenue_growth$revenue_2024))

all_cells <- data_all %>%
  filter(ANIO == 2024) %>%
  distinct(cell_id) %>%
  nrow()

proportion_both_years <- n_both_years / all_cells
cat("Proportion of cells with data in both 2023 and 2024:", proportion_both_years, "\n")


### BETTER UNDERSTANDING DATASET ###

# Check number of unique cells in 2024
n_unique_cells_2024 <- data_all %>%
  filter(ANIO == 2024) %>%
  distinct(cell_id) %>%
  nrow()

cat("Number of unique cells in 2024:", n_unique_cells_2024, "\n")

# Check total number of rows (observations) in 2024
n_rows_2024 <- data_all %>%
  filter(ANIO == 2024) %>%
  nrow()

cat("Total number of rows (data points) in 2024:", n_rows_2024, "\n")

# Check number of observations per cell
obs_per_cell <- data_all %>%
  filter(ANIO == 2024) %>%
  count(cell_id) %>%
  arrange(desc(n))

cat("\nFirst few rows of observations per cell:\n")
print(head(obs_per_cell))

# Visualize the distribution of observations per cell
ggplot(obs_per_cell, aes(x = n)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  labs(title = "Distribution of Observations per Cell (2024)",
       x = "Number of Observations",
       y = "Number of Cells") +
  theme_minimal()

# Check sample cell's data to see actual rows
sample_cell_id <- sample(obs_per_cell$cell_id, 1)
cat("\nSample cell ID:", sample_cell_id, "\n")

sample_cell_data <- data_all %>%
  filter(cell_id == sample_cell_id)

cat("Data for the sample cell:\n")
print(sample_cell_data)


### EVENT-STUDY DIF IN DIF ###

library(tidyverse)
library(here)
library(did)
library(ggplot2)
library(zoo)

# Load 2022 data
year0 <- read_delim(
  here("~/Documents/El Salvador Research/project/data/raw/DGT_IMPUESTO1648495403637-012022-122022.csv"),
  delim = ";"
) %>%
  mutate(cell_id = paste0(ACTECO, "_", MUNICIPIO)) %>%
  group_by(cell_id, ANIO, MES) %>%
  summarise(revenue = sum(VALOR, na.rm = TRUE), .groups = "drop")

# Load 2023 data (full year data despite file name)
year1 <- read_delim(
  here("~/Documents/El Salvador Research/project/data/raw/DGT_IMPUESTO1707167152598-122023-122023.csv"),
  delim = ";"
) %>%
  mutate(cell_id = paste0(ACTECO, "_", MUNICIPIO)) %>%
  group_by(cell_id, ANIO, MES) %>%
  summarise(revenue = sum(VALOR, na.rm = TRUE), .groups = "drop")

# Load 2024 data
year2 <- read_delim(
  here("~/Documents/El Salvador Research/project/data/raw/DGT_IMPUESTO1738685926586-122024-122024.csv"),
  delim = ";"
) %>%
  mutate(cell_id = paste0(ACTECO, "_", MUNICIPIO)) %>%
  group_by(cell_id, ANIO, MES) %>%
  summarise(revenue = sum(VALOR, na.rm = TRUE), .groups = "drop")

# Combine all datasets
data_all <- bind_rows(year0, year1, year2)

# Create numeric year-month variable (YYYYMM)
data_all <- data_all %>%
  mutate(year_month = as.numeric(paste0(ANIO, sprintf("%02d", MES))))

# Classify taxpayer size
data_all <- data_all %>%
  mutate(
    taxpayer_size = case_when(
      revenue > 1e6 ~ "large",
      revenue > 1e5 ~ "medium",
      TRUE ~ "small"
    )
  )

# Assign treatment period based on rollout months
data_all <- data_all %>%
  mutate(
    treatment_period = case_when(
      taxpayer_size == "large" ~ 202307,
      taxpayer_size == "medium" ~ 202310,
      taxpayer_size == "small" ~ 202407
    )
  )

# Sort before creating numeric cell ID for consistency
data_all <- data_all %>%
  arrange(cell_id) %>%
  mutate(cell_id_num = as.numeric(factor(cell_id)))

# Create date columns for proper month differences
data_all <- data_all %>%
  mutate(
    date = as.Date(paste0(year_month, "01"), format = "%Y%m%d"),
    treatment_date = as.Date(paste0(treatment_period, "01"), format = "%Y%m%d")
  )

# Create consecutive numeric period index
unique_months <- sort(unique(data_all$year_month))
data_all <- data_all %>%
  mutate(
    period_num = match(year_month, unique_months),
    treatment_period_num = match(treatment_period, unique_months)
  )

# Clean dataset: remove rows with missing key fields
data_all_clean <- data_all %>%
  filter(!is.na(revenue), !is.na(period_num), !is.na(cell_id_num), !is.na(treatment_period_num))

cat("Number of observations in cleaned dataset:", nrow(data_all_clean), "\n")


# Log transformation of revenue for event study
data_all_clean <- data_all_clean %>%
  mutate(log_revenue = log1p(revenue))

att_gt_results <- att_gt(
  yname = "log_revenue",
  tname = "period_num",
  idname = "cell_id_num",
  gname = "treatment_period_num",
  data = data_all_clean,
  panel = TRUE,
  control_group = "notyettreated"
)

agg_event_study <- aggte(att_gt_results, type = "dynamic", na.rm = TRUE)

# Plot and test as before

# DiD Estimation
att_gt_results <- att_gt(
  yname = "revenue",
  tname = "period_num",
  idname = "cell_id_num",
  gname = "treatment_period_num",
  data = data_all_clean,
  panel = TRUE,
  control_group = "notyettreated"
)

# Event Study / Parallel Trends
agg_event_study <- aggte(att_gt_results, type = "dynamic", na.rm = TRUE)

event_study_df <- data.frame(
  time = agg_event_study$egt,
  att = agg_event_study$att.egt,
  se = agg_event_study$se.egt
)

# Plot event-study ATT estimates
ggplot(event_study_df, aes(x = time, y = att)) +
  geom_point() +
  geom_errorbar(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Corrected Event-study Plot: Parallel Trends Test",
    x = "Months since Treatment",
    y = "ATT"
  ) +
  theme_minimal()

# Formal F-test for pre-treatment months
pre_treatment_df <- subset(event_study_df, time < 0)
parallel_trends_test <- summary(lm(att ~ 0 + factor(time), data = pre_treatment_df))
print(parallel_trends_test)



### MORE PARALLEL TRENDS TESTING ###

# Create a group variable for plotting
data_all_clean <- data_all_clean %>%
  mutate(treated_group = ifelse(period_num >= treatment_period_num, "Treated", "Control"))

# Calculate average revenue per month by treatment status
avg_revenue_plot_data <- data_all_clean %>%
  group_by(year_month, treated_group) %>%
  summarise(avg_revenue = mean(revenue, na.rm = TRUE), .groups = "drop")

# Create true date variable for proper plotting
avg_revenue_plot_data <- avg_revenue_plot_data %>%
  mutate(
    year_month_date = as.Date(paste0(year_month, "01"), format = "%Y%m%d")
  )

# Plot using proper dates
ggplot(avg_revenue_plot_data, aes(x = year_month_date, y = avg_revenue, color = treated_group)) +
  geom_line() +
  labs(
    title = "Average Revenue over Time by Group (Proper Dates)",
    x = "Date",
    y = "Average Revenue"
  ) +
  theme_minimal()


### CHECKING DATA ###
# Create true date for each month
data_all <- data_all %>%
  mutate(
    year_month_date = as.Date(paste0(ANIO, sprintf("%02d", MES), "01"), format = "%Y%m%d")
  )

# Count sample sizes per month
sample_size_per_month <- data_all %>%
  group_by(year_month_date) %>%
  summarise(n_obs = n(), .groups = "drop")

print(sample_size_per_month)

# Plot sample size over time
ggplot(sample_size_per_month, aes(x = year_month_date, y = n_obs)) +
  geom_line(color = "darkgreen") +
  labs(
    title = "Number of Observations per Month",
    x = "Date",
    y = "Number of Observations"
  ) +
  theme_minimal()