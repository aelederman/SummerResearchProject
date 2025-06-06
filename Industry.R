# Load libraries
library(tidyverse)
library(here)

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

# Load-and-clean function
load_and_clean <- function(file_path) {
  df <- read_delim(here(file_path), delim = ";", show_col_types = FALSE)
  names(df) <- gsub('"', '', names(df))
  df %>%
    mutate(
      ACTECO = as.character(ACTECO),
      sector_code = substr(ACTECO, 1, 1),
      sector_name = sector_map[sector_code],
      cell_id = paste0(ACTECO, "_", MUNICIPIO),
      taxpayer_size = case_when(
        VALOR > 1e6 ~ "large",
        VALOR > 1e5 ~ "medium",
        TRUE ~ "small"
      )
    ) %>%
    filter(!is.na(sector_name), !is.na(taxpayer_size))
}

# List your data files
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

# Load all files
all_data <- map_dfr(files, ~load_and_clean(file.path("~/Documents/El Salvador Research/project/data/raw", .x)))

# Count number of unique firms by sector and taxpayer size
sector_summary <- all_data %>%
  distinct(cell_id, sector_name, taxpayer_size) %>%
  count(sector_name, taxpayer_size, name = "firm_count")

# Compute total per sector for label placement
sector_totals <- sector_summary %>%
  group_by(sector_name) %>%
  summarise(total = sum(firm_count), .groups = "drop")

# Join totals for label plotting
sector_summary <- left_join(sector_summary, sector_totals, by = "sector_name")

# Plot with count labels
ggplot(sector_summary, aes(x = reorder(sector_name, -total), y = firm_count, fill = taxpayer_size)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    data = sector_totals,
    aes(x = reorder(sector_name, -total), y = total, label = total),
    vjust = -0.3,
    size = 3,
    inherit.aes = FALSE
  ) +
  labs(
    title = "Firm Count by Sector and Taxpayer Size",
    x = "Sector",
    y = "Number of Unique Firms",
    fill = "Taxpayer Size"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))