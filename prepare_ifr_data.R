library(tidyverse)
library(lubridate)

cat("=== Preparing IFR (Infection Fatality Rate) Data ===\n\n")

# Load raw Chicago data
cat("Loading raw data...\n")
chicago_raw <- read_csv("datasets/chicago.csv", show_col_types = FALSE)

# Pre-compute IFR data
cat("Computing IFR metrics...\n")
chicago_ifr <- chicago_raw %>%
  mutate(
    week_end = mdy(`Week End`),
    `Outcome Unvaccinated` = coalesce(`Outcome Unvaccinated`, 0),
    `Outcome Vaccinated`   = coalesce(`Outcome Vaccinated`, 0),
    `Outcome Boosted`      = coalesce(`Outcome Boosted`, 0),
    outcome_total = `Outcome Unvaccinated` + `Outcome Vaccinated` + `Outcome Boosted`,
    age_min = as.numeric(`Age Group Min`),
    age_max = as.numeric(`Age Group Max`),
    age_mid = (age_min + age_max) / 2
  ) %>%
  select(Outcome, Date = week_end, `Age Group`, age_mid, outcome_total) %>%
  filter(Outcome %in% c("Cases", "Deaths")) %>%
  group_by(Date, `Age Group`, age_mid, Outcome) %>%
  summarise(n = sum(outcome_total), .groups = "drop") %>%
  pivot_wider(names_from = Outcome, values_from = n, values_fill = 0) %>%
  mutate(
    # Set minimum cases threshold to 25 (same as AI default)
    IFR = ifelse(Cases >= 25 & Cases > 0, 100 * Deaths / Cases, NA_real_),
    IFR_pos = ifelse(IFR > 0, IFR, NA_real_)
  ) %>%
  arrange(Date)

# Check the data
cat("\nData summary:\n")
cat("  - Date range:", format(min(chicago_ifr$Date), "%Y-%m-%d"), "to", 
    format(max(chicago_ifr$Date), "%Y-%m-%d"), "\n")
cat("  - Age groups:", paste(unique(chicago_ifr$`Age Group`), collapse = ", "), "\n")
cat("  - Total rows:", nrow(chicago_ifr), "\n")

# Create data folder if needed
if (!dir.exists("data")) {
  dir.create("data")
}

# Save pre-computed data
cat("\nSaving pre-computed IFR data...\n")
write.csv(chicago_ifr, "data/chicago_ifr.csv", row.names = FALSE)

cat("\n✓ IFR data preparation complete!\n")
cat("  Output: data/chicago_ifr.csv\n")