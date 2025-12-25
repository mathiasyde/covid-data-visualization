library(tidyverse)
library(readr)
library(lubridate)



# Load raw Chicago data
chicago_raw <- read_csv("datasets/chicago.csv", show_col_types = FALSE) %>%
  mutate(week_end = mdy(`Week End`))

# Pre-compute VE calculations
ve_precomputed <- chicago_raw %>%
  filter(Outcome == "Cases",
         !is.na(`Unvaccinated Rate`), 
         `Unvaccinated Rate` > 0,
         !is.na(`Vaccinated Rate`)) %>%
  mutate(
    VE = (1 - (`Vaccinated Rate` / `Unvaccinated Rate`)) * 100,
    VE = pmax(pmin(VE, 100), -100),  # Clamp between -100 and 100
    Date = week_end,
    age_lower = parse_number(`Age Group`)
  ) %>%
  select(Date, `Age Group`, Outcome, VE, 
         `Vaccinated Rate`, `Unvaccinated Rate`, age_lower) %>%
  arrange(Date, age_lower)



# Pre-compute aggregations for stream plot (reduces points by half)
ve_stream_data <- ve_precomputed %>%
  filter(`Age Group` != "All") %>%
  group_by(`Age Group`) %>%
  arrange(Date, .by_group = TRUE) %>%
  mutate(idx = row_number()) %>%
  filter(idx %% 2 == 0) %>%  # Keep every other point
  ungroup() %>%
  select(Date, `Age Group`, VE, age_lower)



# Save pre-computed data
if (!dir.exists("data")) dir.create("data")

write_csv(ve_precomputed, "data/ve_precomputed.csv")

write_csv(ve_stream_data, "data/ve_stream_precomputed.csv")
