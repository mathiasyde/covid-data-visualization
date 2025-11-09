library(tidyverse)
library(dplyr)

Chicago <- list()

Chicago$outcomes <- read.csv("datasets/chicago.csv") |>
  select(Date = Week.End, Outcome, Age.Group, Outcome.Unvaccinated, Outcome.Vaccinated, Outcome.Boosted) |>
  # Sort by date
  arrange(as.Date(Date, format="%m/%d/%Y"), Outcome, Age.Group) |>
  # Remove "All" age group and sort logically
  filter(Age.Group != "All") |>
  # Replace NA with 0
  mutate(Outcome.Vaccinated = ifelse(is.na(Outcome.Vaccinated), 0, Outcome.Vaccinated)) |>
  mutate(Outcome.Unvaccinated = ifelse(is.na(Outcome.Unvaccinated), 0, Outcome.Unvaccinated)) |>
  mutate(Outcome.Boosted = ifelse(is.na(Outcome.Boosted), 0, Outcome.Boosted)) |>
  # Format to "YYYY-MM-DD"
  mutate(Date = as.Date(Date, format="%m/%d/%Y")) |>
  mutate(Date = format(Date, "%Y-%m-%d"))

Chicago$population <- read.csv("datasets/chicago.csv") |>
  select(Date = Week.End, Outcome, Age.Group, Population.Unvaccinated, Population.Vaccinated, Population.Boosted) |>
  mutate(Date = as.Date(Date, format="%m/%d/%Y")) |>
  # Keep only "All" age group. The number is repeated for Cases, Hospitalizations, Deaths, so we can just pick one.
  filter(Age.Group == "All", Outcome == "Cases") |>
  
  mutate(Population.Vaccinated = ifelse(is.na(Population.Vaccinated), 0, Population.Vaccinated)) |>
  mutate(Population.Unvaccinated = ifelse(is.na(Population.Unvaccinated), 0, Population.Unvaccinated)) |>
  mutate(Population.Boosted = ifelse(is.na(Population.Boosted), 0, Population.Boosted)) |>
  # Remove the Outcome and Age.Group columns
  select(Date, Population.Vaccinated, Population.Unvaccinated, Population.Boosted) |>
  arrange(Date)

View(Chicago$population)

# CREATE THE DATA FOLDER IF IT DOESN'T EXIST
if (!dir.exists("data")) {
  dir.create("data")
}

# NOW WRITE THE CSV FILES
write.csv(Chicago$outcomes, "data/chicago_outcomes.csv", row.names = FALSE)
write.csv(Chicago$population, "data/chicago_population.csv", row.names = FALSE)

# Print confirmation message
cat("Data files created successfully!\n")
cat("- data/chicago_outcomes.csv\n")
cat("- data/chicago_population.csv\n")