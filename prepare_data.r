library(tidyverse)
library(dplyr)

chicago <- read.csv("datasets/covid_cases_by_vaccination_status_over_time/chicago.csv") |>
    select(Date = Week.End, Outcome, Age.Group, Population.Unvaccinated, Population.Vaccinated, Outcome.Unvaccinated, Outcome.Vaccinated) |>
    # Sort by date
    arrange(as.Date(Date, format="%m/%d/%Y"), Outcome, Age.Group) |>
    # Remove "All" age group and sort logically
    filter(Age.Group != "All") |>
    # Replace NA with 0 for Outcome group
    mutate(Outcome.Vaccinated = ifelse(is.na(Outcome.Vaccinated), 0, Outcome.Vaccinated)) |>
    mutate(Outcome.Unvaccinated = ifelse(is.na(Outcome.Unvaccinated), 0, Outcome.Unvaccinated)) |>
    # Format to "YYYY-MM-DD"
    mutate(Date = as.Date(Date, format="%m/%d/%Y")) |>
    mutate(Date = format(Date, "%Y-%m-%d"))

write.csv(chicago, "data/historical_covid_cases_by_vaccination_status_chicago.csv", row.names = FALSE)
View(chicago)
