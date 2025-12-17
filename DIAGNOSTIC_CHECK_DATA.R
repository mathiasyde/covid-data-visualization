# DIAGNOSTIC: Check COVID Progression Data

library(tidyverse)

# Load the progression data
prog_data <- read.csv("data/chicago_zip_progression.csv", stringsAsFactors = FALSE)

cat("=== BASIC INFO ===\n")
cat("Total rows:", nrow(prog_data), "\n")
cat("Total ZIP codes:", length(unique(prog_data$ZIP_Code)), "\n")
cat("Date range:", min(as.Date(prog_data$Date)), "to", max(as.Date(prog_data$Date)), "\n\n")

cat("=== COLUMN NAMES ===\n")
print(names(prog_data))
cat("\n")

cat("=== CHECK FOR DATA ===\n")
cat("Cases_Weekly - Non-zero values:", sum(prog_data$Cases_Weekly > 0, na.rm = TRUE), "\n")
cat("Cases_Cumulative - Non-zero values:", sum(prog_data$Cases_Cumulative > 0, na.rm = TRUE), "\n")
cat("Deaths_Weekly - Non-zero values:", sum(prog_data$Deaths_Weekly > 0, na.rm = TRUE), "\n")
cat("Deaths_Cumulative - Non-zero values:", sum(prog_data$Deaths_Cumulative > 0, na.rm = TRUE), "\n\n")

cat("=== SAMPLE OF DATA (first 20 rows with non-zero cases) ===\n")
prog_data %>%
  filter(Cases_Weekly > 0 | Cases_Cumulative > 0) %>%
  select(Date, ZIP_Code, Cases_Weekly, Cases_Cumulative, Deaths_Weekly) %>%
  head(20) %>%
  print()

cat("\n=== CHECK SPECIFIC DATE (2021-03-01) ===\n")
prog_data %>%
  filter(Date == "2021-03-01" | Date == "2021-03-07" | Date == "2021-03-14") %>%
  select(Date, ZIP_Code, Cases_Weekly, Cases_Cumulative) %>%
  arrange(desc(Cases_Weekly)) %>%
  head(20) %>%
  print()

cat("\n=== CHECK WHAT ZIP CODES EXIST ===\n")
cat("Sample ZIP codes from data:\n")
print(head(unique(prog_data$ZIP_Code), 20))

# Load boundaries
boundaries <- sf::st_read("geographic/chicago_zip_boundaries.geojson", quiet = TRUE)
cat("\nSample ZIP codes from boundaries:\n")
print(head(unique(boundaries$ZIP_Code), 20))

# Check overlap
data_zips <- unique(as.character(prog_data$ZIP_Code))
boundary_zips <- unique(as.character(boundaries$ZIP_Code))

cat("\n=== ZIP CODE OVERLAP ===\n")
cat("ZIP codes in data:", length(data_zips), "\n")
cat("ZIP codes in boundaries:", length(boundary_zips), "\n")
cat("ZIP codes in BOTH:", length(intersect(data_zips, boundary_zips)), "\n")
cat("ZIP codes ONLY in data:", length(setdiff(data_zips, boundary_zips)), "\n")
cat("ZIP codes ONLY in boundaries:", length(setdiff(boundary_zips, data_zips)), "\n")

if (length(intersect(data_zips, boundary_zips)) == 0) {
  cat("\n⚠️ WARNING: NO MATCHING ZIP CODES!\n")
  cat("Sample from data:", head(data_zips, 5), "\n")
  cat("Sample from boundaries:", head(boundary_zips, 5), "\n")
}

