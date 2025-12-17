library(tidyverse)
library(sf)

cat("=== Starting ZIP Code Data Preparation ===\n\n")

# ============================================
# PART 1: Vaccination Data
# ============================================
cat("Processing vaccination data...\n")

vax_data_raw <- read.csv("datasets/COVID-19_Vaccinations_by_ZIP_Code_-_Historical.csv", 
                         stringsAsFactors = FALSE)

vax_data <- vax_data_raw %>%
  mutate(
    Date = as.Date(Date, format="%m/%d/%Y"),
    ZIP_Code = as.character(Zip.Code)
  ) %>%
  select(
    Date,
    ZIP_Code,
    Population,
    Vaccinated_1st_Dose = X1st.Dose...Percent.Population,
    Fully_Vaccinated = Vaccine.Series.Completed....Percent.Population,
    Boosted = Bivalent...Percent.Population,
    Total_Doses = Total.Doses...Cumulative
  ) %>%
  mutate(
    Vaccinated_1st_Dose = as.numeric(gsub("%", "", Vaccinated_1st_Dose)),
    Fully_Vaccinated = as.numeric(gsub("%", "", Fully_Vaccinated)),
    Boosted = as.numeric(gsub("%", "", Boosted)),
    Total_Doses = as.numeric(Total_Doses),
    Population = as.numeric(Population)
  ) %>%
  filter(!is.na(ZIP_Code), !is.na(Date)) %>%
  arrange(Date, ZIP_Code)

cat("✓ Vaccination data:", nrow(vax_data), "rows,", length(unique(vax_data$ZIP_Code)), "ZIP codes\n\n")

# ============================================
# PART 2: COVID Progression Data
# ============================================
cat("Processing COVID progression data...\n")

prog_data_raw <- read.csv("datasets/COVID-19_Progression_by_ZIP_Code_-_Historical.csv", 
                          stringsAsFactors = FALSE)

# Show what we got
cat("  Columns found:", paste(head(names(prog_data_raw), 15), collapse=", "), "\n")

# The key: Use the actual column names from the file
# Common variations: Week.Start or Week.Start, ZIP.Code or Zip.Code
prog_data <- prog_data_raw %>%
  rename(
    # Handle different possible date column names
    Date_raw = any_of(c("Week.Start", "Week Start", "week.start")),
    # Handle different possible ZIP column names  
    ZIP_Code_raw = any_of(c("ZIP.Code", "ZIP Code", "Zip.Code", "Zip Code"))
  ) %>%
  mutate(
    Date = as.Date(Date_raw, format="%m/%d/%Y"),
    ZIP_Code = as.character(ZIP_Code_raw)
  ) %>%
  select(
    Date,
    ZIP_Code,
    # Use any_of to handle column name variations
    Cases_Weekly = any_of(c("Cases...Weekly", "Cases - Weekly", "Cases..Weekly")),
    Cases_Cumulative = any_of(c("Cases...Cumulative", "Cases - Cumulative", "Cases..Cumulative")),
    Deaths_Weekly = any_of(c("Deaths...Weekly", "Deaths - Weekly", "Deaths..Weekly")),
    Deaths_Cumulative = any_of(c("Deaths...Cumulative", "Deaths - Cumulative", "Deaths..Cumulative")),
    Tests_Weekly = any_of(c("Tests...Weekly", "Tests - Weekly", "Tests..Weekly")),
    Tests_Cumulative = any_of(c("Tests...Cumulative", "Tests - Cumulative", "Tests..Cumulative")),
    Case_Rate_Weekly = any_of(c("Case.Rate...Weekly", "Case Rate - Weekly", "Case.Rate..Weekly")),
    Test_Rate_Weekly = any_of(c("Test.Rate...Weekly", "Test Rate - Weekly", "Test.Rate..Weekly")),
    Percent_Tested_Positive_Weekly = any_of(c("Percent.Tested.Positive...Weekly", 
                                              "Percent Tested Positive - Weekly",
                                              "Percent.Tested.Positive..Weekly"))
  ) %>%
  mutate(across(where(is.character) & !ZIP_Code, as.numeric)) %>%
  mutate(
    Cases_Weekly = replace_na(Cases_Weekly, 0),
    Cases_Cumulative = replace_na(Cases_Cumulative, 0),
    Deaths_Weekly = replace_na(Deaths_Weekly, 0),
    Deaths_Cumulative = replace_na(Deaths_Cumulative, 0),
    Tests_Weekly = replace_na(Tests_Weekly, 0),
    Tests_Cumulative = replace_na(Tests_Cumulative, 0)
  ) %>%
  filter(!is.na(ZIP_Code), !is.na(Date)) %>%
  arrange(Date, ZIP_Code)

cat("✓ COVID progression data:", nrow(prog_data), "rows,", 
    length(unique(prog_data$ZIP_Code)), "ZIP codes\n")

# Show sample
cat("\nSample of processed data:\n")
prog_data %>%
  filter(Cases_Weekly > 0) %>%
  select(Date, ZIP_Code, Cases_Weekly, Cases_Cumulative) %>%
  head(5) %>%
  print()

if (length(unique(prog_data$ZIP_Code)) < 10) {
  cat("\n⚠️ WARNING: Only", length(unique(prog_data$ZIP_Code)), "ZIP codes found!\n")
  cat("Expected around 59 ZIP codes.\n")
  cat("Check that the file was downloaded correctly.\n\n")
}

cat("\n")

# ============================================
# PART 3: Geographic Boundaries
# ============================================
cat("Loading geographic boundaries...\n")

boundaries_raw <- read.csv("Boundaries_-_ZIP_Codes_20251123.csv", 
                           stringsAsFactors = FALSE)

chicago_boundaries <- st_as_sf(boundaries_raw, wkt = "the_geom", crs = 4326) %>%
  select(
    ZIP_Code = ZIP,
    OBJECTID,
    SHAPE_AREA,
    SHAPE_LEN
  ) %>%
  mutate(ZIP_Code = as.character(ZIP_Code))

cat("✓ Boundaries loaded for", nrow(chicago_boundaries), "ZIP codes\n\n")

# ============================================
# PART 4: Save Files
# ============================================
cat("Saving data files...\n")

if (!dir.exists("data")) dir.create("data")
if (!dir.exists("geographic")) dir.create("geographic")

write.csv(vax_data, "data/chicago_zip_vaccination.csv", row.names = FALSE)
cat("✓ Saved: data/chicago_zip_vaccination.csv\n")

write.csv(prog_data, "data/chicago_zip_progression.csv", row.names = FALSE)
cat("✓ Saved: data/chicago_zip_progression.csv\n")

st_write(chicago_boundaries, "geographic/chicago_zip_boundaries.geojson", 
         delete_dsn = TRUE, quiet = TRUE)
cat("✓ Saved: geographic/chicago_zip_boundaries.geojson\n")

cat("\n=== SUMMARY ===\n")
cat("Vaccination: ", nrow(vax_data), "rows,", length(unique(vax_data$ZIP_Code)), "ZIP codes\n")
cat("Progression: ", nrow(prog_data), "rows,", length(unique(prog_data$ZIP_Code)), "ZIP codes\n")
cat("Boundaries:  ", nrow(chicago_boundaries), "ZIP codes\n\n")

cat("✓ All done! You can now run your Shiny app.\n")