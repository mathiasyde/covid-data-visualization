library(tidyverse)
library(sf)

# Vaccination Data by ZIP code

vax_data_raw <- read.csv("datasets/COVID-19_Vaccinations_by_ZIP_Code_-_Historical.csv", stringsAsFactors = FALSE)

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

# Progression Data by ZIP code

prog_data_raw <- read.csv("datasets/COVID-19_Progression_by_ZIP_Code_-_Historical.csv", stringsAsFactors = FALSE)

prog_data <- prog_data_raw %>%
  mutate(
    Date = as.Date(Week.Start, format="%m/%d/%Y"),
    ZIP_Code = as.character(ZIP.Code)
  ) %>%
  select(
    Date,
    ZIP_Code,
    Cases_Weekly = Cases...Weekly,
    Cases_Cumulative = Cases...Cumulative,
    Deaths_Weekly = Deaths...Weekly,
    Deaths_Cumulative = Deaths...Cumulative,
    Tests_Weekly = Tests...Weekly,
    Tests_Cumulative = Tests...Cumulative,
    Case_Rate_Weekly = Case.Rate...Weekly,
    Test_Rate_Weekly = Test.Rate...Weekly,
    Percent_Tested_Positive_Weekly = Percent.Tested.Positive...Weekly
  ) %>%
  mutate(
    Cases_Weekly = as.numeric(Cases_Weekly),
    Cases_Cumulative = as.numeric(Cases_Cumulative),
    Deaths_Weekly = as.numeric(Deaths_Weekly),
    Deaths_Cumulative = as.numeric(Deaths_Cumulative),
    Tests_Weekly = as.numeric(Tests_Weekly),
    Tests_Cumulative = as.numeric(Tests_Cumulative),
    Case_Rate_Weekly = as.numeric(Case_Rate_Weekly),
    Test_Rate_Weekly = as.numeric(Test_Rate_Weekly),
    Percent_Tested_Positive_Weekly = as.numeric(Percent_Tested_Positive_Weekly)
  ) %>%
  filter(!is.na(ZIP_Code), !is.na(Date)) %>%
  arrange(Date, ZIP_Code)

# Merging the datasets

zip_data_combined <- prog_data %>%
  full_join(vax_data, by = c("Date", "ZIP_Code")) %>%
  arrange(Date, ZIP_Code) %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))

# Load and process the geographic boundaries
boundaries_raw <- read.csv("Boundaries_-_ZIP_Codes_20251123.csv", stringsAsFactors = FALSE)

chicago_boundaries <- st_as_sf(boundaries_raw, wkt = "the_geom", crs = 4326) %>%
  select(
    ZIP_Code = ZIP,
    OBJECTID,
    SHAPE_AREA,
    SHAPE_LEN
  ) %>%
  mutate(ZIP_Code = as.character(ZIP_Code))

# Create folder id it doesn't exist
if (!dir.exists("data")) {
  dir.create("data")
}

# create geographic folder is it doesn't exists
if (!dir.exists("geographic")) {
  dir.create("geographic")
}

#Save the combined ZIP code data as csv
write.csv(zip_data_combined, "data/chicago_zip_data.csv", row.names = FALSE)

# Save the geo boundaries as GeoJSON
st_write(chicago_boundaries, "geographic/chicago_zip_boundaries.geojson", delete_dsn = TRUE, quiet = TRUE)

