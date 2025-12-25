# COVID-19 Data Visualization Project

## Project Overview
Exploratory visualization of the COVID-19 pandemic in Chicago, focusing on vaccine effectiveness and outcomes across different populations.

## Setup Instructions

### 1. Clone the Repository
```r
# In RStudio: File → New Project → Version Control → Git
# Enter the repository URL
```

### 2. Install Required Packages
Run the following commands in R to install all dependencies:
```r
# Core Shiny packages
install.packages("shiny")
install.packages("bslib")
install.packages("shinycssloaders")
install.packages("DT")

# Data manipulation
install.packages("dplyr")
install.packages("tidyr")
install.packages("tidyverse")
install.packages("readr")
install.packages("lubridate")

# Visualization
install.packages("ggplot2")
install.packages("ggridges")
install.packages("plotly")
install.packages("viridis")
install.packages("scales")

# Geographic mapping
install.packages("sf")
install.packages("leaflet")
install.packages("rmapshaper")

# Special visualization (streamgraph)
install.packages("devtools")
devtools::install_github("davidsjoberg/ggstream")
```

### 3. Download Required Datasets
Download the following datasets and place them in the `datasets/` folder:

1. **Chicago COVID-19 Outcomes by Vaccination Status**
   - File: `chicago.csv`
   - Source: https://healthdata.gov/dataset/COVID-19-Outcomes-by-Vaccination-Status-Historical/fmz3-7y63/about_data

2. **Chicago COVID-19 Vaccinations by ZIP Code**
   - File: `COVID-19_Vaccinations_by_ZIP_Code_-_Historical.csv`
   - Source: https://data.cityofchicago.org/Health-Human-Services/COVID-19-Vaccinations-by-ZIP-Code-Historical/553k-3xzc/about_data

3. **Chicago COVID-19 Progression by ZIP Code**
   - File: `COVID-19_Progression_by_ZIP_Code_-_Historical.csv`
   - Source: https://data.cityofchicago.org/Health-Human-Services/COVID-19-Progression-by-ZIP-Code-Historical/vrgd-sgft

4. **Chicago ZIP Code Boundaries**
   - File: `Boundaries_-_ZIP_Codes_20251123.csv`
   - Source: https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-ZIP-Codes/gdcf-axmw

### 4. Prepare the Data (CRITICAL STEP!)
Before running the app, you **must** generate the processed data files. Run these scripts in order:
```r
# Step 1: Prepare main outcomes and population data
source("prepare_data.r")

# Step 2: Prepare vaccine effectiveness calculations
source("prepare_ve_data.R")

# Step 3: Prepare ZIP code geographic data
source("prepare_zip_data.R")

# Step 4: Optimize boundary files for faster loading
source("optimize_boundaries.R")
```

**What these scripts do:**
- `prepare_data.r` → Creates `data/chicago_outcomes.csv` and `data/chicago_population.csv`
- `prepare_ve_data.R` → Creates `data/ve_precomputed.csv` and `data/ve_stream_precomputed.csv`
- `prepare_zip_data.R` → Creates `data/chicago_zip_vaccination.csv`, `data/chicago_zip_progression.csv`, and `geographic/chicago_zip_boundaries.geojson`
- `optimize_boundaries.R` → Simplifies geographic boundaries for better performance

### 5. Run the Dashboard
```r
# Open app_optimized.R in RStudio
# Click the "Run App" button in the top-right corner
# Or run: shiny::runApp("app_optimized.R")
```
## Dashboard Chapters

The dashboard tells the COVID-19 story through 5 interactive chapters:

1. **The Beginning** - Overview of COVID-19 outcomes before and after vaccines
2. **The Turning Point** - Vaccine rollout timeline and population shift
3. **The Evidence** - Vaccine effectiveness analysis across age groups
4. **The Disparities** - Geographic patterns across Chicago ZIP codes
5. **Explore Data** - Browse raw datasets with searchable tables

## Troubleshooting

### "Cannot open file 'data/chicago_outcomes.csv'"
**Solution:** Run all four preparation scripts (see step 4 above) to generate the processed data files.

### "Cannot find datasets/chicago.csv"
**Solution:** Download the raw datasets from the links in step 3 and place them in the `datasets/` folder.

### "Package 'ggstream' not found"
**Solution:** Install from GitHub using:
```r
devtools::install_github("davidsjoberg/ggstream")
```

### "ZIP code boundaries not loading"
**Solution:** 
1. Make sure you've run `prepare_zip_data.R`
2. Then run `optimize_boundaries.R` to simplify the geographic data
3. Check that `geographic/chicago_zip_boundaries.geojson` exists

### App is slow or unresponsive
**Solution:** 
1. Ensure you've run `prepare_ve_data.R` to create pre-computed vaccine effectiveness data
2. Run `optimize_boundaries.R` to simplify geographic boundaries
3. Close other R sessions or applications

### "Error in mutate()" or data processing errors
**Solution:** Ensure you're using the latest versions of `dplyr` and `tidyverse`:
```r
update.packages(c("dplyr", "tidyverse"))
```

## Data Sources

All data comes from official public health sources:

- **CDC COVID-19 Outcomes by Vaccination Status**  
  https://healthdata.gov/dataset/COVID-19-Outcomes-by-Vaccination-Status-Historical/fmz3-7y63/about_data

- **Chicago COVID-19 Data Portal**  
  https://data.cityofchicago.org/

- **Chicago ZIP Code Boundaries**  
  https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-ZIP-Codes/gdcf-axmw

## Contributing

This project was created as part of a data visualization course. Team members:
- **Mathias** 
- **Michelle**  
- **Jonas**