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
```r
install.packages("shiny")
install.packages("ggplot2")
install.packages("ggridges")
install.packages("dplyr")
install.packages("tidyr")
install.packages("bslib")
install.packages("tidyverse")
install.packages("sf")
install.packages("leaflet")
```

### 3. Prepare the Data (IMPORTANT!)
Before running the app, you must generate the data files:

1. Make sure you have the raw data file in `datasets/chicago.csv`
2. Open `prepare_data.r` in RStudio
3. Click the **Source** button (or run the entire script)
4. This will create a `data/` folder with processed CSV files

### 4. Run the App
```r
# Open app.r
# Click "Run App" button
```

## Datasets - Chicago:

https://healthdata.gov/dataset/COVID-19-Outcomes-by-Vaccination-Status-Historical/fmz3-7y63/about_data

https://data.cityofchicago.org/Health-Human-Services/COVID-19-Daily-Cases-Deaths-and-Hospitalizations-H/naz8-j4nc/about_data

https://data.cityofchicago.org/Health-Human-Services/COVID-19-Daily-Vaccinations-Chicago-Residents-Cumu/rna5-2pgy

https://data.cityofchicago.org/Health-Human-Services/COVID-19-Progression-by-ZIP-Code-Historical/vrgd-sgft

https://data.cityofchicago.org/Health-Human-Services/COVID-19-Vaccinations-by-ZIP-Code-Historical/553k-3xzc/about_data

https://data.cityofchicago.org/Health-Human-Services/COVID-19-Cases-Tests-and-Deaths-by-ZIP-Code-Histor/yhhz-zm2v/about_data

## Troubleshooting

### "Cannot open file 'data/chicago_outcomes.csv'"
→ Run `prepare_data.r` first to generate the data files

### "Cannot find datasets/chicago.csv"
→ Download the raw data from the links above and place it in the `datasets/` folder

### App won't run
→ Make sure all packages are installed (see step 2 above)
