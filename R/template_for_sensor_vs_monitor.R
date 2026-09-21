# The following script is a template for the comparison analysis between air
# quality sensors and AirNow monitors. A tutorial is provided in the GitHub 
# pages site https://urban-bassoon-v9zkyqw.pages.github.io/ which follows the 
# Rmd and html in the docs folder of the FASM-sensor-AQI-analysis repository.

# User input is only required in the sensor data prep section where file names
# must be input.

# ----- session set up ---------------------------------------------------------

# Ensure 'here' package is installed
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
library(here)

# Source requirements to load necessary packages and configurations
source(here("R/package_requirements.R"))  
# Ensure the working directory or the project root is set to FASM-sensor-AQI-analysis

# Load utility functions from 'functions_for_AQI_barplots.R'
source(here("R/functions_for_AQI_barplots.R"))  
# Ensure the working directory or the project root is set to FASM-sensor-AQI-analysis

# ----- sensor data prep -------------------------------------------------------

# Ensure your metadata and time-series data files are saved in the data 
# folder of the repository (where some examples are already located)

# Load in sensor data (recommend using 'read_csv' rather than 'read.csv')
meta_df <- read_csv(here("data/YOUR_METADATA.csv")) 
# replace YOUR_METADATA with the metadata file name in data folder 

# Ensure metadata columns are the correct class 
meta_df$deviceDeploymentID <- as.character(meta_df$deviceDeploymentID)
meta_df$deviceID <- as.character(meta_df$deviceID)
meta_df$locationID <- as.character(meta_df$locationID)
meta_df$locationName <- as.character(meta_df$locationName)
meta_df$longitude <- as.numeric(meta_df$longitude)
meta_df$latitude <- as.numeric(meta_df$latitude)
meta_df$elevation <- as.numeric(meta_df$elevation)
meta_df$countryCode <- as.character(meta_df$countryCode)
meta_df$stateCode <- as.character(meta_df$stateCode)
meta_df$countyName <- as.character(meta_df$countyName)
meta_df$timezone <- as.character(meta_df$timezone)

data_df <- read_csv(here("data/YOUR_DATA.csv")) 
# replace YOUR_DATA with time-series data file name in data folder 

# Create mts_monitor object 
sensor <- list(meta = meta_df, 
               data = data_df)

sensor <- structure(sensor, 
                    class = c("mts_monitor", "mts", class(sensor)))

# ----- reference AirNow data --------------------------------------------------

# Extract the start and end date from the sensor data
startdate <- format(as.Date(min(sensor$data$datetime)), "%Y%m%d")
enddate <- format(as.Date(max(sensor$data$datetime)), "%Y%m%d")

# Load in AirNow data for the specified date range
airnow <- monitor_load(
  startdate = startdate,  # input start and end date to match sensor data 
  enddate = enddate       # format example if input manually: startdate = 20210601
)

# Find a common start and end date and time stamp between the AirNow and sensor data
aligned_startdate <- max(min(airnow$data$datetime), min(sensor$data$datetime))
aligned_enddate <- min(max(airnow$data$datetime), max(sensor$data$datetime))

# Filter the data to the overlapping datetime stamps
airnow$data <- airnow$data[airnow$data$datetime >= aligned_startdate & airnow$data$datetime <= aligned_enddate, ]
sensor$data <- sensor$data[sensor$data$datetime >= aligned_startdate & sensor$data$datetime <= aligned_enddate, ]

# ----- find adjacent pairs ----------------------------------------------------

# Returns mts_monitor object of sensors
sensor_adjacent <- findAdjacentPairs(sensor = sensor, 
                                     airnow = airnow, 
                                     1000)    # specify the radius in meters

# Call out the ids of the adjacent pairs
airnow_ids <- unique(sensor_adjacent$meta$airnow_id)
sensor_ids <- unique(sensor_adjacent$meta$deviceDeploymentID)

# Filter the airnow data set for adjacent pairs 
airnow_adjacent <-
  airnow %>%
  monitor_select(airnow_ids)

# Apply the NowCast algorithm 
sensor_adjacent <- 
  sensor_adjacent %>%
  monitor_nowcast(includeShortTerm = TRUE)

airnow_adjacent <-
  airnow_adjacent %>%
  monitor_nowcast(includeShortTerm = TRUE)

# ----- map adjacent pairs -----------------------------------------------------

# This starts by creating a leaflet map of just the airnow monitors
map <- MazamaLocationUtils::table_leaflet(
  airnow_adjacent$meta,
  jitter = 0
)

# Then add the nearby sensors to the map object 
MazamaLocationUtils::table_leafletAdd(
  map,
  sensor_adjacent$meta,
  jitter = 0,
  fillColor = "salmon",
  color = "black"
) %>%
  
  # Then add a title which include a count of monitors and their nearby sensor pairs
  leaflet::addControl(
    html = paste0("<h3>Mapped locations of ", nrow(airnow_adjacent$meta), " AirNow monitors and ", nrow(sensor_adjacent$meta), " nearby sensors", "</h3>"),
    position = "topright")

# ----- create unlisted AQI category data frame --------------------------------

AQI_unlisted <- create_AQI_unlisted(sensor_adjacent = sensor_adjacent,
                                    airnow_adjacent = airnow_adjacent)

# ----- create combined bar plot -----------------------------------------------

create_combined_barplot(AQI_unlisted = AQI_unlisted,
                        threshold_1 = 5,    # input the threshold for plus/minus one AQI category (default is 5)
                        threshold_2 = 1)    # input the threshold for plus/minus two or more AQI categories (default is 1)

# ----- create AQI categories bar plot grid ------------------------------------

# check the max AQI category reported by AirNow to determine use of 
# create_categories_barplot_'whatever the max AQI category is'

create_plots_by_category(AQI_unlisted = AQI_unlisted,
                         num_plots = max(AQI_unlisted$airnow), # determine the maximum AQI category reported by AirNow
                         threshold_1 = 30, # input the threshold for plus/minus one AQI category (default is 30)
                         threshold_2 = 1)  # input the threshold for plus/minus two or more AQI categories (default is 1)

# ----- create table of AQI category hours -------------------------------------

tab <- table(
  "Sensor" = AQI_unlisted$sensor, # makes the sensor AQI categories the rows
  "Monitor" = AQI_unlisted$airnow # makes the monitor AQI categories the columns
)

addmargins(tab) # includes sums of all rows and columns 