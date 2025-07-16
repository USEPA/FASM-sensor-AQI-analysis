# The following script is a template for the comparison analysis between air
# quality sensors and AirNow monitors. Additional instructions are in the Word 
# file and source functions are from 'functions_for_AQI_barplots.R'. Areas where
# user input is required are noted below. 

# ----- session set up ---------------------------------------------------------

# Load necessary packages

if(!require("MazamaCoreUtils"))
  install.packages("MazamaCoreUtils")
if(!require("dplyr"))
  install.packages("dplyr")
if(!require("AirMonitor"))
  install.packages("AirMonitor")
if(!require("AirMonitorPlots"))
  devtools::install_github('mazamascience/AirMonitorPlots', build_vignettes=TRUE)
if(!require("tidyverse"))
  install.packages("tidyverse")
if(!require("AirSensor2"))
  devtools::install_github('mazamascience/AirSensor2')
if(!require("ggpubr"))
  install.packages("ggpubr")

library(MazamaCoreUtils)
library(dplyr)
library(AirMonitor)
library(AirMonitorPlots)
library(tidyverse)
library(AirSensor2)
library(ggpubr)

# Load utility functions from 'functions_for_AQI_barplots.R'
source()   # input file path to 'functions_for_AQI_barplots.R'

# ----- data set up ------------------------------------------------------------

# Load in sensor data 
meta_df <- # input file path to meta data (recommend using 'read_csv' rather than 'read.csv')
  
data_df <- # input file path to time series data (recommend using 'read_csv' rather than 'read.csv')
  
# Create mts_monitor object 
sensor <- list(meta = meta_df, 
               data = data_df)

sensor <- structure(sensor, 
                    class = c("mts_monitor", "mts", class(sensor)))

# Load in AirNow data 
airnow <- monitor_load(
  startdate = ,            # input start and end date to match sensor data 
  enddate =                # format example: startdate = 20210601
)

# ----- find adjacent pairs ----------------------------------------------------

# returns mts_monitor object of sensors
sensor_adjacent <- findAdjacentPairs(sensor = sensor, 
                                     airnow = airnow, 
                                     1000)    # specify the radius in meters

# call out the ids of the adjacent pairs
airnow_ids <- unique(sensor_adjacent$meta$airnow_id)
sensor_ids <- unique(sensor_adjacent$meta$deviceDeploymentID)

# filter the airnow data set for adjacent pairs 
airnow_adjacent <-
  airnow %>%
  monitor_select(airnow_ids)

# apply the NowCast algorithm 
sensor_adjacent <- 
  sensor_adjacent %>%
  monitor_nowcast(includeShortTerm = TRUE)

airnow_adjacent <-
  airnow_adjacent %>%
  monitor_nowcast(includeShortTerm = TRUE)

# if needed, manually trim (remove) rows in the sensor and AirNow time series 
# data tables for hourly start and end time stamps to match 

# ----- create unlisted AQI category data frame --------------------------------

AQI_unlisted <- create_AQI_unlisted(sensor_adjacent = sensor_adjacent,
                                    airnow_adjacent = airnow_adjacent)

# ----- create combined bar plot -----------------------------------------------

create_combined_barplot(AQI_unlisted = AQI_unlisted,
                        threshold_1 = 5,    # input the threshold for plus/minus one AQI category
                        threshold_2 = 1)    # input the threshold for plus/minus two or more AQI categories

# ----- create AQI categories bar plot grid ------------------------------------

# check the max AQI category reported by AirNow to determine use of 
# create_categories_barplot_'whatever the max AQI category is'

# for example, if the max AQI category was unhealthy
create_categories_barplot_unhealthy(AQI_unlisted = AQI_unlisted,
                                    threshold_1 = 30,     # input the threshold for plus/minus one AQI category
                                    threshold_2 = 1)     # input the threshold for plus/minus two or more AQI categories