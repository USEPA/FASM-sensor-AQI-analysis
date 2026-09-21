# Attempt to remove lock directory
lock_dir <- "C:/Program Files/R/R-4.4.2/library/00LOCK"
if (file.exists(lock_dir)) {
  unlink(lock_dir, recursive = TRUE)
}

# Function to install CRAN packages if not already installed
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
}

# Define CRAN packages
cran_packages <- c(
  "MazamaCoreUtils",
  "MazamaLocationUtils",
  "AirMonitor",
  "tidyverse",
  "ggpubr",
  "here"
)

# Install CRAN packages
install_if_missing(cran_packages)

# Define GitHub packages
github_packages <- list(
  "AirMonitorPlots" = "mazamascience/AirMonitorPlots",
  "AirSensor2" = "mazamascience/AirSensor2"
)

# Install GitHub packages using devtools
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
library(devtools)

for (pkg in names(github_packages)) {
  if (!require(pkg, character.only = TRUE)) {
    devtools::install_github(github_packages[[pkg]], build_vignettes = TRUE)
  }
}

# Load all libraries
library(MazamaCoreUtils)
library(MazamaLocationUtils)
library(AirMonitor)
library(AirMonitorPlots)
library(tidyverse)
library(AirSensor2)
library(ggpubr)
library(here)
library(DT)
