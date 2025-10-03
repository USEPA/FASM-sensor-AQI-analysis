## FASM-sensor-AQI-analysis/data

This folder is for sensor data. It already contains some example sensor data that is used to guide users through the tutorial and is where users should save their own sensor data when they are ready to complete the analysis. 

Example sensor data is contained in two csv files (see **Requirements for Sensor Data** in main README): the metadata and the time-series data. Contained in this folder are example sensor data from two sensor manufacturers already displayed on the Fire and Smoke Map. Thank you to PurpleAir (MTA #1261-19) and to Clarity Movement Co. (EPA MTA 1643-25, FS MTA 25-RD-11132543-061) for providing these data via the cited Material Transfer Agreements.

The file *'clarity_data.csv'* contains the example time-series data for Clarity sensors from May 30, 2023 through September 29, 2023. The first column is the *datetime*, and each following column is for a specific sensor *deviceDeploymentID* with hourly PM2.5 concentrations.

The file *'clarity_meta.csv'* contains the example metadata for Clarity sensors. Each column is a metadata variable, so each row contains metadata for a specific sensor *deviceDeploymentID*. Note that the order of the *deviceDeploymentID* down the first column of the metadata table matches the order of the *deviceDeploymentID* across the column headers of the time-series data after the *datetime* column.

The file *'purpleair_data.csv'* contains the example time-series data for PurpleAir sensors from August 28, 2024 through October 03, 2024. The first column is the *datetime*, and each following column is for a specific sensor *deviceDeploymentID* with hourly PM2.5 concentrations.

The file *'purpleair_meta.csv'* contains the example metadata for PurpleAir sensors. Each column is a metadata variable, so each row contains metadata for a specific sensor *deviceDeploymentID*. Note that the order of the *deviceDeploymentID* down the first column of the metadata table matches the order of the *deviceDeploymentID* across the column headers of the time-series data after the *datetime* column.
