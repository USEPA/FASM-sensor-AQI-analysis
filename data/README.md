## FASM-sensor-AQI-analysis/data

This folder is for sensor data. It already contains some example sensor data that is used to guide users through the tutorial and is where users should save their own sensor data when they are ready to complete the analysis. 

Example sensor data is contained in two csv files (see **Requirements for Sensor Data** in main README): the meta data and the timeseries data. Contained in this folder is example sensor data from two sensor manufacturers already displayed on the Fire and Smoke Map. Thank you to PurpleAir for providing these data via the Material Transfer Agreement (MTA #1261-19) and thank you to Clarity for providing these data via the MTA (EPA MTA 1643-25, FS MTA 25-RD-11132543-061).

The file *'clarity_data.csv'* contains the example time series data for Clarity sensors from May 30, 2023 through September 29, 2023. The first column is the *datetime*, and each following column is for a specific sensor *deviceDeploymentID* with hourly PM2.5 concentrations.

The file *'clarity_meta.csv'* contains the example meta data for Clarity sensors. Each column is a meta data variable, so each row contains meta data for a specific sensor *deviceDeploymentID*. Note that the order of the *deviceDeploymentID* down the first column of the meta data table matches the order of the *deviceDeploymentID* across the column headers of the time series data after the *datetime* column.
