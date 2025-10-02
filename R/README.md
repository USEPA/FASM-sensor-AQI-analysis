## FASM-sensor-AQI-analysis/R

This folder contains .R files used to complete the AQI category agreement analysis. One file is a template that is edited by the user to complete the anlysis, and two files are sources of supporting functions and code that are loaded into the editable file. These two supporting files do not need to be edited by the user.

The *'template_for_sensor_vs_monitor.R'* file is the fillable template that users walk through to complete the AQI category agreement analysis. 
There are a few designated spots where user input is required to load in their sensor data for analysis as well as optional areas where users can edit some customizable 
variables. Otherwise, this template lays out code where the user can simply "press play" to generate the bar plots visualizing AQI category agreement between their sensors 
and nearby AirNow monitors.

The *'package_requirements.R'* file is a supporting file that contains code that ensures the necessary packages are installed and loaded into the user's working environment. It gets loaded into and run 
in the template script as a source file as the initial step to set up the working environment.

The *'functions_for_AQI_barplots.R'* file is a supporting file that contains four functions that were created to help with this AQI category agreement analysis. These functions are loaded into the 
working environment as a source file through an early step in the template script. These functions help find nearby sensor/monitor pairs, create a dataframe of AQI categories,
and visualize AQI category agreement between sensors and monitors as bar plots.
