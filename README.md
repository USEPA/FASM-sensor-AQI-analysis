# FASM-sensor-AQI-analysis
## Background and Overview

This repository contains code to accompany the AirNow Fire and Smoke Map (FASM) Sensor Network Data Acceptance Protocol (link), a process that the U.S. Environmental Protection Agency (EPA) and the U.S. Forest Service (USFS) have tested for adding information from different sensor manufacturers onto the FASM. This code is intended to help users generate a particular set of bar plots to anayze and visualize AQI category agreement between their sensors and AirNow reference monitors within a specified radius.
## Getting Started

Follow these steps to set up a copy of this project on your local machine and format your sensor data for use with this repository's code:
### Prerequisites

Before you begin, ensure you have the following installed:

* **R**
* **RStudio**
* **Git**
### Cloning the Repository to RStudio

1.  **On GitHub:**
    *   Navigate to this repository's main page.
    *   Click the green **Code** button.
    *   Copy the HTTPS URL of the repository.
2.  **In RStudio:**
    *   Open RStudio.
    *   Go to **File > New Project**.
    *   Select **Version Control**.
    *   Choose **Git**.
    *   In the "Repository URL" field, paste the URL you copied from GitHub.
    *   RStudio will automatically suggest a "Project directory name". You can accept this or change it if needed.
    *   Choose a location on your computer to save the project by clicking **Browse**.
    *   Click **Create Project**.

Once the repository is cloned, you can follow the example laid out in (link). Then add your own sensor data to create your own AQI category agreement barplots (see sensor data considerations requirements). Should this example and these functions not work exactly for you, feel free to take the code as inspiration for generating these plots in a way that works best for you. 
### Requirements for Sensor Data

The sensor data must consist of two files (.csv or .xlsx format), one of meta data and one of time series data that will be combined into a *mts_monitor* object for analysis. To read more about the *mts_monitor* data format, see https://mazamascience.github.io/MazamaTimeSeries/ . 
#### Sensor Meta Data

The meta data file must have each row as a unique device and columns of device meta data.
* Required columns must include (written and titled **exactly** as below):
   *  *deviceDeploymentID*
   *  *deviceID*
   *  *locationID*
   *  *locationName*
   *  *longitude*
   *  *latitude*
   *  *elevation*
   *  *countryCode*
   *  *stateCode*
   *  *countyName*
   *  *timezone*
* All columns are of class character except *longitude*, *latutide*, and *elevation* which are of class numeric.
* The *elevation* and *timezone* columns must exist for functions to run, but aren't required to be populated (can be left blank).
* *deviceDeploymentID*, *deviceID*, and *locationID* are character identifiers for the sensors. *locationID* is typically generated from the latitude/longitude data, *deviceID* is an identification for the sensor, and *deviceDeploymentID* is a combination of the *locationID* and *deviceID*.
* Any additional meta data columns beyond those listed above can be included.
#### Sensor Time Series Data

The time series data file must have each row as an hourly **UTC** timestamp (with date and time) and columns of the measured parameter for each device, plus the first column for the timestamps.
* The column titles must be:
   * *datetime*
   * the *deviceDeploymentID* used in the meta data

**IMPORTANT**: The order of the *deviceDeploymentID* down the rows of the meta data must match the order of the *deviceDeploymentID* across the columns of the time series data after the datetime column.
* For example, if sensor123abc is the first row in the meta data, then sensor123abc must also be the second column in the time series data after the datetime column. If sensor456def is the second row in the meta data, then sensor456def must also be the third column in the time series data after the datetime column and the first sensor.
### Generate Bar Plots

Once the sensor data is in the appropriate format as described above, you can use the code in this repository to make your own AQI category agreement barplots. Add your meta data and time series data files to the data folder of this repository to begin walking through the template. You can follow the example laid out in (link), which also shows using example sensor data. Should this example and these functions not work exactly for you, feel free to take the code as inspiration for generating these plots in a way that works best for you. 
## Contact

Please direct inquiries to:

Elizabeth Good, Physical Scientist<br>U.S. EPA, Office of Air Quality Planning and Standards<br>Ambient Air Monitoring Group<br>Email: good.elizabeth@epa.gov
## References and Acknowledgements 

This code relys on functions from packages by Jon Callahan. Visit https://github.com/MazamaScience to read package documentation.
## Disclaimer

The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government. 
