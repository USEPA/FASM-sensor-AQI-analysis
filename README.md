# FASM-sensor-AQI-analysis
## Background and Overview

Timely and accurate air quality information during wildland fires is essential for helping emergency responders make decisions, supporting air and public health agencies in giving informed recommendations, and enabling communities and individuals to take health protective actions. Since its launch, the AirNow Fire and Smoke Map (FASM) has proved to be a valuable resource providing timely and accurate air quality information, but there are still areas where monitoring data is sparse. Adding additional air sensor data can help improve coverage, but careful consideration is needed to ensure that FASM remains a trustworthy source of near real-time fine particulate matter (PM2.5) information.

This repository contains code to accompany the FASM Air Sensor Network Data Acceptance Protocol (link), a process and list of requirements that the U.S. Environmental Protection Agency (EPA) and the U.S. Forest Service (USFS) have developed from a pilot process for acceptance and display of new air sensor networks onto FASM. Interested data providers must follow the protocol to be considered for inclusion on the map. The core principle driving the decision to include a sensor network on FASM is whether the additional PM2.5 data provides value to users during smoke episodes. A sensor network would be prioritized if it adds new insights or extends coverage to the map.

The protocol outlines a series of requirements that aim to 1) describe the sensor, 2) describe the network, 3) ensure timely data reporting and transmission, 4) document the quality assurance and control methodology used, 5) demonstrate collocated sensor performance at high smoke concentrations in addition to meeting EPA’s sensor performance targets, and 6) demonstrate good comparability of data between the sensor network and the permanent and temporary monitoring network using thresholds developed from the pilot process.

This code is intended to help users generate a particular set of bar plots to analyze and visualize AQI category agreement between their sensors and AirNow reference monitors within a specified radius (aligned with the data requirements in Table 4 in the protocol).

## Structure and Contents

The following directory is used to organize the files in this repository:

```
FASM-sensor-AQI-analysis
├── docs
├── R
└── data
```

The working directory for all scripts and R Markdown documents will be
`FASM-sensor-AQI-analysis/`.

The `docs/` directory contains the Rmd and html files the are the tutorial walking through the template script. They are linked to the [GitHub Pages website](https://urban-bassoon-v9zkyqw.pages.github.io/) for this repository.

The `R/` directory contains the template R script for users to fill in and run the code to perform the analysis as well as supporting source R scripts with necessary functions and packages.

The `data/` directory contains example sensor data sets and is where users should save their own sensor data to perform the analysis.

Each folder has additional information and descriptions in README files.
## Getting Started

Follow these steps to set up a copy of this project on your local machine and format your sensor data for use with this repository's code:
### Prerequisites

Before you begin, ensure you have the following installed:

* **R**
* **RStudio**
* **Git**

Try [this website](https://www.sthda.com/english/wiki/installing-r-and-rstudio-easy-r-programming) for instructions and tips to install R and RStudio.
Try [this website](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git ) for instructions and tips to install and setup Git. 
### Cloning the Repository to RStudio

Cloning a repository means creating a local copy of a remote project on your computer. You download the entire project (all files, folders, etc.) from GitHub to R and RStudio on your computer. Cloning automatically sets up a connection between the new local repository on your computer and the original remote repository on GitHub. It allows you to sync your work by "pulling" any updates made to the origin to your local repository. This repository will not accept users "pushing" changes from their local copy to the original repository. 

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
### Requirements for Sensor Data

The sensor data must consist of two files (.csv or .xlsx format), one of metadata and one of time-series data that will be combined into a *mts_monitor* object for analysis. To read more about the *mts_monitor* data format, see the [MazamaTimeSeries](https://mazamascience.github.io/MazamaTimeSeries/) description from Mazama Science.
#### Sensor Metadata

The metadata file must have each row as a unique device and columns of device metadata.
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
* Any additional metadata columns beyond those listed above can be included.
#### Sensor Time-series Data

The time-series data file must have each row as an hourly **UTC** timestamp (with date and time) and columns of the measured parameter for each device, plus the first column for the timestamps.
* The column titles must be:
   * *datetime*
   * the *deviceDeploymentID* used in the metadata

**IMPORTANT**: The order of the *deviceDeploymentID* down the rows of the metadata must match the order of the *deviceDeploymentID* across the columns of the time-series data after the datetime column.
* For example, if sensor123abc is the first row in the metadata, then sensor123abc must also be the second column in the time-series data after the datetime column. If sensor456def is the second row in the metadata, then sensor456def must also be the third column in the time-series data after the datetime column and the first sensor.
### The AQI Category Agreement Analysis

Once your sensor data is in the appropriate *mts_monitor* format as described above and your code environment is appropriately configured, you can use the scripts in this repository to make your own AQI category agreement bar plots. Add your metadata and time-series data files to the `data` folder of this repository and begin walking through the `template_for_sensor_vs_monitor.R` file. You can follow the tutorial laid out in the [GitHub Pages website](https://urban-bassoon-v9zkyqw.pages.github.io/tutorial.html), which includes a narrative of instructions that go with the template file and shows the use of example sensor data. Example sensor data is available for use to follow the tutorial precisely before trying with your own sensor data.
## Contact

Please direct inquiries to:

Elizabeth Good, Physical Scientist<br>U.S. EPA, Office of Air Quality Planning and Standards<br>Ambient Air Monitoring Group<br>Email: good.elizabeth@epa.gov
## References and Acknowledgements 

This code relys on functions from packages by Dr. Jonathan Callahan and Mazama Science. Visit [Mazama Science](https://github.com/MazamaScience) on GitHub to read package documentation.

Thank you to Clarity Movement Co. (EPA MTA 1643-25, FS MTA 25-RD-11132543-061) and PurpleAir (MTA #1361-19) for providing example data via the cited Material Transfer Agreements. Clarity and PurpleAir are two sensor manufacturers currently displayed on the Fire and Smoke Map.
## Disclaimer

The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government. 
