# Fiscal Monitor

## Overview

R-Shiny App for exploratory analyses of data on the public finances of Swiss cantons. 

The app currently includes 3 modules:
-   A tool to explore time series for various fiscal indicators on a cantonal basis. 
-   An interactive map, coloring the cantons based on their values for the selected year-variable combination.
-   A regression application where the coefficients for the year on a selected variable is estimated and visualised on a cantonal basis. 

## Live Demo

Check out the app on [Heroku](https://fiscalmonitor-096a1fdc3ae8.herokuapp.com).

## Local Installation and Usage 

-   Make sure to have R, R-Studio and the `shiny` and `golem` packages installed. Other packages (listed in the DESCRIPTION file) can also be installed at a later stage.
-   Clone this repository and open the project (via fiscalmonitor.Rproj).
-   Create the file `.Renviron` in to root folder of the repository. 
-   The repo already contains the prepared dataset (data_final/final_dataset.rds). However, it can also automatically create this dataset from raw data, which may be useful if new periods have been added. To make use of this, 
    1) delete final_dataset.rds 
    2) download and unzip the file "All files (GFS / FS)" from the [SFFA](https://www.efv.admin.ch/efv/en/home/themen/finanzstatistik/daten.html)
    3) In `.Renviron`, add the line `PATH_FFA="<insert path to the downloaded data here>"`
-   Download the map data, "ThemaKart map boundaries - Set 2023" from the [SFSO](https://www.bfs.admin.ch/bfs/en/home/statistics/regional-statistics/base-maps/cartographic-bases.html). Set `PATH_MAP="<insert path here>"` in `.Renviron`, specifying the paths to the downloaded folders.
-   Start the app `golem::run_dev()` in the R console (accept installation of required additional dependencies if prompted to do so). If you kept final_data.rds, the startup should be quick. Otherwise, the app first prepares the dataset. In this process, it also downloads population data from the [SFSO](https://www.pxweb.bfs.admin.ch/pxweb/en/px-x-0102020000_104/-/px-x-0102020000_104.px/) via the `pxR` package.
-   If the step above worked, you can also install the package with `devtools::install()` and then use it in any R session on your system with `fiscalmonitor::run_app()`.

## Data Sources

-   Swiss Federal Finance Administration (SFFA): Detailed data FS.
-   Swiss Federal Statistical Office (SFSO): Demographic balance by age and canton.
-   Swiss Federal Statistical Office (SFSO): ThemaKart map boundaries - Set 2023.
