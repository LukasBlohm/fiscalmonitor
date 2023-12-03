# Fiscal Monitor

## Overview

R-Shiny App for exploratory analyses of data of the Swiss Federal Finance Administration (FFA).

## Installation

-   Make sure to have R, R-Studio and the `shiny` and `golem` packages installed.
-   Clone repository
-   Download and unzip the file "All files (GFS / FS)" from the [FFA](https://www.efv.admin.ch/efv/en/home/themen/finanzstatistik/daten.html)
-   Download the "ThemaKart map boundaries - Set 2023" from the [FSO](https://www.bfs.admin.ch/bfs/en/home/statistics/regional-statistics/base-maps/cartographic-bases.html)
-   Create `.Renviron` file in to root folder of the repository. In this file, set `PATH_FFA="<insert path here>"` and `PATH_MAP="<insert path here>"`, specifying the paths to the downloaded folders.

## Usage

-   Start the app `golem::run_dev()` in the R console (accept installation of required additional dependencies if prompted to do so).
