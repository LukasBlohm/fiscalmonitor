# Fiscal Monitor

## Overview

R-Shiny App for exploratory analyses of data of the Swiss Federal Finance Administration (FFA).

## Installation

-   Make sure to have R, R-Studio and the `shiny` and `golem` packages installed.
-   Clone repository
-   Download the data from the [FFA](https://www.efv.admin.ch/dam/efv/de/dokumente/finanzstatistik/daten/fs-zipdatei.zip.download.zip/zip_d.zip)
-   Create .Renviron file in to root folder of the repository. In this file, set DATAPATH="path", specifying the path to the downloaded folder.

## Usage

-   Start the app `golem::run_dev()` in the R console (accept installation of required additional dependencies if prompted to do so).
