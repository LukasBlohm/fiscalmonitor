
library(tidyverse)
library(readxl)
library(janitor)

PATHS <- list()
PATHS$data_raw <- Sys.getenv("DATAPATH")
PATHS$data_prepared <- "data_prepared/"

v_colors <- c(RColorBrewer::brewer.pal(n = 8, name = "Accent"))

df_cantons <- structure(
  list(canton_nr = 1:26,
       canton = c("AG", "AI", "AR", "BE", "BL", "BS", "FR", "GE", "GL",
                  "GR", "JU", "LU", "NE", "NW", "OW", "SG", "SH", "SO",
                  "SZ", "TG", "TI", "UR", "VD", "VS", "ZG", "ZH"),
       canton_name = c("Aargau", "Appenzell Innerrhoden", "Appenzell Ausserrhoden",
                       "Bern", "Basel-Landschaft", "Basel-Stadt", "Freiburg", "Genf",
                       "Glarus", "Graubünden", "Jura", "Luzern", "Neuenburg", "Nidwalden",
                       "Obwalden", "St. Gallen", "Schaffhausen", "Solothurn", "Schwyz",
                       "Thurgau", "Tessin", "Uri", "Waadt", "Wallis", "Zug", "Zürich"
       )),
  class = "data.frame", row.names = c(NA, -26L))
