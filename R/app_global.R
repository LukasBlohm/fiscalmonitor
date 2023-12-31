
# library(tidyverse)
# library(bfsMaps)
# library(readxl)
# library(janitor)

PATHS <- list()

# Ensure that the path does not end with a /
PATHS$data_map <- stringr::str_replace(
  Sys.getenv("PATH_MAP"), "2023_GEOM_TK/", "2023_GEOM_TK"
  )
# Tell the bfsMaps where the maps are stored
options(bfsMaps.base = PATHS$data_map)

PATHS$data_raw <- Sys.getenv("PATH_FFA")
# Ensure that the path ends with a /
PATHS$data_raw <- ifelse(
  stringr::str_ends(PATHS$data_raw, stringr::fixed("/")),
  PATHS$data_raw, paste0(PATHS$data_raw, "/")
  )

PATHS$data_prepared <- "data_prepared/"

PATHS$fso_url <- fso_url <- "https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?"
PATHS$fso_pop_file <- "px-x-0102020000_104"

PATHS$ffa_prepared <- paste0(PATHS$data_prepared, "ffa_data.rds")

v_colors <- c(RColorBrewer::brewer.pal(n = 8, name = "Accent"))

df_cantons <- structure(
  list(canton_nr = 1:26,
       canton = c(
         "AG", "AI", "AR", "BE", "BL", "BS", "FR", "GE", "GL",
         "GR", "JU", "LU", "NE", "NW", "OW", "SG", "SH", "SO",
         "SZ", "TG", "TI", "UR", "VD", "VS", "ZG", "ZH"),
       canton_name = c(
         "Aargau", "Appenzell Innerrhoden", "Appenzell Ausserrhoden",
         "Bern", "Basel-Landschaft", "Basel-Stadt", "Freiburg", "Genf",
         "Glarus", "Graubünden", "Jura", "Luzern", "Neuenburg", "Nidwalden",
         "Obwalden", "St. Gallen", "Schaffhausen", "Solothurn", "Schwyz",
         "Thurgau", "Tessin", "Uri", "Waadt", "Wallis", "Zug", "Zürich"
         ),
       canton_name_fso = c(
         "Aargau", "Appenzell Ausserrhoden", "Appenzell Innerrhoden",
         "Basel-Landschaft", "Basel-Stadt", "Bern / Berne", "Fribourg / Freiburg",
         "Genève", "Glarus", "Graubünden / Grigioni / Grischun", "Jura", "Luzern",
         "Neuchâtel", "Nidwalden", "Obwalden", "Schaffhausen",
         "Schwyz", "Solothurn", "St. Gallen", "Thurgau", "Ticino",
         "Uri", "Valais / Wallis", "Vaud", "Zug", "Zürich"
         )
       ),
  class = "data.frame", row.names = c(NA, -26L))
