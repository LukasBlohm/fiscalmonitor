
# library(tidyverse)
# library(bfsMaps)
# library(readxl)
# library(janitor)

PATHS <- list()

# Ensure that the path does not end with a /
# PATHS$data_map <- stringr::str_replace(
#   Sys.getenv("PATH_MAP"), "2023_GEOM_TK/", "2023_GEOM_TK"
#   )

PATHS$data_map <- "data_map/2023_GEOM_TK"

# Tell the bfsMaps where the maps are stored
options(bfsMaps.base = PATHS$data_map)

PATHS$data_raw <- Sys.getenv("PATH_FFA")
# Ensure that the path ends with a /
PATHS$data_raw <- ifelse(
  stringr::str_ends(PATHS$data_raw, stringr::fixed("/")),
  PATHS$data_raw, paste0(PATHS$data_raw, "/")
  )

PATHS$data_intermediate <- "data_intermediate/"

PATHS$fso_url <- fso_url <- "https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?"
PATHS$fso_pop_file <- "px-x-0102020000_104"

PATHS$fso_population <- paste0(PATHS$data_intermediate, "fso_population.rds")
PATHS$ffa_rev_exp <- paste0(PATHS$data_intermediate, "ffa_rev_exp.rds")
PATHS$ffa_balance <- paste0(PATHS$data_intermediate, "ffa_balance.rds")

PATHS$data_final <- paste0("data_final/final_dataset.rds")


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
         "Aargau", "Appenzell Innerrhoden", "Appenzell Ausserrhoden",
         "Bern / Berne", "Basel-Landschaft", "Basel-Stadt", "Fribourg / Freiburg",
         "Genève", "Glarus", "Graubünden / Grigioni / Grischun", "Jura", "Luzern",
         "Neuchâtel", "Nidwalden", "Obwalden", "St. Gallen", "Schaffhausen",
         "Solothurn", "Schwyz", "Thurgau", "Ticino",
         "Uri", "Vaud", "Valais / Wallis", "Zug", "Zürich"
         )
       ),
  class = "data.frame", row.names = c(NA, -26L))
