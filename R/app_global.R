
PATHS <- list()


# Preparations for map data --------------------------------------------------------

PATHS$data_map <- "data_map/2023_GEOM_TK"

# Tell the bfsMaps package where the maps are stored
options(bfsMaps.base = PATHS$data_map)

# Verify that files are there
message("Content of the map directory:\n",
        paste(dir(options()$bfsMaps.base, recursive = TRUE), collapse = "\n"))


# Conditionally adjust Unicode normalization in internal package data maps.csv
# (which stores filenames) from NFC to NFD
# when the code runs in a container (assumes NFC) on macOS (gives NFD)
running_env <- Sys.getenv("RUNNING_ENV")

if (running_env == "macOS_container") {
  package_dir <- find.package("bfsMaps")
  maps_csv_path <- file.path(package_dir, "extdata", "maps.csv")

  maps_csv_original <- read.csv(maps_csv_path, sep = ";")

  maps_csv_nfd <- maps_csv_original |>
    dplyr::mutate(dplyr::across(tidyselect::everything(), ~ stringi::stri_trans_nfd(.)))
  readr::write_delim(maps_csv_nfd, file = maps_csv_path, delim = ";")
}



# Further paths ------------------------------------------------------------------

PATHS$data_raw <- Sys.getenv("PATH_FFA")
# Ensure that the path ends with a /
PATHS$data_raw <- ifelse(
  stringr::str_ends(PATHS$data_raw, stringr::fixed("/")),
  PATHS$data_raw, paste0(PATHS$data_raw, "/")
  )

PATHS$data_prepared <- "data_prepared/"

PATHS$fso_url <- fso_url <- "https://www.pxweb.bfs.admin.ch/DownloadFile.aspx?"
PATHS$fso_pop_file <- "px-x-0102020000_104"

PATHS$fso_population <- paste0(PATHS$data_prepared, "fso_population.rds")
PATHS$ffa_rev_exp <- paste0(PATHS$data_prepared, "ffa_rev_exp.rds")
PATHS$ffa_balance <- paste0(PATHS$data_prepared, "ffa_balance.rds")

PATHS$data_final <- paste0("data_final/final_dataset.rds")


# Mappings of variables -----------------------------------------------

v_colors <- c(RColorBrewer::brewer.pal(n = 8, name = "Dark2"))

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



v_rev_name_mapping <- c(
  "total" = "Gesamteinnahmen",
  "fiscal" = "Fiskaleinnahmen",
  "taxinc" = "Einkommenssteuern natürliche Personen",
  "taxwealth" = "Vermögenssteuern natürliche Personen",
  "taxprofit" = "Gewinnsteuern juristische Personen",
  "taxcap" = "Kapitalsteuern juristische Personen",
  "int" = "Zinseinnahmen",
  "transfer" = "Transfereinnahmen",
  "fla" = "Finanz- und Lastenausgleich",
  "invest" = "Investitionsbeiträge"
)

v_exp_name_mapping <- c(
  "total" = "Gesamtausgaben",
  "admin" = "Allgemeine Verwaltung",
  "security" = "Öffentliche Ordnung und Sicherheit, Verteidigung",
  "education" = "Bildung",
  "culture" = "Kultur, Sport und Freizeit, Kirche",
  "health" = "Gesundheit",
  "socsecurity" = "Soziale Sicherheit",
  "traffic" = "Verkehr und Nachrichtenübermittlung",
  "environment" = "Umweltschutz und Raumordnung",
  "economy" = "Volkswirtschaft",
  "finance" = "Finanzen und Steuern"
)

v_balance_name_mapping <- c(
  "active" = "Aktiven",
  "assetsfinance" = "Finanzvermögen",
  "assetsadmin" = "Verwaltungsvermögen",
  "passive" = "Passiven",
  "debtnet" = "Nettoschulden",
  "debtgross" = "Bruttoschulden",
  "liabilities" = "Fremdkapital",
  "liabilitiesshort" = "Kurzfristige Finanzverbindlichkeiten",
  "liabilitieslong" = "Langfristige Finanzverbindlichkeiten",
  "equity" = "Eigenkapital"
)







