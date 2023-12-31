#' Prepare indicator category for a canton
#'
#' @param canton_abb String, denoting the abbreviation of the canton
#' @param category String, denoting the indicator category
#'
#' @importFrom magrittr %>%
#'
#' @return Data frame
prepare_canton <- function(canton_abb, category) {

  sheet <- dplyr::case_when(
    category == "rev" ~ "einnahmen",
    category == "exp" ~ "ausgaben_funk",
    category == "balance" ~ "bilanz"
  )

  target_variables <- unlist(
    dplyr::case_when(
      category == "rev" ~ list(
        c("Gesamteinnahmen",
          "Fiskaleinnahmen",
          "Einkommenssteuern natürliche Personen",
          "Vermögenssteuern natürliche Personen",
          "Gewinnsteuern juristische Personen",
          "Kapitalsteuern juristische Personen",
          "Zinseinnahmen",
          "Transfereinnahmen",
          "Finanz- und Lastenausgleich",
          "Investitionsbeiträge"
        )
      ),
      category == "exp" ~ list(
        c("Gesamtausgaben",
          "Allgemeine Verwaltung",
          "Öffentliche Ordnung und Sicherheit, Verteidigung",
          "Bildung",
          "Kultur, Sport und Freizeit, Kirche",
          "Gesundheit",
          "Soziale Sicherheit",
          "Verkehr und Nachrichtenübermittlung",
          "Umweltschutz und Raumordnung",
          "Volkswirtschaft",
          "Finanzen und Steuern")
      ),
      category == "balance" ~ list(c(
        "Aktiven",
        "Finanzvermögen",
        "Verwaltungsvermögen",
        "Passiven",
        "Fremdkapital",
        "Laufende Verbindlichkeiten",
        "Kurzfristige Finanzverbindlichkeiten",
        "Langfristige Finanzverbindlichkeiten",
        "Eigenkapital")
      )
    )
  )

  df_temp <- suppressMessages(
    readxl::read_excel(
      paste0(.GlobalEnv$PATHS$data_raw,
             "fs_ktn/ktn_", tolower(canton_abb), ".xlsx"),
      sheet = sheet, col_names = FALSE)
  )

  df_temp <- df_temp[6:nrow(df_temp), ] %>%
    tidyr::unite(funct, c("...1", "...2")) %>%
    janitor::row_to_names(row_number = 1)  %>%
    t() %>%
    data.frame() %>%
    tibble::rownames_to_column("year") %>%
    janitor::row_to_names(row_number = 1) %>%
    dplyr::rename(year = 1) %>%
    tidyr::gather("item", "value", -year) %>%
    # separate(col = item, into = c("item_nr", "item"), sep = "_") %>%
    tidyr::separate_wider_delim(
      cols = item, names = c("item_nr", "item"), delim = "_"
      ) %>%
    dplyr::filter(item %in% target_variables) %>%
    dplyr::mutate(canton = toupper(canton_abb)) %>%
    dplyr::select(-item_nr)

  message("Prepared ", category, " for ", toupper(canton_abb))

  return(df_temp)
}




