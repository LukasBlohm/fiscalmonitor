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



#' prepare_debt
#'
#' @param category_selected String, denoting the aggregation level, i.e. agg or pc
#' @param federal_level_selected String, denoting the federal level, i.e. cantonal or municipal.
#' @param df_cantons Data frame, storing canton data (name, abbreviation, etc.)
#'
#' @importFrom magrittr %>%
#'
#' @return Data frame
prepare_debt_indicator <- function(category_selected, federal_level_selected, df_cantons) {

  sheet <- dplyr::case_when(
    category_selected == "agg" ~ "schuld",
    category_selected == "pc" ~ "schuld_per_capita"
    )

  df_temp <- suppressMessages(
    readxl::read_excel(
      paste0(.GlobalEnv$PATHS$data_raw,
             dplyr::case_when(
               federal_level_selected == "cantonal" ~ "fs_ktn/ktn",
               federal_level_selected == "municipal" ~ "fs_gdn/gdn"),
             "_schuld.xlsx"),
      sheet = sheet
    )
  )

  df_debt <- df_temp[c(5, 7:nrow(df_temp)), 2:ncol(df_temp)] %>%
    t() %>%
    data.frame() %>%
    janitor::row_to_names(row_number = 1) %>%
    dplyr::rename(year = 1) %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), ~ as.numeric(.))) %>%
    tidyr::pivot_longer(cols = 2:27, names_to = "canton_name", values_to = "chf") %>%
    dplyr::mutate(federal_level = federal_level_selected, category = category_selected) %>%
    { if (category_selected == "agg")
      dplyr::mutate(., chf = chf * 1000) else .
      } %>%
    # dplyr::mutate(chf = ifelse(category == "agg", chf * 1000, chf)) %>%   # 1000 CHF to CHF
    dplyr::mutate(canton_name = dplyr::if_else(
      stringr::str_detect(canton_name, "Rechnungen der Stadt und des Kantons Basel"),
      "Gemeinden Kanton Basel-Stadt",
      canton_name)) %>%
    # Remove prefixes "Kanton " or "Gemeinden Kanton " from canton_name column.
    dplyr::mutate(canton_name = stringr::str_replace(
      canton_name, pattern = dplyr::if_else(
        federal_level == "cantonal", "Kanton " , "Gemeinden Kanton "),
      replacement = "")) %>%
    dplyr::left_join(df_cantons, by = "canton_name") %>%
    dplyr::select(canton, year, federal_level, category, chf)

  message("Prepared debt ", category_selected, " on level", federal_level_selected)

  return(df_debt)
}
