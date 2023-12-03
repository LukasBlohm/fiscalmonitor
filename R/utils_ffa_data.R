#' Prepare indicator category for a canton
#'
#' @param canton_abb String, denoting the abbreviation of the canton
#' @param category String, denoting the indicator category
#'
#' @return Data frame
prepare_canton <- function(canton_abb, category) {

  sheet <- case_when(category == "rev" ~ "einnahmen",
                     category == "exp" ~ "ausgaben_funk",
                     category == "balance" ~ "bilanz")

  target_variables <- unlist(
    case_when(
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

  df_temp <- suppressMessages(read_excel(
    paste0(.GlobalEnv$PATHS$data_raw, "fs_ktn/ktn_", tolower(canton_abb), ".xlsx"),
    sheet = sheet, col_names = FALSE))

  df_temp <- df_temp[6:nrow(df_temp), ] %>%
    unite(funct, c("...1", "...2")) %>%
    row_to_names(row_number = 1)  %>%
    t() %>%
    data.frame() %>%
    rownames_to_column("year") %>%
    row_to_names(row_number = 1) %>%
    rename(year = 1) %>%
    gather("item", "value", -year) %>%
    separate(col = item, into = c("item_nr", "item"), sep = "_") %>%
    filter(item %in% target_variables) %>%
    mutate(canton = toupper(canton_abb)) %>%
    select(-item_nr)

  message("Prepared ", category, " for ", toupper(canton_abb))

  return(df_temp)
}



#' prepare_debt
#'
#' @param category String, denoting the aggregation level, i.e. agg or pc
#' @param level String, denoting the federal level, i.e. cantonal or municipal.
#' @param df_cantons Data frame, storing canton data (name, abbreviation, etc.)
#'
#' @return Data frame
prepare_debt_indicator <- function(category, level, df_cantons) {

  sheet <- case_when(category == "agg" ~ "schuld",
                     category == "pc" ~ "schuld_per_capita")

  df_temp <- suppressMessages(read_excel(paste0(.GlobalEnv$PATHS$data_raw,
                                                case_when(level == "cantonal" ~ "fs_ktn/ktn",
                                                          level == "municipal" ~ "fs_gdn/gdn"),
                                                "_schuld.xlsx"),
                                         sheet = sheet))

  df_debt <- df_temp[c(5, 7:nrow(df_temp)), 2:ncol(df_temp)] %>%
    t() %>%
    data.frame() %>%
    row_to_names(row_number = 1) %>%
    rename(year = 1) %>%
    mutate(across(1:last_col(), ~ as.numeric(.))) %>%
    pivot_longer(cols = 2:27, names_to = "canton_name", values_to = "chf") %>%
    mutate(level = level,
           category = category) %>%
    mutate(chf = if_else(category == "agg", chf * 1000, chf)) %>%   # 1000 CHF to CHF
    mutate(canton_name = if_else(
      str_detect(canton_name, "Rechnungen der Stadt und des Kantons Basel"),
      "Gemeinden Kanton Basel-Stadt",
      canton_name)) %>%
    mutate(canton_name = str_replace(
      canton_name, pattern = if_else(
        level == "cantonal", "Kanton " , "Gemeinden Kanton "),
      replacement = "")) %>%
    left_join(df_cantons, by = "canton_name") %>%
    select(canton, year, level, category, chf)

  message("Prepared debt ", category, " on level", level)

  return(df_debt)
}