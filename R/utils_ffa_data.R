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
        c(
        # "Gesamteinnahmen",
        # "Fiskaleinnahmen",
        # "Einkommenssteuern natürliche Personen",
        # "Vermögenssteuern natürliche Personen",
        # "Gewinnsteuern juristische Personen",
        # "Kapitalsteuern juristische Personen",
        # "Zinseinnahmen",
        # "Transfereinnahmen",
        # "Finanz- und Lastenausgleich",
        # "Investitionsbeiträge"
          "Total receipts",
          "Tax receipts",
          "Income tax, natural persons",
          "Wealth tax, natural persons",
          "Profit taxes, legal entities",
          "Taxes on capital, legal entities",
          "Interest income",
          "Transfer of financial interests",
          "Fiscal equalization and cost compensation",
          "Investment contributions"
        )
      ),
      category == "exp" ~ list(
        c(
          # "Gesamtausgaben",
          # "Allgemeine Verwaltung",
          # "Öffentliche Ordnung und Sicherheit, Verteidigung",
          # "Bildung",
          # "Kultur, Sport und Freizeit, Kirche",
          # "Gesundheit",
          # "Soziale Sicherheit",
          # "Verkehr und Nachrichtenübermittlung",
          # "Umweltschutz und Raumordnung",
          # "Volkswirtschaft",
          # "Finanzen und Steuern"
          "Total expenditure",
          "General administration",
          "Public order and security, defense",
          "Education",
          "Culture, sport and leisure, church",
          "Health",
          "Social security",
          "Transportation and telecommunications",
          "Protection of the environment and spatial planning",
          "National economy",
          "Finances and taxes"
          )
      ),
      category == "balance" ~ list(c(
        # "Aktiven",
        # "Finanzvermögen",
        # "Verwaltungsvermögen",
        # "Passiven",
        # "Fremdkapital",
        # "Laufende Verbindlichkeiten",
        # "Kurzfristige Finanzverbindlichkeiten",
        # "Langfristige Finanzverbindlichkeiten",
        # "Eigenkapital"

        "Cash and cash equivalents",
        "Receivables",
        "Short-term financial investments",
        "Prepaid expenses and accrued income",
        "Inventories and work in progress",
        "Long-term financial investments",
        "Tangible fixed assets, NAA",
        "Receivables from special financing and funds in liabilities",
        "Tangible fixed assets, AA",
        "Inventories, AA",
        "Intangible fixed assets",
        "Loans and financial interests n.e.c.",
        "Loans",
        "Financial interests, share capital",
        "Investment contributions",
        "Assets due from government units",
        "Accumulated additional depreciation and amortization",
        "Current liabilities",
        "Short-term financial liabilities", "Accrued expenses and deferred income",
        "Short-term provisions", "Long-term financial liabilities",
        "Liabilities toward government units",
        "Long-term provisions",
        "Restricted funds",
        "Special financing and funds in net assets/equity",
        "Global budget area reserves",
        "Advance financing",
        "Fiscal policy reserves",
        "Restatement reserve",
        "Revaluation reserve, non-administrative assets",
        "Other net assets/equity",
        "Accumulated surplus/deficit"

        )
      )
    )
  )

  cli::cli_process_start("Prepare {category} for {toupper(canton_abb)}")

  df_temp <- suppressMessages(
    readxl::read_excel(
      paste0(.GlobalEnv$PATHS$data_raw,
             #"fs_ktn/ktn_",
             tolower(canton_abb), "_raw.xlsx"),
      sheet = sheet, col_names = FALSE)
  )

  df_temp_processed <- df_temp[6:nrow(df_temp), ] %>%
    tidyr::unite(funct, c("...1", "...2")) %>%
    janitor::row_to_names(row_number = 1)  %>%
    t() %>%
    data.frame() %>%
    tibble::rownames_to_column("year") %>%
    janitor::row_to_names(row_number = 1) %>%
    dplyr::rename(year = 1) %>%
    tidyr::gather("item", "value", -year) %>%
    tidyr::separate_wider_delim(
      cols = item, names = c("item_nr", "item"), delim = "_"
      ) %>%
    dplyr::filter(item %in% target_variables) %>%
    dplyr::mutate(year = as.integer(year),
                  canton = toupper(canton_abb)) %>%
    dplyr::select(-item_nr)

  cli::cli_process_done()

  return(df_temp_processed)
}




