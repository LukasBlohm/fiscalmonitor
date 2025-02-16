#' Prepare revenues and expenditures
#'
#' @param df_cantons Data frame, storing canton data (name, abbreviation, etc.)
#' @param save_to String, storing the path where the output should be saved
#'
#' @importFrom magrittr %>%
#'
#' @return Data frame
prepare_rev_exp <- function(df_cantons, save_to = NULL) {

  cli::cli_process_start("Prepare revenue data")

  df_revenues <- purrr::map(
    df_cantons$canton, ~prepare_canton(canton_abb = .x, category = "rev")
    ) %>%
    dplyr::bind_rows()


  cli::cli_process_done()
  cli::cli_process_start("Prepare expenditure data")

  df_expenditures <- purrr::map(
    df_cantons$canton, ~prepare_canton(canton_abb = .x, category = "exp")
    ) %>%
    dplyr::bind_rows()

  cli::cli_process_done()


  cli::cli_process_start("Prepare df_rev_exp")

  df_rev_exp <- df_revenues %>%
    dplyr::bind_rows(df_expenditures) %>%
    dplyr::mutate(
      value = tidyr::replace_na(as.numeric(value), 0) # concerns only interest expenditure in AI 2015-2020
      ) %>%
    dplyr::mutate(value = value / 1000) %>%                      # from 1000CHF to mio CHF
    dplyr::select(canton, year, item, value) %>%
    unique() %>%
    tidyr::pivot_wider(names_from = item, values_from = value) %>%  #, names_sep = "_") %>%
    dplyr::select(
      canton, year,
      # can_rev_total_mio = "Gesamteinnahmen",
      # can_rev_fiscal_mio = "Fiskaleinnahmen",
      # can_rev_taxinc_mio = "Einkommenssteuern natürliche Personen",
      # can_rev_taxwealth_mio = "Vermögenssteuern natürliche Personen",
      # can_rev_taxprofit_mio = "Gewinnsteuern juristische Personen",
      # can_rev_taxcap_mio = "Kapitalsteuern juristische Personen",
      # can_rev_int_mio = "Zinseinnahmen",
      # can_rev_transfer_mio = "Transfereinnahmen",
      # can_rev_fla_mio = "Finanz- und Lastenausgleich",
      # can_rev_invest_mio = "Investitionsbeiträge",
      #
      # can_exp_total_mio = "Gesamtausgaben",
      # can_exp_admin_mio = "Allgemeine Verwaltung",
      # can_exp_security_mio = "Öffentliche Ordnung und Sicherheit, Verteidigung",
      # can_exp_education_mio = "Bildung",
      # can_exp_culture_mio = "Kultur, Sport und Freizeit, Kirche",
      # can_exp_health_mio = "Gesundheit",
      # can_exp_socsecurity_mio = "Soziale Sicherheit",
      # can_exp_traffic_mio = "Verkehr und Nachrichtenübermittlung",
      # can_exp_environment_mio = "Umweltschutz und Raumordnung",
      # can_exp_economy_mio = "Volkswirtschaft",
      # can_exp_finance_mio = "Finanzen und Steuern"
      can_rev_total_mio = "Total receipts",
      can_rev_fiscal_mio = "Tax receipts",
      can_rev_taxinc_mio = "Income tax, natural persons",
      can_rev_taxwealth_mio = "Wealth tax, natural persons",
      can_rev_taxprofit_mio = "Profit taxes, legal entities",
      can_rev_taxcap_mio = "Taxes on capital, legal entities",
      can_rev_int_mio = "Interest income",
      can_rev_transfer_mio = "Fiscal equalization and cost compensation",
      can_rev_fla_mio = "Transfer of financial interests",
      can_rev_invest_mio = "Investment contributions",

      can_exp_total_mio = "Total expenditure",
      can_exp_admin_mio = "General administration",
      can_exp_security_mio = "Public order and security, defense",
      can_exp_education_mio = "Education",
      can_exp_culture_mio = "Culture, sport and leisure, church",
      can_exp_health_mio = "Health",
      can_exp_socsecurity_mio = "Social security",
      can_exp_traffic_mio = "Transportation and telecommunications",
      can_exp_environment_mio = "Protection of the environment and spatial planning",
      can_exp_economy_mio = "National economy",
      can_exp_finance_mio = "Finances and taxes"
    )

  cli::cli_process_done()


  if (!is.null(save_to)){
    readr::write_rds(df_rev_exp, save_to)
    cli::cli_alert_info("Saved df_rev_exp in ", save_to)
  }

  return(df_rev_exp)
}


#### Debt (Cantonal Balance Sheets) ####

#' prepare_balance
#'
#' @param df_cantons Data frame, storing canton data (name, abbreviation, etc.)
#' @param save_to String, storing the path where the output should be saved
#'
#' @importFrom magrittr %>%
#'
#' @return Data frame
prepare_balance <- function(df_cantons, save_to = NULL) {

  df_balance <- purrr::map(
    df_cantons$canton,
    ~prepare_canton(canton_abb = .x, category = "balance")
    ) %>%
    dplyr::bind_rows() %>%
    tidyr::pivot_wider(values_from = "value", names_from = "item") %>%
    dplyr::mutate(
      dplyr::across(
        3:tidyselect::last_col(),
        ~dplyr::if_else(is.na(as.numeric(.)), 0, as.numeric(.) / 1000)    # CHF 1000 -> CHF mio
      )
    ) %>%
    dplyr::mutate(
      can_balance_debtnet_mio = (Fremdkapital - Finanzvermögen),
      can_balance_debtgross_mio = (
        `Laufende Verbindlichkeiten` +
          `Kurzfristige Finanzverbindlichkeiten` +
          `Langfristige Finanzverbindlichkeiten`)
    ) %>%
    dplyr::select(
      canton, year, can_balance_debtnet_mio, can_balance_debtgross_mio,
      can_balance_active_mio = "Aktiven",
      can_balance_assetsfinance_mio = "Finanzvermögen",
      can_balance_assetsadmin_mio = "Verwaltungsvermögen",
      can_balance_passive_mio = "Passiven",
      can_balance_liabilities_mio = "Fremdkapital",
      can_balance_liabilitiesshort_mio = "Kurzfristige Finanzverbindlichkeiten",
      can_balance_liabilitieslong_mio = "Langfristige Finanzverbindlichkeiten",
      can_balance_equity_mio = "Eigenkapital"
    )

  message("Prepared df_balance")

  if (!is.null(save_to)){
    readr::write_rds(df_balance, save_to)
    message("Saved df_balance in ", save_to)
  }

  return(df_balance)
}







