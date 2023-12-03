#' Prepare revenues and expenditures
#'
#' @param df_cantons Data frame, storing canton data (name, abbreviation, etc.)
#'
#' @return Data frame
prepare_rev_exp <- function(df_cantons) {

  df_revenues <- map(
    df_cantons$canton, ~prepare_canton(canton_abb = .x, category = "rev")
    ) %>%
    bind_rows()

  message("Prepared df_revenues")

  df_expenditures <- map(
    df_cantons$canton, ~prepare_canton(canton_abb = .x, category = "exp")
    ) %>%
    bind_rows()

  message("Prepared df_expenditures")

  df_rev_exp <- df_revenues %>%
    bind_rows(df_expenditures) %>%
    mutate(year = as.numeric(year),
           value = replace_na(as.numeric(value), 0)) %>%  # concerns only interest expenditure in AI 2015-2020
    mutate(value = value / 1000) %>%                      # from 1000CHF to mio CHF
    select(canton, year, item, value) %>%
    unique() %>%
    pivot_wider(names_from = item, values_from = value) %>%  #, names_sep = "_") %>%
    select(canton, year,
           can_rev_total_mio = "Gesamteinnahmen",
           can_rev_fiscal_mio = "Fiskaleinnahmen",
           can_rev_taxinc_mio = "Einkommenssteuern natürliche Personen",
           can_rev_taxwealth_mio = "Vermögenssteuern natürliche Personen",
           can_rev_taxprofit_mio = "Gewinnsteuern juristische Personen",
           can_rev_taxcap_mio = "Kapitalsteuern juristische Personen",
           can_rev_int_mio = "Zinseinnahmen",
           can_rev_transfer_mio = "Transfereinnahmen",
           can_rev_fla_mio = "Finanz- und Lastenausgleich",
           can_rev_invest_mio = "Investitionsbeiträge",

           can_exp_total_mio = "Gesamtausgaben",
           can_exp_admin_mio = "Allgemeine Verwaltung",
           can_exp_security_mio = "Öffentliche Ordnung und Sicherheit, Verteidigung",
           can_exp_education_mio = "Bildung",
           can_exp_culture_mio = "Kultur, Sport und Freizeit, Kirche",
           can_exp_health_mio = "Gesundheit",
           can_exp_socsecurity_mio = "Soziale Sicherheit",
           can_exp_traffic_mio = "Verkehr und Nachrichtenübermittlung",
           can_exp_environment_mio = "Umweltschutz und Raumordnung",
           can_exp_economy_mio = "Volkswirtschaft",
           can_exp_finance_mio = "Finanzen und Steuern")

  message("Prepared df_rev_exp")

  return(df_rev_exp)
}


#### Debt (Cantonal Balance Sheets) ####

#' prepare_balance
#'
#' @param df_cantons Data frame, storing canton data (name, abbreviation, etc.)
#'
#' @return Data frame
prepare_balance <- function(df_cantons) {

  df_balance <- map(df_cantons$canton,
                    ~prepare_canton(canton_abb = .x, category = "balance")) %>%
    bind_rows() %>%
    pivot_wider(values_from = "value", names_from = "item") %>%
    mutate(across(c(1, 3:last_col()), ~if_else(is.na(as.numeric(.)), 0, as.numeric(.)))) %>%
    mutate(can_balance_debtnet_mio = (Fremdkapital - Finanzvermögen) / 1000,    #
           can_balance_debtgross_mio = (`Laufende Verbindlichkeiten` +
                               `Kurzfristige Finanzverbindlichkeiten` +
                               `Langfristige Finanzverbindlichkeiten`) / 1000) %>%
    select(canton, year, can_balance_debtnet_mio, can_balance_debtgross_mio,
           can_balance_active_mio = "Aktiven",
           can_balance_assetsfinance_mio = "Finanzvermögen",
           can_balance_assetsadmin_mio = "Verwaltungsvermögen",
           can_balance_passive_mio = "Passiven",
           can_balance_liabilities_mio = "Fremdkapital",
           can_balance_liabilitiesshort_mio = "Kurzfristige Finanzverbindlichkeiten",
           can_balance_liabilitieslong_mio = "Langfristige Finanzverbindlichkeiten",
           can_balance_equity_mio = "Eigenkapital",)

  message("Prepared df_balance")

  return(df_balance)
}

#' prepare_debt
#'
#' @param df_cantons Data frame, storing canton data (name, abbreviation, etc.)
#'
#' @return Data frame
prepare_debt <- function(df_cantons) {

  ## Debt (FFA debt tables)
  df_debt <- map2(rep(c("agg", "pc"), 2),
                  rep(c("cantonal", "municipal"), each = 2),
                  ~prepare_debt_indicator(category = .x, level = .y, df_cantons)) %>%
    bind_rows() %>%
    pivot_wider(names_from = c("level", "category"), values_from = "chf") %>%
    mutate(across(contains("agg"), ~ . / 1000000)) %>%   # From CHF to mio. CHF
    select(canton, year,
           can_debt_agg_mio = "cantonal_agg",
           mun_debt_agg_mio = "municipal_agg",
           can_debt_pc_chf = "cantonal_pc",
           mun_debt_pc_chf = "municipal_pc")

  message("Prepared df_debt")

  return(df_debt)
}




#### Prepare full Dataset ####

#' prepare_full_ffa_data
#'
#' @param df_cantons Data frame, storing canton data (name, abbreviation, etc.)
#' @param save Logical, should the data be saved (under data_prepared/)?
#'
#' @return Data frame of the fiscal data
#' @export
#'
#' @examples
#' \dontrun{df <- prepare_full_ffa_data(df_cantons)}
prepare_full_ffa_data <- function(df_cantons, save = FALSE)  {

  message("Prepare FFA data")

  df_rev_exp <- prepare_rev_exp(df_cantons)
  message("------------------------------------------")
  df_debt <- prepare_debt(df_cantons)
  message("------------------------------------------")
  df_balance <- prepare_balance(df_cantons)
  message("------------------------------------------")

  df_final <- df_rev_exp %>%
    left_join(df_balance, by = c("canton", "year")) %>%
    full_join(df_debt, by = c("canton", "year")) %>%
    arrange(canton, year) %>%
    pivot_longer(
      -c(canton, year),
      names_to = c("level", "cat1", "cat2", "unit"), names_sep = "_") %>%
    mutate(year = as.integer(year))

  message("Prepared df_final")

  if (save){
    write_csv(df_final, paste0(PATHS$data_prepared, "ffa_data.csv"))
    message("Saved df_final")
  }

  return(df_final)
}











