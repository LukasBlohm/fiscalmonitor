#" Title
#"
#" @param canton_abb
#" @param category
#"
#" @return
#" @export
#"
#" @examples
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





#### Debt ####


#" Title
#"
#" @param category
#" @param level
#"
#" @return
#" @export
#"
#" @examples
prepare_ffa_debt <- function(category, level, df_cantons) {

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

  message("Prepared df_debt")

  return(df_debt)
}






#" Title
#"
#" @param cantons
#"
#" @return
#" @export
#"
#" @examples
prepare_rev_exp <- function(df_cantons) {

  df_revenues <- map(
    df_cantons$canton,  ~prepare_canton(canton_abb = .x, category = "rev")
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

prepare_debt <- function(df_cantons) {

  ## Debt (FFA debt tables)
  df_debt <- map2(rep(c("agg", "pc"), 2),
                  rep(c("cantonal", "municipal"), each = 2),
                  ~prepare_ffa_debt(category = .x, level = .y, df_cantons)) %>%
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




#### Load full Dataset ####

#" Title
#"
#" @param df_cantons Data frame, storing canton data (name, abbreviation, etc.)
#" @param save Logical, should the data be saved (under data_prepared/)?
#"
#" @return
#" @export
#"
#" @examples
prepare_full_ffa_data <- function(df_cantons, save = FALSE)  {

  # df_cantons <- df_cantons %>%
  #   select(canton_nr, canton, canton_name)

  df_rev_exp <- prepare_rev_exp(df_cantons)
  message("------------------------------------------")
  df_debt <- prepare_debt(df_cantons)
  message("------------------------------------------")
  df_balance <- prepare_balance(df_cantons)
  message("------------------------------------------")

  df_final <- df_rev_exp %>%
    left_join(df_balance, by = c("canton", "year")) %>%
    full_join(df_debt, by = c("canton", "year")) %>%
    arrange(canton, year)

  message("Prepared df_final")

  if (save){
    write_csv(df_final, paste0(PATHS$data_prepared, "ffa_data.csv"))
    message("Saved df_final")
  }

  return(df_final)
}











