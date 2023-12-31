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



#' prepare_debt
#'
#' @param df_cantons Data frame, storing canton data (name, abbreviation, etc.)
#'
#' @importFrom magrittr %>%
#'
#' @return Data frame
prepare_debt <- function(df_cantons) {

  ## Debt (FFA debt tables)
  df_debt <- purrr::map2(
    rep(c("agg", "pc"), 2), rep(c("cantonal", "municipal"), each = 2),
    ~prepare_debt_indicator(
      category_selected = .x, federal_level_selected = .y, df_cantons
    )
  ) %>%
    dplyr::bind_rows() %>%
    tidyr::pivot_wider(
      names_from = c("federal_level", "category"), values_from = "chf"
    ) %>%
    dplyr::mutate(across(tidyselect::contains("agg"), ~ . / 1000000)) %>%   # From CHF to mio. CHF
    dplyr::select(
      canton, year,
      can_debt_agg_mio = "cantonal_agg",
      mun_debt_agg_mio = "municipal_agg",
      can_debt_pc_chf = "cantonal_pc",
      mun_debt_pc_chf = "municipal_pc"
    )

  message("Prepared df_debt")

  return(df_debt)
}
