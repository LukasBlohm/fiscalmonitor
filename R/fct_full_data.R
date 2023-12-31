
#### Prepare full Dataset ####

#' prepare_full_data
#'
#' @param df_cantons Data frame, storing canton data (name, abbreviation, etc.)
#' @param save_to String, storing the path where the output should be saved
#'
#' @importFrom magrittr %>%
#'
#' @return Data frame of the fiscal data
#' @export
#'
#' @examples
#' \dontrun{df <- prepare_full_ffa_data(df_cantons)}
prepare_full_data <- function(df_cantons, save_to = NULL)  {

  message("Prepare FFA data")

  if (file.exists(.GlobalEnv$PATHS$ffa_rev_exp)) {
    df_rev_exp <- readr::read_rds(.GlobalEnv$PATHS$ffa_rev_exp)
  } else {
    df_rev_exp <- prepare_rev_exp(
      df_cantons, save_to = .GlobalEnv$PATHS$ffa_rev_exp
      )
    message("------------------------------------------")
  }

  # df_debt <- prepare_debt(df_cantons)
  # message("------------------------------------------")

  if (file.exists(.GlobalEnv$PATHS$ffa_balance)) {
    df_balance <- readr::read_rds(.GlobalEnv$PATHS$ffa_balance)
  } else {
    df_balance <- prepare_balance(
      df_cantons, save_to = .GlobalEnv$PATHS$ffa_balance
      )
    message("------------------------------------------")
  }


  # Check for saved FSO population data; download, prepare and save if not found
  if (file.exists(.GlobalEnv$PATHS$fso_population)) {
    df_fso <- readr::read_rds(.GlobalEnv$PATHS$fso_population)
  } else {
    df_fso <- get_fso_pop_data(
      fso_url, file = .GlobalEnv$PATHS$fso_pop_file, df_cantons,
      save_to = .GlobalEnv$PATHS$fso_population)
  }


  df_final <- df_rev_exp %>%
    dplyr::left_join(df_balance, by = c("canton", "year")) %>%
    # dplyr::full_join(df_debt, by = c("canton", "year")) %>%
    dplyr::arrange(canton, year) %>%
    tidyr::pivot_longer(
      -c(canton, year),
      names_to = c("federal_level", "cat1", "cat2", "unit"), names_sep = "_",
      values_to = "agg_miochf"
    ) %>%
    dplyr::select(-unit) %>%
    dplyr::left_join(df_fso, by = dplyr::join_by(canton, year)) %>%
    dplyr::mutate(year = as.integer(year),
                  pc_chf = agg_miochf / pop_count * 1000000) %>%
    dplyr::select(-pop_count) %>%
    tidyr::pivot_longer(
      c(pc_chf, agg_miochf),
      names_to = "unit", values_to = "value"
      )

  message("Prepared df_final")

  if (!is.null(save_to)){
    readr::write_rds(df_final, save_to)
    message("Saved df_final in ", save_to)
  }
  return(df_final)
}











