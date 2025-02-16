#' Get_fso_pop_data
#'
#' @param fso_url String, denoting the path to the px sheets
#' @param file String, denoting the name of the px sheet
#' @param df_cantons Data frame, storing canton data (name, abbreviation, etc.)
#' @param save_to String, storing the path where the output should be saved
#'
#' @importFrom magrittr %>%
#'
#' @return Data frame
#'
#' @examples
#' \dontrun{df_population <- get_fso_pop_data(fso_url, file, df_cantons, save = TRUE)}
#' \dontrun{ggplot(df_population, aes(year, pop_count, color = canton))+ geom_line()}
get_fso_pop_data <- function(fso_url, file, df_cantons, save_to = NULL) {

  cli::cli_alert_info("Prepare FSO data")
  cli::cli_process_start("Download of ", file)

  population_px <- pxR::read.px(
    paste0(fso_url, "file=", file), encoding = "cp1252"
    ) %>%
    as.data.frame()

  cli::cli_process_done()

  df_population <- population_px %>%
    dplyr::select(
      canton_name_fso = "Kanton",
      year = "Jahr",
      variable = "Demografische.Komponente",
      nationality = "Staatsangehörigkeit..Kategorie.",
      age = "Alter",
      gender = "Geschlecht",
      value
      ) %>%
    dplyr::filter(variable == "Bestand am 1. Januar") %>%
    dplyr::filter(nationality == "Staatsangehörigkeit (Kategorie) - Total") %>%
    dplyr::filter(gender == "Geschlecht - Total") %>%
    dplyr::filter(age == "Alter - Total") %>%
    # Convert factor to int (recode to character first to avoid error)
    dplyr::mutate(year = as.integer(as.character(year))) %>%
    dplyr::filter(year >= 1990L) %>%
    # Join with cantons (and drop non-canton observations)
    dplyr::right_join(df_cantons, by = "canton_name_fso") %>%
    dplyr::select(canton, year, pop_count = "value") %>%
    dplyr::arrange(year, canton)

  if (!is.null(save_to)){
    readr::write_rds(df_population, save_to)
    cli::cli_alert_info("Saved {.code df_population} in ", save_to)
  }
  return(df_population)
}
















