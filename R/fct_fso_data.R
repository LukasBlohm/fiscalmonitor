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

  population_px <- pxR::read.px(
    paste0(fso_url, "file=", file), encoding = "cp1252"
    ) %>%
    as.data.frame()

  df_population <- population_px %>%
    dplyr::select(canton_name_fso = "Kanton",
           year = "Jahr",
           variable = "Demografische.Komponente",
           nationality = "Staatsangehörigkeit..Kategorie.",
           age = "Alter",
           gender = "Geschlecht",
           value) %>%
    dplyr::filter(variable == "Bestand am 1. Januar") %>%
    dplyr::filter(nationality == "Staatsangehörigkeit (Kategorie) - Total") %>%
    dplyr::filter(gender == "Geschlecht - Total") %>%
    dplyr::filter(age == "Alter - Total") %>%
    # Convert factor
    dplyr::mutate(year = as.integer(as.character(year))) %>%
    dplyr::filter(year >= 1990) %>%
    # Join with cantons (and drop non-canton observations)
    dplyr::right_join(df_cantons, by = "canton_name_fso") %>%
    dplyr::select(canton, year, pop_count = "value") %>%
    dplyr::arrange(year, canton)

  # if (save){
  #   path <- paste0(PATHS$data_prepared, "fso_pop_data.csv")
  #   readr::write_csv(df_population, path)
  #   message("Saved ", path)
  # }
  if (!is.null(save_to)){
    readr::write_rds(df_population, save_to)
    message("Saved df_population in", save_to)
  }
  return(df_population)
}
















