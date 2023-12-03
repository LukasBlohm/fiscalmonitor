#' Get_fso_pop_data
#'
#' @param fso_url String, denoting the path to the px sheets
#' @param file String, denoting the name of the px sheet
#' @param df_cantons Data frame, storing canton data (name, abbreviation, etc.)
#' @param save Logical, should the data be saved (under data_prepared/)?
#'
#' @return Data frame
#'
#' @examples
#' \dontrun{df_population <- get_fso_pop_data(fso_url, file, df_cantons, save = TRUE)}
#' \dontrun{ggplot(df_population, aes(year, pop_count, color = canton))+ geom_line()}
get_fso_pop_data <- function(fso_url, file, df_cantons, save = FALSE) {

  population_px <- pxR::read.px(
    paste0(fso_url, "file=", file), encoding = "cp1252"
    ) %>%
    as.data.frame()

  df_population <- population_px %>%
    select(canton_name_fso = "Kanton",
           year = "Jahr",
           variable = "Demografische.Komponente",
           nationality = "Staatsangehörigkeit..Kategorie.",
           age = "Alter",
           gender = "Geschlecht",
           value) %>%
    filter(variable == "Bestand am 1. Januar") %>%
    filter(nationality == "Staatsangehörigkeit (Kategorie) - Total") %>%
    filter(gender == "Geschlecht - Total") %>%
    filter(age == "Alter - Total") %>%
    # Convert factor
    mutate(year = as.integer(as.character(year))) %>%
    filter(year >= 1990) %>%
    # Join with cantons (and drop non-canton observations)
    right_join(df_cantons, by = "canton_name_fso") %>%
    select(canton, year, pop_count = "value") %>%
    arrange(year, canton)

  if (save){
    path <- paste0(PATHS$data_prepared, "fso_pop_data.csv")
    write_csv(df_population, path)
    message("Saved ", path)
  }
  return(df_population)
}
















