#' Title
#'
#' @param df Data frame
#' @param input List
#'
#' @importFrom magrittr %>%
#'
#' @noRd
filter_df_map <- function(df, input) {

  # print(paste0("df before filter"))
  # print(head(df))

  df_plot <- df %>%
    dplyr::filter(year == input$year) %>%
    dplyr::filter(federal_level == input$level) %>%
    dplyr::filter(cat1 == input$cat1) %>%
    dplyr::filter(cat2 == input$cat2) %>%
    dplyr::filter(unit == input$unit)

  return(df_plot)
}
