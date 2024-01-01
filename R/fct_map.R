#' Title
#'
#' @param df Data frame
#' @param input List, storing user inputs
#'
#' @importFrom magrittr %>%
#'
#' @noRd
filter_df_map <- function(df, input) {
  df %>%
    dplyr::filter(year == input$year) %>%
    apply_common_filters(input)
}
