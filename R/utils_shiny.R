



canton_selector <- function(ns) {
  shiny::selectInput(ns("canton_selection"),
                     "Highlight Canton",
                     choices = unique(.GlobalEnv$df_cantons$canton),
                     selected = NULL,
                     multiple = TRUE)
}

level_selector <- function(ns) {
  shiny::selectInput(ns("level"),
                     "Level",
                     choices = c("can", "mun"),
                     selected = "can")
}

cat1_selector <- function(ns) {
  shiny::selectInput(ns("cat1"),
                     "Category 1",
                     choices = unique(.GlobalEnv$df_base$cat1),
                     selected = "rev")
}

cat2_selector <- function(ns) {
  shiny::selectInput(ns("cat2"),
                     "Category 2",
                     choices = unique(.GlobalEnv$df_base$cat2),
                     selected = "total")
}

unit_selector <- function(ns) {
  shiny::selectInput(ns("unit"),
                     "Unit",
                     choices = unique(.GlobalEnv$df_base$unit),
                     selected = "pc_chf")
}




# Updates -----------------------------------------------------------------


#' Title
#'
#' @param session
#' @param input
#'
#' @importFrom magrittr %>%
#'
#' @noRd
update_cat2 <- function(session, input) {
  shiny::updateSelectInput(
    session, inputId = "cat2",
    choices = .GlobalEnv$df_var_structure %>%
      dplyr::filter(federal_level == input$level) %>%
      dplyr::filter(cat1 == input$cat1) %>%
      dplyr::filter(unit == shiny::isolate(input$unit)) %>%
      dplyr::pull(cat2)
    )
}

#' #' Title
#' #'
#' #' @param session
#' #' @param input
#' #'
#' #' @importFrom magrittr %>%
#' #'
#' #' @noRd
#' update_unit <- function(session, input) {
#'   shiny::updateSelectInput(
#'     session, inputId = "unit",
#'     choices = df_var_structure %>%
#'       dplyr::filter(federal_level == input$level) %>%
#'       dplyr::filter(cat1 == input$cat1) %>%
#'       dplyr::filter(cat2 == input$cat2) %>%
#'       dplyr::pull(unit)
#'   )
#' }


