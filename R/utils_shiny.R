

year_slider <- function(ns, ...) {
  shiny::sliderInput(
    ns("year"),
    "Jahr",
    min = min(.GlobalEnv$df_base$year),
    max = max(.GlobalEnv$df_base$year),
    value = ...,
    step = 1,
    round = TRUE,
    sep = ""
  )
}

canton_selector <- function(ns) {
  shiny::selectInput(
    ns("canton_selection"),
    "Kanton hervorheben",
    choices = unique(.GlobalEnv$df_cantons$canton),
    selected = NULL,
    multiple = TRUE
  )
}

level_selector <- function(ns) {
  shiny::selectInput(
    ns("level"),
    "Level",
    choices = c("can", "mun"),
    selected = "can"
  )
}

cat1_selector <- function(ns) {
  shiny::selectInput(
    ns("cat1"),
    "Oberkategorie",
    choices = c("Einnahmen" = "rev",
                "Ausgaben" = "exp",
                "Bilanz" = "balance"),
    # choices = unique(.GlobalEnv$df_base$cat1),
    selected = "rev"
  )
}

cat2_selector <- function(ns) {
  shiny::selectInput(
    ns("cat2"),
    "Unterkategorie",
    choices = NULL
  )
}

unit_selector <- function(ns) {
  shiny::selectInput(
    ns("unit"),
    "Einheit",
    # choices = unique(.GlobalEnv$df_base$unit),
    choices = c("Pro Kopf (CHF)" = "pc_chf",
                "Total (Mio. CHF)" = "agg_miochf"),
    selected = "pc_chf"
  )
}




# Updates -----------------------------------------------------------------


#' Title
#'
#' @param session Internal shiny variable
#' @param input List, storing user inputs
#'
#' @importFrom magrittr %>%
#'
#' @noRd
update_cat2_selector <- function(session, input) {
  shiny::updateSelectInput(
    session, inputId = "cat2",
    choices = .GlobalEnv$df_var_structure %>%
      # dplyr::filter(federal_level == input$level) %>%
      dplyr::filter(cat1 == input$cat1) %>%
      dplyr::filter(unit == shiny::isolate(input$unit)) %>%
      dplyr::pull(cat2) %>%
      # Get labels to display
      purrr::set_names(get(paste0("v_", input$cat1, "_name_mapping")))
    )
}









