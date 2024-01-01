#' mod_reg_ui UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_reg_ui <- function(id){
  ns <- NS(id)

  bslib::page_sidebar(

    sidebar = bslib::sidebar(

      # level_selector(ns),
      cat1_selector(ns),
      cat2_selector(ns),
      unit_selector(ns)
      ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Koeffizienten und Standardfehler pro Kanton\nder Regression der selektierten Variable auf das Jahr"),
      #bslib::card_header("Result of per canton regression of selected variable on year"),
      shiny::plotOutput(ns("plot"))
      )
    )

}

#' mod_reg Server Functions
#'
#' @noRd
mod_reg_server <- function(id){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns


    shiny::observe({
      update_cat2(session, input)
    })

    df_filtered <- shiny::reactive(
      apply_common_filters(.GlobalEnv$df_base, input)
      )

    output$plot <- renderPlot({

      df_reg <- df_filtered() %>%
        tidyr::nest(data = -canton) %>%
        dplyr::mutate(
          model = purrr::map(data, ~ lm(value ~ year, data = .)),
          tidied = purrr::map(model, ~broom::tidy(.))
          ) %>%
        tidyr::unnest(tidied) %>%
        dplyr::filter(term == "year") %>%
        dplyr::arrange(estimate)

      # Plot coefficients for year
      modelsummary::modelplot(
        models = purrr::set_names(df_reg$model, df_reg$canton),
        coef_omit = 'Interc',
        coef_rename = c("year" = ""),
        facet = TRUE
        )
    })
  })
}









