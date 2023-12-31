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

      shiny::selectInput(ns("level"),
                  "Level",
                  choices = c("can", "mun"),
                  selected = "can"),
      shiny::selectInput(ns("cat1"),
                  "Category 1",
                  choices = unique(.GlobalEnv$df_base$cat1),
                  selected = "rev"),
      shiny::selectInput(ns("cat2"),
                  "Category 2",
                  choices = unique(.GlobalEnv$df_base$cat2),
                  selected = "total"),
      shiny::selectInput(ns("unit"),
                  "Unit",
                  choices = c("mio", "CHF"),
                  selected = "mio")
      ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Result of per canton regression of selected variable on year"),
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
      shiny::updateSelectInput(session, inputId = "cat2",
                        choices = df_var_structure %>%
                          dplyr::filter(federal_level == input$level) %>%
                          dplyr::filter(cat1 == input$cat1) %>%
                          dplyr::filter(unit == input$unit) %>%
                          dplyr::pull(cat2))
    })

    shiny::observe({
      shiny::updateSelectInput(session, inputId = "unit",
                        choices = df_var_structure %>%
                          dplyr::filter(federal_level == input$level) %>%
                          dplyr::filter(cat1 == input$cat1) %>%
                          dplyr::filter(cat2 == input$cat2) %>%
                          dplyr::pull(unit))
    })

    df_filtered <- shiny::reactive(
      filter_df(.GlobalEnv$df_base, input) %>%
        dplyr::select(-canton_marked)
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









