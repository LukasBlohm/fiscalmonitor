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

      selectInput(ns("level"),
                  "Level",
                  choices = c("can", "mun"),
                  selected = "can"),
      selectInput(ns("cat1"),
                  "Category 1",
                  choices = unique(.GlobalEnv$df_base$cat1),
                  selected = "rev"),
      selectInput(ns("cat2"),
                  "Category 2",
                  choices = unique(.GlobalEnv$df_base$cat2),
                  selected = "total"),
      selectInput(ns("unit"),
                  "Unit",
                  choices = c("mio", "CHF"),
                  selected = "mio")
      ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Result of per canton regression of selected variable on year"),
      plotOutput(ns("plot"))
      )
    )

}

#' mod_reg Server Functions
#'
#' @noRd
mod_reg_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    observe({
      updateSelectInput(session, inputId = "cat2",
                        choices = df_var_structure %>%
                          filter(level == input$level) %>%
                          filter(cat1 == input$cat1) %>%
                          filter(unit == input$unit) %>%
                          pull(cat2))
    })

    observe({
      updateSelectInput(session, inputId = "unit",
                        choices = df_var_structure %>%
                          filter(level == input$level) %>%
                          filter(cat1 == input$cat1) %>%
                          filter(cat2 == input$cat2) %>%
                          pull(unit))
    })

    df_filtered <- reactive(
      filter_df(.GlobalEnv$df_base, input) %>%
        select(-canton_marked)
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









