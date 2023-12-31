#' my_ts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ts_ui <- function(id){
  ns <- NS(id)

  bslib::page_sidebar(

    sidebar = bslib::sidebar(
      shiny::selectInput(ns("canton_selection"),
                  "Highlight Canton",
                  choices = unique(.GlobalEnv$df_cantons$canton),
                  selected = NULL,
                  multiple = TRUE),

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
                  choices = unique(.GlobalEnv$df_base$unit),
                  selected = "pc_chf")
      ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Time Series"),
      plotly::plotlyOutput(ns("plot"))
      )
    )

}

#' ts Server Functions
#'
#' @noRd
mod_ts_server <- function(id){
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

    df_plot <- shiny::reactive(filter_df(.GlobalEnv$df_base, input))


    output$plot <- plotly::renderPlotly({

      shiny::req(input$cat2)
      shiny::req(input$unit)
      shiny::req(df_plot())

      create_plot(df_plot(), input) %>%
        plotly::ggplotly(tooltip = "text") %>%
        plotly::config(modeBarButtonsToRemove = c(
          # "zoomIn2d", "zoomOut2d", ""resetScale2d"
          "zoom2d", "lasso2d", "pan2d", "box2d", "select2d", "autoScale2d",
          "hoverClosestCartesian", "hoverCompareCartesian"
        )) %>%
        plotly::layout(
          legend = list(orientation = "h", x = 0.5, xanchor = "center")
        )
    })
  })
}









