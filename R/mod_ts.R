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
      selectInput(ns("canton_selection"),
                  "Highlight Canton",
                  choices = unique(.GlobalEnv$df_cantons$canton),
                  selected = NULL,
                  multiple = TRUE),

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
      bslib::card_header("Time Series"),
      plotly::plotlyOutput(ns("plot"))
      )
    )

}

#' ts Server Functions
#'
#' @noRd
mod_ts_server <- function(id){
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

    df_plot <- reactive(filter_df(.GlobalEnv$df_base, input))


    output$plot <- plotly::renderPlotly({

      create_plot(df_plot(), input) %>%
        plotly::ggplotly(
          tooltip = "text"
        ) %>%
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









