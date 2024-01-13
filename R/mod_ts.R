#' my_ts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_ts_ui <- function(id){
  ns <- shiny::NS(id)

  bslib::page_sidebar(

    sidebar = bslib::sidebar(
      canton_selector(ns),
      # level_selector(ns),
      cat1_selector(ns),
      cat2_selector(ns),
      unit_selector(ns),

      shiny::checkboxInput(ns("norm"), "Daten Normalisieren", value = FALSE),
      shiny::conditionalPanel(
        condition = "input.norm",
        ns = ns,
        year_slider(ns, value = median(.GlobalEnv$df_base$year))
        )
      ),
    bslib::card(
      full_screen = TRUE,
      #bslib::card_header(),
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

    shiny::observeEvent(input$cat1, {
      update_cat2_selector(session, input)
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









