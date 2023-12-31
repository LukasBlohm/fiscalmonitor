#' mod_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_map_ui <- function(id){
  ns <- shiny::NS(id)

  bslib::page_sidebar(

    sidebar = bslib::sidebar(

      shiny::sliderInput(ns("year"),
                  "Year",
                  min = min(.GlobalEnv$df_base$year),
                  max = max(.GlobalEnv$df_base$year),
                  value = max(.GlobalEnv$df_base$year),
                  step = 1,
                  round = TRUE,
                  sep = ""),

      shiny::selectInput(ns("level"),
                  "Level",
                  choices = c("can", "mun"),
                  selected = "can"),
      shiny::selectInput(ns("cat1"),
                  "Category 1",
                  choices = c("rev", "exp", "debt"),
                  selected = "rev"),
      shiny::selectInput(ns("cat2"),
                  "Category 2",
                  choices = c("total", "int", "net", "gross", "agg", "pc"),
                  selected = "total"),
      shiny::selectInput(ns("unit"),
                  "Unit",
                  choices = c("mio", "CHF"),
                  selected = "mio")
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Map"),
      shiny::plotOutput(ns("plot_map"))
    )
  )
}

#' mod_map Server Functions
#'
#' @noRd
mod_map_server <- function(id){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    shiny::observe({
      shiny::updateSelectInput(
        session, inputId = "cat2",
        choices = df_var_structure %>%
          dplyr::filter(federal_level == input$level) %>%
          dplyr::filter(cat1 == input$cat1) %>%
          dplyr::filter(unit == input$unit) %>%
          dplyr::pull(cat2)
        )
    })

    shiny::observe({
      shiny::updateSelectInput(session, inputId = "unit",
                               choices = df_var_structure %>%
                                 dplyr::filter(federal_level == input$level) %>%
                                 dplyr::filter(cat1 == input$cat1) %>%
                                 dplyr::filter(cat2 == input$cat2) %>%
                                 dplyr::pull(unit))
    })

    df_plot <- shiny::reactive(filter_df_map(.GlobalEnv$df_base, input))


    output$plot_map <- renderPlot({

      # print(head(df_plot()))

      v_data <- df_plot()$value %>% purrr::set_names(df_plot()$canton)

      min_indicator <- round(
        min(v_data),  -(stringr::str_length(
          stringr::str_extract(max(v_data), "[0-9]+")) + 1)
        )
      max_indicator <- round(
        max(v_data),
        -(stringr::str_length(stringr::str_extract(min(v_data), "[0-9]+")) + 1)
        )

      v_colors <- grDevices::colorRampPalette(c("white", DescTools::hred))(100)

      bfsMaps::PlotKant(id = names(v_data),
               col = DescTools::FindColor(v_data, cols = v_colors),
               main = NULL)

      DescTools::ColorLegend(
        x = "left", # horiz = FALSE,
        inset = -0.01, cols = v_colors,
        labels = formatC((
          seq(min_indicator, max_indicator,
              (max_indicator - min_indicator) / 5)),
          digits = 0, format="f"),
        width = 12000, frame = "grey", cex = 0.8
      )

      # Capture the plot
      captured_plot <- grDevices::recordPlot()

      # Close the graphics device
      dev.off()

      captured_plot
    })
  })
}









