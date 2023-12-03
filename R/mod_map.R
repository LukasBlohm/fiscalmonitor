#' mod_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_map_ui <- function(id){
  ns <- NS(id)
  list(

    sidebar = sidebarPanel(

      sliderInput(ns("year"),
                  "Year",
                  min = min(.GlobalEnv$df_base$year),
                  max = max(.GlobalEnv$df_base$year),
                  value = max(.GlobalEnv$df_base$year),
                  step = 1,
                  round = TRUE,
                  sep = ""),

      selectInput(ns("level"),
                  "Level",
                  choices = c("can", "mun"),
                  selected = "can"),
      selectInput(ns("cat1"),
                  "Category 1",
                  choices = c("rev", "exp", "debt"),
                  selected = "rev"),
      selectInput(ns("cat2"),
                  "Category 2",
                  choices = c("total", "int", "net", "gross", "agg", "pc"),
                  selected = "total"),
      selectInput(ns("unit"),
                  "Unit",
                  choices = c("mio", "CHF"),
                  selected = "mio")
    ),
    main = mainPanel(
      plotOutput(ns("plot_map"))
    )
  )

}

#' mod_map Server Functions
#'
#' @noRd
mod_map_server <- function(id){
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

    df_plot <- reactive(filter_df_map(.GlobalEnv$df_base, input))


    output$plot_map <- renderPlot({

      v_data <- df_plot()$value %>% set_names(df_plot()$canton)

      # print(min(v_data))
      # print(max(v_data))

      min_indicator <- round(min(v_data),
                             -(str_length(str_extract(max(v_data), "[0-9]+")) + 1))
      max_indicator <- round(max(v_data),
                             -(str_length(str_extract(min(v_data), "[0-9]+")) + 1))

      # print(min_indicator)
      # print(max_indicator)


      # and a color ramp from white to hred
      v_colors <- colorRampPalette(c("white", hred))(100)

      PlotKant(id = names(v_data),
               col = FindColor(v_data, cols = v_colors),
               main = "Cantons")

      ColorLegend(
        x = "left", # horiz = FALSE,
        inset = -0.01, cols = v_colors,
        labels=formatC((seq(min_indicator, max_indicator,
                            (max_indicator - min_indicator) / 5)),
                       digits = 0, format="f"),
        width = 12000, frame = "grey", cex = 0.8
      )

      # Capture the plot
      captured_plot <- recordPlot()

      # Close the graphics device
      dev.off()

      # Later, you can replay this plot
      captured_plot

    })
  })
}









