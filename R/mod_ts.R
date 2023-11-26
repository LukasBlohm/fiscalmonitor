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
  list(

    sidebar = sidebarPanel(
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
      plotOutput(ns("plot"))
      )
    )

}

#' ts Server Functions
#'
#' @noRd
mod_ts_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    df <- read_csv(paste0(PATHS$data_prepared, "ffa_data.csv")) %>%
      pivot_longer(
        -c(canton, year),
        names_to = c("level", "cat1", "cat2", "unit"), names_sep = "_")

    df_plot <- reactive(filter_df(df, input))


    output$plot <- renderPlot({

      create_plot(df_plot(), input)

    })
  })
}









