#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  mod_ts_server("timeseries")
  mod_map_server("map")
}
