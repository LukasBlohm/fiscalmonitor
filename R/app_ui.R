#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      h1("Fiscal Monitor"),

      tabsetPanel(
        tabPanel(
          title = "TS",
          mod_ts_ui(id = "timeseries")$sidebar,
          mod_ts_ui(id = "timeseries")$main
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#'
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Fiscal Monitor"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
