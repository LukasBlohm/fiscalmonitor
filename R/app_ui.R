#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'
#' @noRd
app_ui <- function(request) {

  htmltools::tagList(

    # golem_add_external_resources(),
    bslib::page_navbar(

      theme = bslib::bs_theme(version = 5, bootswatch = "lux"),

      title = "Fiscal Monitor",

      bslib::nav_panel(
        title = "Zeitreihe",
        mod_ts_ui(id = "timeseries")
      ),

      bslib::nav_panel(
        title = "Karte",
        mod_map_ui(id = "map")
      ),

      bslib::nav_panel(
        title = "Regressionen",
        mod_reg_ui(id = "reg")
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
      app_title = "Kantonale Finanzen"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
