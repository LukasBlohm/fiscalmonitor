#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {

  source("R/app_global.R")

  if (file.exists(.GlobalEnv$PATHS$final_dataset)) {
    .GlobalEnv$df_base <- readr::read_rds(.GlobalEnv$PATHS$final_dataset)
  } else {
    .GlobalEnv$df_base <- prepare_full_data(
      df_cantons, save_to = .GlobalEnv$PATHS$final_dataset
      )
  }

  .GlobalEnv$df_var_structure <- df_base %>%
    dplyr::select(federal_level, cat1, cat2, unit) %>%
    unique()


  golem::with_golem_options(
    app = shiny::shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
