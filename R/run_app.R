#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {

  source("R/app_global.R")

  if (file.exists(paste0(PATHS$data_prepared, "ffa_data.csv"))) {
    .GlobalEnv$df_base <- read_csv(paste0(PATHS$data_prepared, "ffa_data.csv"))
  } else {
    .GlobalEnv$df_base <- prepare_full_ffa_data(df_cantons, save = TRUE)
  }

  .GlobalEnv$df_var_structure <- df_base %>%
    select(level, cat1, cat2, unit) %>%
    unique()


  with_golem_options(
    app = shinyApp(
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
