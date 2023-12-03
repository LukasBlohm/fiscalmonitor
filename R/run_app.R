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

  .GlobalEnv$df_base <- read_csv(paste0(PATHS$data_prepared, "ffa_data.csv")) %>%
    pivot_longer(
      -c(canton, year),
      names_to = c("level", "cat1", "cat2", "unit"), names_sep = "_") %>%
    mutate(year = as.integer(year))

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
