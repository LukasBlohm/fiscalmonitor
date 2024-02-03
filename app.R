pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = TRUE)
# fiscalmonitor::run_app()

port <- Sys.getenv("PORT")
if (port == "") port <- 3838
options('shiny.port' = as.numeric(port), shiny.host = '0.0.0.0')
