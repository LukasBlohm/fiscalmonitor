download_data <- function(v_cantons) {
  url_base <- "https://www.efv.admin.ch/dam/efv/en/dokumente/finanzstatistik/daten/fs_ktn"

  dir_raw <- .GlobalEnv$PATHS$data_raw

  if (!dir.exists(dir_raw)) {
    dir.create(dir_raw)
  }

  purrr::walk(
    v_cantons,
    \(canton) {
      cli::cli_process_start("Download data of canton {toupper(canton)}.")
      curl::curl_download(
        glue::glue("{url_base}/ktn_{canton}.xlsx.download.xlsx"),
        destfile = file.path(dir_raw, paste0(canton, "_raw.xlsx"))
      )
      cli::cli_process_done()
      Sys.sleep(1)
    }
  )
  cli::cli_rule()
}
