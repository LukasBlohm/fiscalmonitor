



canton_selector <- function(ns) {
  shiny::selectInput(
    ns("canton_selection"),
    "Kanton hervorheben",
    choices = unique(.GlobalEnv$df_cantons$canton),
    selected = NULL,
    multiple = TRUE
  )
}

level_selector <- function(ns) {
  shiny::selectInput(
    ns("level"),
    "Level",
    choices = c("can", "mun"),
    selected = "can"
  )
}

cat1_selector <- function(ns) {
  shiny::selectInput(
    ns("cat1"),
    "Oberkategorie",
    choices = c("Einnahmen" = "rev",
                "Ausgaben" = "exp",
                "Bilanz" = "balance"),
    # choices = unique(.GlobalEnv$df_base$cat1),
    selected = "rev"
  )
}

cat2_selector <- function(ns) {
  shiny::selectInput(
    ns("cat2"),
    "Unterkategorie",
    choices = NULL
  )
}

unit_selector <- function(ns) {
  shiny::selectInput(
    ns("unit"),
    "Einheit",
    # choices = unique(.GlobalEnv$df_base$unit),
    choices = c("Pro Kopf (CHF)" = "pc_chf",
                "Total (Mio. CHF)" = "agg_miochf"),
    selected = "pc_chf"
  )
}




# Updates -----------------------------------------------------------------


#' Title
#'
#' @param session
#' @param input
#'
#' @importFrom magrittr %>%
#'
#' @noRd
update_cat2 <- function(session, input) {
  shiny::updateSelectInput(
    session, inputId = "cat2",
    choices = .GlobalEnv$df_var_structure %>%
      # dplyr::filter(federal_level == input$level) %>%
      dplyr::filter(cat1 == input$cat1) %>%
      dplyr::filter(unit == shiny::isolate(input$unit)) %>%
      dplyr::pull(cat2) %>%
      rename_cat2(cat1 = input$cat1)
    )
}


rename_cat2 <- function(x, cat1) {
  if (cat1 == "rev") {
    purrr::set_names(
      x,
      c("total" = "Gesamteinnahmen",
      "fiscal" = "Fiskaleinnahmen",
      "taxinc" = "Einkommenssteuern natürliche Personen",
      "taxwealth" = "Vermögenssteuern natürliche Personen",
      "taxprofit" = "Gewinnsteuern juristische Personen",
      "taxcap" = "Kapitalsteuern juristische Personen",
      "int" = "Zinseinnahmen",
      "transfer" = "Transfereinnahmen",
      "fla" = "Finanz- und Lastenausgleich",
      "invest" = "Investitionsbeiträge")
    )
  } else if (cat1 == "exp") {
    purrr::set_names(
      x,
      c("total" = "Gesamtausgaben",
        "admin" = "Allgemeine Verwaltung",
        "security" = "Öffentliche Ordnung und Sicherheit, Verteidigung",
        "education" = "Bildung",
        "culture" = "Kultur, Sport und Freizeit, Kirche",
        "health" = "Gesundheit",
        "socsecurity" = "Soziale Sicherheit",
        "traffic" = "Verkehr und Nachrichtenübermittlung",
        "environment" = "Umweltschutz und Raumordnung",
        "economy" = "Volkswirtschaft",
        "finance" = "Finanzen und Steuern")
    )
  } else if (cat1 == "balance") {
    purrr::set_names(
      x,
      c("active" = "Aktiven",
        "assetsfinance" = "Finanzvermögen",
        "assetsadmin" = "Verwaltungsvermögen",
        "passive" = "Passiven",
        "liabilities" = "Fremdkapital",
        "liabilitiesshort" = "Kurzfristige Finanzverbindlichkeiten",
        "liabilitieslong" = "Langfristige Finanzverbindlichkeiten",
        "equity" = "Eigenkapital")
    )
  }
}










