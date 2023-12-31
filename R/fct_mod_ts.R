#' Title
#'
#' @param df
#' @param input
#'
#' @importFrom magrittr %>%
#'
#' @return Data frame
#'
#' @noRd
filter_df <- function(df, input) {

  # print(paste0("df before filter"))
  # print(head(df))

  df_plot <- df %>%
    dplyr::filter(federal_level == input$level) %>%
    dplyr::filter(cat1 == input$cat1) %>%
    dplyr::filter(cat2 == input$cat2) %>%
    dplyr::filter(unit == input$unit) %>%
    dplyr::mutate(
      canton_marked = factor(
        dplyr::case_when(
          canton %in% input$canton_selection ~ canton,
          TRUE ~ "Other"
        ), levels = c(unique(.GlobalEnv$df_cantons$canton), "Other")
      )
    )
  return(df_plot)
}


#' Title
#'
#' @param df_plot
#' @param input
#'
#' @importFrom magrittr %>%
#'
#' @return Plot
#'
#' @noRd
create_plot <- function(df_plot, input) {

  # print(paste0("df in plotting function"))
  # print(head(df_plot))

  v_colors_plot <- .GlobalEnv$v_colors[1:length(input$canton_selection)] %>%
    purrr::set_names(input$canton_selection)

  v_colors_plot <- c(v_colors_plot, "Other" = "grey")

  plt <- ggplot2::ggplot(df_plot) +
    # Points to carry hover text
    suppressWarnings(
      ggplot2::geom_point(
        mapping = ggplot2::aes(
          year, value, group = canton,
          text = paste0(
            canton, " (", year, ")",
            "<br>", round(value, 1), " ", stringr::str_to_title(unit), ". CHF"
          )),
        alpha = 0 # hide points
      )
    ) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(year, value, group = canton, color = canton_marked),
      size = 1
      ) +
    ggplot2::scale_color_manual("", values = v_colors_plot) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "bottom")

  return(plt)
}
