#' Title
#'
#' @param df_base Data frame, storing main data
#' @param input List, storing user inputs
#'
#' @importFrom magrittr %>%
#'
#' @return Data frame
#'
#' @noRd
filter_df <- function(df_base, input) {

  # print(paste0("df_base before filter"))
  # print(head(df_base))

  df_plot <- df_base %>%
    apply_common_filters(input) %>%
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
#' @param df_plot Data frame
#' @param input List, storing user inputs
#'
#' @importFrom magrittr %>%
#'
#' @return Plot
#'
#' @noRd
create_plot <- function(df_plot, input) {

  # print(paste0("df in plotting function"))
  # print(head(df_plot))

  v_colors_plot <- c("Other" = "grey")

  if (!is.null(input$canton_selection)) {
    v_colors_plot <- c(.GlobalEnv$v_colors[1:length(input$canton_selection)] %>%
      purrr::set_names(input$canton_selection),
      v_colors_plot)
  }
  # print(v_colors_plot)

  s_unit <- ifelse(
    stringr::str_detect(unique(df_plot$unit), stringr::fixed("mio")), "Mio. ", ""
    )

  plot <- ggplot2::ggplot(df_plot) +
    # Points to carry hover text
    suppressWarnings(
      ggplot2::geom_point(
        mapping = ggplot2::aes(
          year, value, group = canton,
          text = paste0(
            canton, " (", year, ")",
            "<br>", round(value, 1), " ", s_unit, "CHF"
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

  return(plot)
}
