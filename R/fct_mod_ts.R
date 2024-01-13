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
          TRUE ~ "Andere"
        ), levels = c("Andere", unique(.GlobalEnv$df_cantons$canton))
      )
    ) %>%
    { if (input$norm)
      dplyr::mutate(
        ., value = value / value[year == input$year] * 100,
        .by = canton) else . }

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

  v_colors_plot <- c("Andere" = "grey")

  if (!is.null(input$canton_selection)) {
    v_colors_plot <- c(.GlobalEnv$v_colors[1:length(input$canton_selection)] %>%
      purrr::set_names(input$canton_selection),
      v_colors_plot)
  }
  # print(v_colors_plot)


  s_unit <- ifelse(
    stringr::str_detect(unique(df_plot$unit), stringr::fixed("mio")), "Mio. ", ""
    )

  if (input$norm) {

    scale_factor <- 10
    limits = c(floor(min(df_plot$value) / scale_factor) * scale_factor,
               ceiling(max(df_plot$value) / scale_factor) * scale_factor)
  }


  plot <- ggplot2::ggplot(df_plot) +
    # Points to carry hover text
    suppressWarnings(
      ggplot2::geom_point(
        mapping = ggplot2::aes(
          year, value, group = canton,
          text = paste0(
            canton, " (", year, ")",
            "<br>",
            ifelse(!input$norm, round(value, 1), round(value, 1) - 100), " ",
            ifelse(!input$norm,
                   paste0(s_unit, "CHF"),
                   paste("% Wachstum seit", input$year))
          )),
        alpha = 0 # hide points
      )
    ) +
    conditional_colored_lines(input) +
    conditional_point(input) +
    conditional_yscale(input, limits = limits) +
    conditional_color_scale(input, v_colors_plot) +
    ggplot2::labs(
      x = NULL, y = NULL,
      title = paste(
        get(paste0("v_", input$cat1, "_name_mapping"))[[input$cat2]],
        "\n", ifelse(input$norm, paste("(Index:", input$year, "= 100)"), "")
        )
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "bottom")

  return(plot)
}


conditional_colored_lines <- function(input) {

  if (length(input$canton_selection) == 0) {
    ggplot2::geom_line(
      mapping = ggplot2::aes(year, value, group = canton),
      size = 1, color = "navy"
    )
  } else {
    ggplot2::geom_line(
      mapping = ggplot2::aes(year, value, group = canton, color = canton_marked),
      size = 1
    )
  }
}


conditional_point <- function(input) {

  if (input$norm) {
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        input$year, 100
      ),
      size = 3, shape = 21, color = "black", fill = "white"
    )
  }
}


conditional_color_scale <- function(input, v_colors_plot) {
    if (length(input$canton_selection) > 0) {
      ggplot2::scale_color_manual("", values = v_colors_plot)
    }
  }


conditional_yscale <- function(input, ...) {
  if (input$norm) {
    ggplot2::scale_y_continuous(
      limits = ...
      # ,
      # labels = scales::label_percent(scale = 1)
      )
  } else {
    ggplot2::scale_y_continuous(
      labels = scales::label_number(scale = 1, suffix = "k")
    )
  }
}



