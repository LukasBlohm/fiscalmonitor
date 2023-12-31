filter_df <- function(df, input) {

  # print(paste0("df before filter"))
  # print(head(df))

  df_plot <- df %>%
    filter(level == input$level) %>%
    filter(cat1 == input$cat1) %>%
    filter(cat2 == input$cat2) %>%
    filter(unit == input$unit) %>%
    mutate(canton_marked = factor(case_when(
      canton %in% input$canton_selection ~ canton,
      TRUE ~ "Other"
    ), levels = c(unique(.GlobalEnv$df_cantons$canton), "Other")))

  return(df_plot)
}


create_plot <- function(df_plot, input) {

  # print(paste0("df in plotting function"))
  # print(head(df_plot))

  v_colors_plot <- .GlobalEnv$v_colors[1:length(input$canton_selection)] %>%
    set_names(input$canton_selection)

  v_colors_plot <- c(v_colors_plot, "Other" = "grey")

  plt <- ggplot(df_plot) +
    # Points to carry hover text
    suppressWarnings(
      geom_point(
        mapping = aes(
          year, value, group = canton,
          text = paste0(
            canton, " (", year, ")",
            "<br>", round(value, 1), " ", stringr::str_to_title(unit), ". CHF"
          )),
        alpha = 0 # hide points
      )
    ) +
    geom_line(
      mapping = aes(year, value, group = canton, color = canton_marked),
      size = 1
      ) +
    scale_color_manual("", values = v_colors_plot) +
    labs(x = NULL, y = NULL) +
    theme_classic() +
    theme(legend.position = "bottom")

  return(plt)
}
