filter_df_map <- function(df, input) {

  # print(paste0("df before filter"))
  # print(head(df))

  df_plot <- df %>%
    filter(year == input$year) %>%
    filter(level == input$level) %>%
    filter(cat1 == input$cat1) %>%
    filter(cat2 == input$cat2) %>%
    filter(unit == input$unit)

  return(df_plot)
}
