#' Title
#'
#' @param df Data frame
#' @param input List, storing user inputs
#'
#' @noRd
apply_common_filters <- function(df, input) {
  dplyr::filter(
    df,
    # federal_level == input$level
    cat1 == input$cat1,
    cat2 == input$cat2,
    unit == input$unit
  )
}


#' generate_vector_code
#'
#' Generate code to reproduce a vector in the environment. This can also
#' be executed with `eval(parse())`.
#'
#' @param v Input vector
#' @param class String, either ´numeric´ or `character´
#'
#' @return The code to create the vector
#'
#' @examples
#' \dontrun{vector_code <- generate_vector_code(v_character, class = "character")}
#' \dontrun{eval(parse(vector_code))}
generate_vector_code <- function(v, class = "numeric") {
  # Convert the vector to a string in a way that can be executed as R code
  if (class == "numeric") {
    vector_code <- paste("c(", paste(v, collapse = ", "), ")", sep = "")
  } else if (class == "character") {
    vector_code <- paste("c('", paste(v, collapse = "', '"), "')", sep = "")
  }
  return(vector_code)
}

# # Example vector
# my_vector <- c(1, 2, 3, 4, 5)
#
# # Get the code to recreate this vector
# vector_code <- generate_vector_code(sort(as.character(unique(population_df1$canton))),
#                                     class = "character")
#
# # Print the generated code
# print(vector_code)
#
# new_vector <- eval(parse(text=vector_code))

