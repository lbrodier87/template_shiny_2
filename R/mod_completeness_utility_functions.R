#' Create choice options for selectInput
#' 
#' This function takes a data frame as input and creates a named vector with
#' the entry of the vector being the column names and the name of the entry
#' of the vector being the column name + the data type in parenthesis. It is used
#' to generate the value for the argument 'choices' or 'selected' of the 
#' shiny::selectInput() function.
#'
#' @param df 
#'
#' @return named vector
#' @export
#'
#' @examples
prepare_choice_options = function(df) {
  choices = colnames(df)
  df = df %>% mutate_if(is.POSIXct, as_date)
  names(choices) = paste0( colnames(df), " (", map(df, class), ")")
  return(choices)
}

#' Get names of columns with missing values
#' 
#' This function takes a data frame as input and returns a vector with the 
#' name of the columns that contains missing values
#'
#' @param df a dataframe
#' @param sort sort colnames by alphabetical order? Default to TRUE
#' @return character vector
#' @export
#'
#' @examples
get_colnames_missing = function(df, sort = TRUE) {
  vec_colnames_missing = df %>% miss_var_summary() %>% 
    filter(pct_miss > 0) %>% pull(variable) 
  if (sort) {vec_colnames_missing = vec_colnames_missing %>% sort}
  return(vec_colnames_missing)
}