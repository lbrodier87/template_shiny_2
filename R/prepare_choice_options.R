#' Create choice options for selectInput
#' 
#' This function takes a data frame as input and creates a named vector with
#' the entry of the vector being the column names and the name of the entry
#' of the vector being the column name + the data type in parenthesis
#'
#' @param df 
#'
#' @return named vector
#' @export
#'
#' @examples
prepare_choice_options = function(df) {
  choices = colnames(df)
  names(choices) = paste0( colnames(df), " (", map(df, class), ")")
  return(choices)
}