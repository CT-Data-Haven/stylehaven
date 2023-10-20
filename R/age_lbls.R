#' @title Clean up age group labels
#' @description This function does some tedious regex replacement to make common labels we use for age groups (i.e. syntactically correct variable names) to labels meant for display in charts and tables. The main features are replacing underscores between numbers with dashes, adding space between text and numbers, replacing symbols, and parsing numbers. So "ages00_17" becomes "Ages 0-17", and "ages65plus" becomes "Ages 65+". Of course, you could also use this for other numeric ranges, like years.
#' @param x A string vector of labels to clean up
#' @return A string vector of display-worthy labels
#' @examples 
#' a <- c("ages00_17", "ages18_64", "ages65plus", "under18", "ages18up")
#' age_lbls(a)
#' @export 
age_lbls <- function(x) {
  x <- stringr::str_replace(x, "(?<=\\d)_(?=\\d)", "-")
  x <- stringr::str_replace_all(x, "_", " ")
  x <- stringr::str_replace(x, "(plus|up)$", "+")
  x <- stringr::str_replace_all(x, "(?<=[A-Za-z])\\B(?=\\d)", " ")
  x <- stringr::str_replace_all(x, "(?<=\\d)\\B(?=[A-Za-z])", " ")
  x <- stringr::str_remove(x, "(?<=\\b)0(?=[0-9])")
  x <- stringr::str_to_sentence(x)
  x
}
