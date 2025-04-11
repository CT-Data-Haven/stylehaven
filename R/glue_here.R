#' @title Parameterized, project-based file paths
#' @description Quick wrapper around `stringr::str_glue` and `here::here` to fill a small recurring need: filling in file paths based on some parameter, but building them relative to the project root.
#' @param x Character vector of paths, relative to the project root, with glue-style encodings.
#' @param ... Additional named arguments to fill into glue brackets
#' @param .snake Boolean, whether text should also be converted to snakecase. Default: TRUE
#' @return A character vector of absolute paths with glue strings filled in.
#' @examples 
#' year <- 2025
#' loc <- "New Haven"
#' glue_here(c("input_data/demo/{loc}_table_{year}.csv",
#'             "{year}_headings.txt"))
#' # --> projectdir/input_data/demo/new_haven_table_2025.csv,
#' #       projectdir/2025_headings.txt
#' glue_here("{year}_headings_{topic}.txt", topic = "education")
#' # --> projectdir/2025_headings_education.txt
#' @export 
#' @seealso [here::here()] [stringr::str_glue()]
glue_here <- function(x, ..., .snake = TRUE) {
  x <- purrr::map(x, function(x0) {
    glue_here_once_(x0, ..., .snake = .snake, .envir = .GlobalEnv)
  })
  if (is.list(x)) {
    x <- unlist(x)
  }
  x
}

glue_here_once_ <- function(x, ..., .snake, .envir) {
  if (length(x) > 1) {
    cli::cli_abort("{.arg x} should be length 1")
  }
  x <- stringr::str_glue(x, ..., .envir = .envir)
  if (.snake) {
    x <- stringr::str_replace_all(tolower(x), "[\\-\\s\\+]+", "_")
    x <- stringr::str_remove_all(x, "[\\']")
  }
  x <- here::here(x)
  x
}
