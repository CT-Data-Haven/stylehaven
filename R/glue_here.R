#' @title Parameterized, project-based file paths
#' @description Quick wrapper around `stringr::str_glue` and `here::here` to fill a small recurring need: filling in file paths based on some parameter, but building them relative to the project root.
#' @param x Character vector of paths, relative to the project root, with glue-style encodings.
#' @param ... Additional named arguments to fill into glue brackets
#' @param .snake Boolean, whether text should also be converted to snakecase. Default: TRUE
#' @param .envir Environment to be passed down `stringr::str_glue`. Probably don't need to worry about it.
#' @return A character vector of absolute paths with glue strings filled in.
#' @examples
#' glue_here("{year}_headings_{topic}.txt", year = 2025, topic = "education")
#'
#' # in a regular script you shouldn't need to worry about setting .envir
#' # but scoping is weird for package examples
#' # year <- 2025
#' # loc <- "New Haven"
#' env <- rlang::env(year = 2025, loc = "New Haven")
#' glue_here(c(
#'     "input_data/demo/{loc}_table_{year}.csv",
#'     "{year}_headings.txt"
#' ), .envir = env)
#'
#' @export
#' @keywords misc
#' @seealso [here::here()] [stringr::str_glue()]
glue_here <- function(x, ..., .snake = TRUE, .envir = environment()) {
    x <- purrr::map(x, function(x0) {
        glue_here_once_(x0, ..., .snake = .snake, .envir = .envir)
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
