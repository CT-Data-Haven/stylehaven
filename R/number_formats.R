#' @title Number formatting utilities
#' @description This is a collection of small utilities for formatting numbers, especially for use in charts and tables. Many of them are simply wrappers around `scales` functions.
#' @param x A numeric vector
#' @param ... Additional arguments to pass to underlying `scales` functions, such as `scales::percent` and `scales::number`.
#' @return A string vector of formatted numbers.
#' @details `round100` takes a decimal, such as a percentage, multiplies by 100, and rounds, e.g. `0.251` becomes `"25"`, useful for then pasting with additional text (see examples).
#'
#' `percent100` adds on to `round100` by appending a percent sign, e.g. `0.251` becomes `"25%"`.
#'
#' `percent_lt1` takes an additional step, replacing any values below 1 percent with `"<1%"`, useful for suppressing small numbers.
#' @examples
#' paste("The value is", round100(0.251), "percent.") # outputs "The value is 25 percent."
#'
#' percent100(c(0.25, 0.251, 0.008)) # outputs "25%", "25%", "1%"
#'
#' percent_lt1(c(0.25, 0.251, 0.008)) # outputs "25%", "25%", "<1%"
#' @rdname number_formats
#' @keywords function
#' @export
percent100 <- function(x, ...) {
  scales::percent(x, accuracy = 1, ...)
}

#' @rdname number_formats
#' @export
percent_lt1 <- function(x, ...) {
  ifelse(x < 0.01, "<1%", percent100(x, ...))
}

#' @rdname number_formats
#' @export
round100 <- function(x, ...) {
  scales::number(x, accuracy = 1, scale = 100, ...)
}
