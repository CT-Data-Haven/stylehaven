#' @title Number formatting utilities
#' @description This is a collection of small utilities for formatting numbers, especially for use in charts and tables. Many of them are simply wrappers around `scales` functions.
#' @param x A numeric vector
#' @inheritParams scales::label_number
#' @param ... Additional arguments to pass to underlying `scales` functions, such as `scales::label_percent` and `scales::label_number`.
#' @return A string vector of formatted numbers.
#' @details `round100` takes a decimal, such as a percentage, multiplies by 100, and rounds, e.g. `0.251` becomes `"25"`, useful for then pasting with additional text (see examples).
#'
#' `percent100` adds on to `round100` by appending a percent sign, e.g. `0.251` becomes `"25%"`.
#'
#' `percent_lt1` takes an additional step, replacing any values below 1 percent with `"<1%"`, useful for suppressing small numbers.
#' @examples
#' paste("The value is", percent_txt(0.25))
#'
#' percent100(c(0.25, 0.251, 0.008))
#'
#' percent_lt1(c(0.25, 0.251, 0.008))
#'
#' paste(percent_eng(0.1), "of adults say...")
#'
#' dollar1(c(81.1, 105.22))
#'
#' dollark(c(12345, 89100))
#' dollark(c(12345, 89100), accuracy = 0.1)
#' @rdname number_formats
#' @keywords string-formatting
#' @export
round100 <- function(x, ...) {
    num_error(x)
    scales::label_number(accuracy = 1, scale = 100, ...)(x)
}

#' @rdname number_formats
#' @export
comma <- function(x, ...) {
    num_error(x)
    scales::label_comma(...)(x)
}

#' @rdname number_formats
#' @export
percent100 <- function(x, ...) {
    num_error(x)
    scales::label_percent(accuracy = 1, ...)(x)
}

#' @rdname number_formats
#' @export
percent_lt1 <- function(x, ...) {
    num_error(x)
    percent_thresh(x, thresh = 0.01, less_than = TRUE, ...)
}

#' @rdname number_formats
#' @export
percent_txt <- function(x, accuracy = 1, ...) {
    num_error(x)
    scales::label_percent(accuracy = accuracy, suffix = " percent", ...)(x)
}

#' @rdname number_formats
#' @param sentence_case Boolean: if `TRUE` (default), text will also be converted to sentence case,
#' i.e. capitalizing the first letter
#' @export
percent_eng <- function(x, accuracy = 1, sentence_case = TRUE) {
    num_error(x)
    x <- english::english(round(x * 100))
    x <- paste(x, "percent")
    if (sentence_case) {
        x <- stringr::str_to_sentence(x)
    }
    x
}

#' @rdname number_formats
#' @export
dollark <- function(x, accuracy = 1, ...) {
    num_error(x)
    scales::label_dollar(accuracy = accuracy, scale = 1 / 1000, suffix = "k", ...)(x)
}

#' @rdname number_formats
#' @export
dollar1 <- function(x, ...) {
    num_error(x)
    scales::label_dollar(accuracy = 1, ...)(x)
}
