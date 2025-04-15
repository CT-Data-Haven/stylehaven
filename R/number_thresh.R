#' @title Format and top- or bottom-code numbers based on a threshold
#' @description This is a set of helper functions for formatting numbers where you
#' also need to top-code or bottom-code values above or below some threshold (or both).
#' Additional arguments help with generating plain English text or escaped HTML
#' characters for printing in tables.
#' @param x Numeric vector
#' @param thresh Numeric, the threshold above/below which numbers will be capped.
#' @param less_than Boolean: if `TRUE`, values *less than* the threshold will be
#' lumped together. Otherwise, values *greater than* the threshold will be
#' lumped. Ignored if both bottom and top endpoints are given in `thresh`. Default: TRUE
#' @param accuracy Number: accuracy of formatted numbers, passed to `scales::label_number`
#' and related functions. Defaults to 1, meaning no decimal places are returned.
#' @param txt Boolean: if `TRUE`, plain English is used (e.g. "less than") instead of
#' symbols (e.g. "<"). For `percent_thresh`, this also means using " percent" instead
#' of "%". Default: FALSE
#' @param html Boolean: if `TRUE`, HTML-appropriate symbols are used (e.g. `&lt;`)
#' instead of more readable ones (e.g. "<"). Has no effect if `txt = TRUE`. Default: FALSE
#' function
#' @return A character vector of the same length as `x`
#' @inheritDotParams scales::label_number
#' @examples
#' rate <- c(0.9, 0.95, 0.92, 0.991)
#' percent_thresh(rate, thresh = 0.99, less_than = FALSE)
#' percent_thresh(rate, thresh = 0.99, less_than = FALSE, txt = TRUE)
#'
#' # censor amounts under 100 dollars or above 1000 dollars
#' money <- c(200, 99, 400, 1005, 999)
#' dollar_thresh(money, thresh = c(100, 1000))
#' @export
#' @keywords string-formatting
#' @rdname number_thresh

number_thresh <- function(x, thresh,
                          less_than = TRUE,
                          accuracy = 1,
                          txt = FALSE,
                          html = FALSE,
                          ...) {
    thresher(x, thresh,
        less_than = less_than,
        accuracy = accuracy,
        txt = txt,
        html = html,
        lbl_type = "number",
        ...
    )
}

#' @export
#' @rdname number_thresh
percent_thresh <- function(x, thresh,
                           less_than = TRUE,
                           accuracy = 1,
                           txt = FALSE,
                           html = FALSE,
                           ...) {
    thresher(x, thresh,
        less_than = less_than,
        accuracy = accuracy,
        txt = txt,
        html = html,
        lbl_type = "percent",
        ...
    )
}

#' @export
#' @rdname number_thresh
dollar_thresh <- function(x, thresh,
                          less_than = TRUE,
                          accuracy = 1,
                          txt = FALSE,
                          html = FALSE,
                          ...) {
    thresher(x, thresh,
        less_than = less_than,
        accuracy = accuracy,
        txt = txt,
        html = html,
        lbl_type = "dollar",
        ...
    )
}


thresh_function <- function(type, txt) {
    # suffix <- prefix <- NULL
    args <- list()
    if (txt) {
        if (type == "percent") {
            args$suffix <- " percent"
        } else if (type == "dollar") {
            # args$suffix <- " dollars"
            # args$prefix <- ""
        }
    }
    if (type == "percent") {
        fun <- scales::label_percent
    } else if (type == "dollar") {
        fun <- scales::label_currency
    } else {
        fun <- scales::label_comma
    }
    purrr::partial(fun, !!!args)
}

thresher <- function(x,
                     thresh,
                     less_than = TRUE,
                     accuracy = 1,
                     txt = FALSE,
                     html = FALSE,
                     lbl_type = c("number", "percent", "dollar"),
                     ...) {
    num_error(x)
    rlang::arg_match(lbl_type)
    if (length(thresh) < 1 || length(thresh) > 2) {
        cli::cli_abort("{.arg thresh} can take arguments of length 1 or 2.")
    }
    if (length(less_than) > 1) {
        cli::cli_abort("{.arg less_than} can only take arguments of length 1.")
    }

    lbl_fun <- thresh_function(lbl_type, txt)
    lbllr <- lbl_fun(accuracy = accuracy, ...)
    # create lower, upper bounds. use inf as dummies
    if (length(thresh) < 2) {
        if (less_than) {
            thresh <- c(thresh, Inf)
        } else {
            thresh <- c(-Inf, thresh)
        }
    } else {
        # make sure they're in order
        thresh <- sort(thresh)
    }

    # can make both endpoints now
    less_than <- c(TRUE, FALSE)
    sign <- fmt_signs(less_than, txt, html)

    dplyr::case_when(
        x < thresh[1] ~ paste0(sign[1], lbllr(thresh[1])),
        x > thresh[2] ~ paste0(sign[2], lbllr(thresh[2])),
        TRUE ~ lbllr(x)
    )
}

fmt_signs <- function(less_thans, txt, html) {
    purrr::map_chr(less_thans, function(less_than) {
        fmt_signs_once_(less_than, txt, html)
    })
}

fmt_signs_once_ <- function(less_than, txt, html) {
    lookup <- tibble::tribble(
        ~t,    ~l,    ~h,    ~sign,
        TRUE,  TRUE,  TRUE,  "less than ",
        FALSE, TRUE,  TRUE,  "&lt;",
        TRUE,  FALSE, TRUE,  "more than ",
        FALSE, FALSE, TRUE,  "&gt;",
        TRUE,  TRUE,  FALSE, "less than ",
        FALSE, TRUE,  FALSE, "<",
        TRUE,  FALSE, FALSE, "more than ",
        FALSE, FALSE, FALSE, ">"
    )
    lookup$sign[less_than == lookup$l & txt == lookup$t & html == lookup$h]
}
