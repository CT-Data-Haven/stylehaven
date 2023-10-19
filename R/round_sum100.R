#' @title Round a set of numbers so they'll add up to 100
#' @description This function takes a vector of numbers and rounds them to
#' coerce them to add up to 100. It works best for numbers with several decimal
#' places that are close to adding to 100. It's definitely not
#' fool-proof, but can help with situations like waffle charts where previous
#' rounding may make numbers not add up to exactly 100.
#' @param x A numeric vector
#' @param digits Number of digits to use for rounding; you probably don't need
#' to change this. Default: 0
#' @param verbose Logical; if `TRUE`, will print the sum, letting you confirm
#' whether the numbers do indeed add to 100. Defaults `FALSE`.
#' @return A numeric vector that hopefully adds up to 100.
#' @examples
#' round_sum100(c(9.2124, 40.292, 50.2), 0) # yay
#' round_sum100(c(0.24, 0.61, 0.15) * 100) # yay
#' round_sum100(c(0.24, 0.6, 0.15) * 100) # sad
#' @export

#' @rdname round_sum100
round_sum100 <- function(x, digits = 0, verbose = FALSE) {
  if (any(is.na(x))) cli::cli_warn("{.val NA} values in {.arg x} are being dropped.")
  x <- stats::na.omit(x)

  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- utils::tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  out <- y / up
  if (verbose) {
    out_sum <- sum(out)
    if (out_sum == 100) {
      cli::cli_alert_success("Sums to {out_sum}.")
    } else {
      cli::cli_alert_warning("Sums to {out_sum}, not 100.")
    }
  }
  out
}
