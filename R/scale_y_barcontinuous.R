#' @title Set a continuous scale more suited to bar charts
#' @description This is a light wrapper around `ggplot2::scale_y_continuous` that sets an `expand` argument of `mult = c(0, 0.05)`. The purpose of this is just to align the bases of bars with the x-axis, rather than the default that leaves a gap between bars and the x-axis.
#' @param top Numeric, giving the ratio by which to set the upper padding. Defaults 0.05, as is the case for the underlying `ggplot2::scale_y_continuous`.
#' @param ... Additional arguments passed on to `ggplot2::scale_y_continuous`.
#' @keywords function
#' @export
scale_y_barcontinuous <- function(top = 0.05, ...) {
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, top)), ...)
}
