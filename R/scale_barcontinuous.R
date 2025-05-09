#' @title Set a continuous scale more suited to bar charts
#' @description This is a light wrapper around `ggplot2::scale_x_continuous` / `ggplot2::scale_y_continuous` that sets an `expand` argument of `mult = c(0, 0.05)`. The purpose of this is just to align the bases of bars with the x-axis (for `scale_y_continuous`) or y-axis (for `scale_x_continuous`), rather than the default that leaves a gap between bars and the x- or y-axis.
#' @param top Numeric, giving the ratio by which to set the upper padding. Defaults 0.05, as is the case for the underlying `ggplot2::scale_x/y_continuous`.
#' @param ... Additional arguments passed on to `ggplot2::scale_x/y_continuous`.
#' @examples
#' library(ggplot2)
#' 
#' local_govt <- cws_trend |> 
#'   dplyr::filter(year == 2024, question == "local_govt_responsive")
#' 
#' # default expansion leaves an awkward gap below 0-aligned bars
#' p <- ggplot(local_govt, aes(x = group, y = value)) +
#'   geom_col() +
#'   scale_x_discrete(labels = scales::label_wrap(10))
#' 
#' p
#' 
#' # scale_y_barcontinuous changes expansion to remove it
#' p +
#'   scale_y_barcontinuous()
#' 
#' @rdname scale_barcontinuous
#' @keywords viz-utils
#' @keywords ggplot
#' @export
scale_y_barcontinuous <- function(top = 0.05, ...) {
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, top)), ...)
}

#' @rdname scale_barcontinuous
#' @export
scale_x_barcontinuous <- function(top = 0.05, ...) {
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, top)), ...)
}
