#' @title Offset bar chart labels without hard-coding
#' @description Sometimes when you add direct labels to a bar chart, it can be annoying to hard-code a value by which to offset the labels to add some padding before the end of the bar, e.g. using `nudge_y`. It's also a problem when you have different scales on different facets, or when batch generating charts where you don't know exactly what the scale will be for all of them.
#'
#' `ratio_to_max` finds the maximum value in a vector, and returns some fraction of it that you specify. `offset_lbls` adds columns to your data with parameters for adjusting labels, and uses `ratio_to_max` under the hood. Values are offset proportionate to the maximum value in the dataset. It also handles situations where some bars are too small to fit labels properly inside the bar.
#' @param data A data frame
#' @param value Bare name of the column to use
#' @param frac Numeric, the desired fraction of the maximum value by which to offset. Default: 0.05
#' @param thresh Numeric threshold for the ratio at which values will be considered too small to fit inside bars. Default: 0.15
#' @param margin Numeric scaling factor for positioning small values above / to the right of bars. Default: 1.5
#' @param fun A function, used to create value labels. `scales::label_*` functions will be very useful here. If `NULL` (the default), no formatting is done.
#' @param na A string to use to replace `NA` values. Default: `"N/A"`. If `NULL`, will print as `"NA"`.
#' @return For `ratio_to_max`, a single number giving frac * max(x). For `offset_lbls`, a data frame with the same number of rows as `data` and an additional 5 columns.
#' @details `offset_lbls` makes some decisions about how you might want to place labels but tries not to force those choices on you. The first thing calculated is a baseline offset, from `ratio_to_max`. For each value, there's a ratio to the maximum value. If that ratio is less than `thresh`, the value is considered "small". For non-small values, a y position is calculated by subtracting the baseline offset from the value; for small values, the y position is the value plus the baseline times the margin. Additionally, you can set text justifications differently for small and non-small values to set the alignment of labels above or below the bar.
#'
#' It's good to mess around with `frac` and `thresh` to suit your chart. I find that horizontal bars might need smaller values of `frac` and/or larger values of `thresh`.
#'
#' The columns in the returned data frame are:
#' * `is_small`: Logical: is this value considered small with respect to the maximum value and your threshold. This is useful if you have white labels _inside_ your bars, but need black or gray text for labels that fall _outside_ the bars.
#' * `off`: Numeric: the number by which y-positions are offset, positive for small values and negative for non-small values
#' * `value_off`: Numeric: value positions at which to place labels. `NA` values are replaced with 0. Used to be called `y`, but I'm changing to match ggplot's move away from `coord_flip`.
#' * `just`: Numeric, either 0 or 1 to use as horizontal or vertical justification in e.g. `ggplot2::geom_text`.
#' * `lbl`: Character: values as formatted by the function in `fun`, with `NA` values replaced per the `na` argument.
#' @examples
#' library(ggplot2)
#' # shares of people rating health as poor is too small for most groups to have
#' # easily legible label, so offset them to the right of their bars
#' self_rated_health |>
#'     offset_lbls(value = value, fun = percent100) |>
#'     ggplot(aes(x = value, y = forcats::fct_rev(group), fill = response)) +
#'     geom_col() +
#'     geom_text(aes(x = value_off, hjust = just, label = lbl, color = is_small)) +
#'     facet_grid(cols = vars(response)) +
#'     scale_x_barcontinuous(breaks = NULL) +
#'     scale_color_manual(values = c("TRUE" = "gray30", "FALSE" = "white")) +
#'     theme(legend.position = "none")
#' @rdname offset_lbls
#' @keywords viz-utils
#' @export
offset_lbls <- function(data, value, frac = 0.05, thresh = 0.15, margin = 1.5, fun = NULL, na = "N/A") {
    if (is.null(fun)) {
        fun <- as.character
    }
    if (!inherits(fun, "function")) {
        cli::cli_abort("Argument {.arg fun} must be a function, or {.val NULL}.")
    }
    if (is.null(na)) {
        na <- NA
    }
    data <- dplyr::mutate(data, ratio = {{ value }} / max({{ value }}, na.rm = TRUE))
    data <- dplyr::mutate(data, base_off = ratio_to_max({{ value }}, frac))
    data <- dplyr::mutate(data, is_small = ratio <= thresh | is.na({{ value }}))
    data <- dplyr::mutate(data, off = ifelse(is_small, margin * base_off, -base_off))
    data <- dplyr::mutate(data, value_off = tidyr::replace_na({{ value }} + off, 0))
    data <- dplyr::mutate(data, just = ifelse(is_small | value_off == 0, 0, 1))
    data <- dplyr::mutate(data, lbl = tidyr::replace_na(purrr::map_chr({{ value }}, fun), na))
    data <- dplyr::select(data, -ratio, -base_off)
    data
}

#' @param x A numeric vector
#' @rdname offset_lbls
#' @export
ratio_to_max <- function(x, frac = 0.05) {
    frac * max(x, na.rm = TRUE)
}
