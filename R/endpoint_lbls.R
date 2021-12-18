#' @title endpoint_lbls
#' @description Easily put together labels for the endpoints of a chart, such as a line chart with outwardly-justified labels at each end. Labels with the formats `{name}: {fun(value)}` (long format) and `{fun(value}` (short format) will be created, such as `"Connecticut: 12%"` and `"16%"` respectively. Often one format will be used on one end and the other format on the other end, but `long_side` gives you some options around this. The intention around combining labels like this is so you can put labels on both ends, aligned nicely, in a single `geom_text` call.
#' @param data A data frame
#' @param x Bare column name of the independent value which has the start and finish points (probably a date). If this isn't numeric, `endpoint_lbls` will try to coerce it to numeric, which may yield undesired results.
#' @param value Bare column name of the value column
#' @param group Bare column name of the name or grouping variable that should be shown in the labels.
#' @param frac Numeric; ratio to the `x` variable by which labels should be offset. Default: 0.2
#' @param fun A function, used to create value labels. `scales::label_*` functions will be very useful here. If `NULL` (the default), no formatting is done.
#' @param long_side Character, either `"right"` (the default), `"left"`, or `"both"`. For `"left"` or `"right"`, this refers to whether the longer label should be on the right or the left, returning a short label on the opposite side. If `"both"`, only the long label is returned. Labels are combined in a new column called `lbl`.
#' @return A data frame: the original data frame passed in to `data`, with 3 additional columns:
#'
#' * `x`, the x-values with offsets added
#' * `just`, a column of 0 or 1 giving the justification value dependent on which end the label will appear
#' * `lbl`, a column of formatted label text
#' @examples
#' \dontrun{
#' if(interactive()){
#'    library(ggplot2)
#'    # note that it will still be up to your judgment to set scale expansion,
#'    # since that will depend on things outside the scope of just this function
#'    cws_trend %>%
#'      dplyr::filter(indicator == "local_govt_responsive", category == "Age") %>%
#'      endpoint_lbls(value = value, x = year, group = group,
#'                    fun = percent100) %>%
#'      ggplot(aes(x = year, y = value, color = group)) +
#'      geom_line() +
#'      geom_point(size = 3) +
#'      geom_text(aes(label = lbl, hjust = just, x = x)) +
#'      scale_x_continuous(expand = expansion(add = c(1, 2)))
#'
#'    cws_trend %>%
#'      dplyr::filter(indicator == "local_govt_responsive", category == "Age") %>%
#'      endpoint_lbls(value = value, x = year, group = group, long_side = "both",
#'                    fun = percent100) %>%
#'      ggplot(aes(x = year, y = value, color = group)) +
#'      geom_line() +
#'      geom_point(size = 3) +
#'      geom_text(aes(label = lbl, hjust = just, x = x)) +
#'      scale_x_continuous(expand = expansion(add = 2))
#'  }
#' }
#' @export
#' @rdname endpoint_lbls
endpoint_lbls <- function(data, x, value, group, frac = 0.2, fun = NULL, long_side = c("right", "left", "both")) {
  long_side <- match.arg(long_side, c("right", "left", "both"))
  if (is.null(fun)) {
    fun <- I
  }
  assertthat::assert_that(inherits(fun, "function"), msg = "`fun` should be a function.")
  rng <- range(as.numeric(data[[rlang::as_label(enquo(x))]]))
  out <- data %>%
    dplyr::mutate({{ x }} := as.numeric({{ x }}),
                  is_min = {{ x }} == min({{ x }}, na.rm = TRUE),
                  sign = ifelse(is_min, -1, 1),
                  off = frac * sign,
                  x = {{ x }} + off,
                  just = ifelse(is_min, 1, 0),
                  short_lbl = purrr::map_chr({{ value }}, fun),
                  long_lbl = sprintf("%s: %s", {{ group }}, short_lbl))
  if (long_side == "right") {
    out <- out %>%
      dplyr::mutate(lbl = ifelse(is_min, short_lbl, long_lbl))
  } else if (long_side == "left") {
    out <- out %>%
      dplyr::mutate(lbl = ifelse(is_min, long_lbl, short_lbl))
  } else {
    out <- out %>%
      dplyr::mutate(lbl = long_lbl)
  }
  out %>%
    dplyr::select(-is_min, -sign, -off, -short_lbl, -long_lbl)
}

