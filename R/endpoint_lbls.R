#' @title endpoint_lbls
#' @description Easily put together labels for the endpoints of a chart, such as a line chart with outwardly-justified labels at each end. Labels with the formats `{name}: {fun(value)}` (long format) and `{fun(value}` (short format) will be created, such as `"Connecticut: 12%"` and `"16%"` respectively. Often one format will be used on one end and the other format on the other end, but `long_side` gives you some options around this. The intention around combining labels like this is so you can put labels on both ends, aligned nicely, in a single `geom_text` call.
#' @param data A data frame
#' @param x Bare column name of the independent value which has the start and finish points (probably a date). If this isn't numeric, `endpoint_lbls` will try to coerce it to numeric, which may yield undesired results.
#' @param value Bare column name of the value column
#' @param group Bare column name of the name or grouping variable that should be shown in the labels.
#' @param mult Numeric; ratio to the `x` variable by which labels should be offset. Default: 0.05.
#' @param add Numeric; single value along the `x` variable by which labels should be offset. Defaults `NULL`. If both `mult` and `add` are given, `mult` takes precedence. Previously there was a `frac` argument which was supposed to act like `mult` but erroneously acted like `add`. 
#' @param fun A function, used to create value labels. `scales::label_*` functions will be very useful here. If `NULL` (the default), no formatting is done.
#' @param long_side Character, either `"right"` (the default), `"left"`, `"both"`, or `"none"`. For `"left"` or `"right"`, this refers to whether the longer label should be on the right or the left, returning a short label on the opposite side. If `"both"`, only long labels are returned; if `"none"`, only short labels are returned. Regardless, labels are combined in a new column called `lbl`.
#' @return A data frame: the original data frame passed in to `data`, with 3 additional columns:
#'
#' * `x`, the x-values with offsets added
#' * `just`, a column of 0 or 1 giving the justification value dependent on which end the label will appear
#' * `lbl`, a column of formatted label text
#' @examples
#'    library(ggplot2)
#'    # note that it will still be up to your judgment to set scale expansion,
#'    # since that will depend on things outside the scope of just this function
#'    cws_trend |>
#'      dplyr::filter(indicator == "local_govt_responsive", category == "Age") |>
#'      endpoint_lbls(value = value, x = year, group = group,
#'                    fun = percent100) |>
#'      ggplot(aes(x = year, y = value, color = group)) +
#'      geom_line() +
#'      geom_point(size = 3) +
#'      geom_text(aes(label = lbl, hjust = just, x = x)) +
#'      scale_x_continuous(expand = expansion(add = c(1, 3)),
#'                         breaks = c(2015, 2021))
#'
#'    cws_trend |>
#'      dplyr::filter(indicator == "local_govt_responsive", category == "Age") |>
#'      endpoint_lbls(value = value, x = year, group = group, long_side = "both",
#'                    fun = percent100, add = 0.4, mult = NULL) |>
#'      ggplot(aes(x = year, y = value, color = group)) +
#'      geom_line() +
#'      geom_point(size = 3) +
#'      geom_text(aes(label = lbl, hjust = just, x = x)) +
#'      scale_x_continuous(expand = expansion(add = 3),
#'                         breaks = c(2015, 2021))
#' @export

#' @rdname endpoint_lbls
endpoint_lbls <- function(data, x, value, group, mult = 0.05, add = NULL, fun = NULL, long_side = c("right", "left", "both", "none")) {
  long_side <- rlang::arg_match(long_side)
  if (is.null(fun)) {
    fun <- as.character
  }
  if (!inherits(fun, "function")) {
    cli::cli_abort("Argument {.arg fun} must be a function, or {.val NULL}.")
  }
  rng <- range(as.numeric(data[[rlang::as_label(enquo(x))]]))
  # calculate add if not supplied
  if (is.null(mult) & is.null(add)) {
    cli::cli_abort("Must supply {.arg mult} or {.arg add}.")
  }
  if (!is.null(mult)) {
    # if both mult & add given, use mult
    if (!is.null(add)) {
      cli::cli_alert_info("Both {.arg mult} and {.arg add} were supplied; only {.arg mult} will be used.")
    }
    base_off <- (rng[2] - rng[1]) * mult
  } else {
    base_off <- add
  }

  out <- data
  out <- dplyr::mutate(out, {{ x }} := as.numeric({{ x }}))
  out <- dplyr::mutate(out, is_max = {{ x }} == max({{ x }}, na.rm = TRUE))
  out <- dplyr::mutate(out, sign = ifelse(is_max, 1, -1))
  out <- dplyr::mutate(out, off = base_off * sign)
  out <- dplyr::mutate(out, x = {{ x }} + off)
  out <- dplyr::mutate(out, just = ifelse(is_max, 0, 1))
  out <- dplyr::mutate(out, short_lbl = purrr::map_chr({{ value }}, fun))
  out <- dplyr::mutate(out, long_lbl = sprintf("%s: %s", {{ group }}, short_lbl))

  if (long_side == "right") {
    out <- dplyr::mutate(out, lbl = dplyr::if_else(is_max, long_lbl, short_lbl))
  } else if (long_side == "left") {
    out <- dplyr::mutate(out, lbl = dplyr::if_else(is_max, short_lbl, long_lbl))
  } else if (long_side == "both") {
    out <- dplyr::mutate(out, lbl = long_lbl)
  } else if (long_side == "none") {
    out <- dplyr::mutate(out, lbl = short_lbl)
  }
  dplyr::select(out, -is_max, -sign, -off, -short_lbl, -long_lbl)
}
