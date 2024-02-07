#' @title Prepare chart labels that should be repelled apart
#' @description This function is designed for plots, particularly dot plots,
#' where some values are very close together and need to be pushed apart in
#' order to avoid overlaps. It returns just the columns needed to know which
#' points should be dodged apart.
#'
#' It doesn't calculate new positions for those points—
#' that depends on your specific use, but `ggrepel::geom_text_repel` is likely
#' what you'll use with this. You'll probably call this to get a data frame
#' of the points that should be dodged, then use `dplyr::semi_join` on the
#' original data to plot the points that *do* need to be dodged, and
#' `dplyr::anti_join` for the ones that *don't* need to be dodged (see example).
#' @param data A data frame
#' @param x Bare name of the column being plotted on the independent axis
#' @param value Bare name of the dependent variable
#' @param group Bare column name of the grouping variable, likely used for
#' point color
#' @param thresh Numeric: the threshold value of the difference between points.
#' Any pair of points with a difference of less than `thresh` will be
#' considered too close, and will be included in the output as points to dodge.
#' @param digits Number of digits to round to before calculating differences
#' between points. Default: 2
#' @return A data frame of too-close points, with columns corresponding to the
#' `x` and `group` columns.
#' @examples
#' if (requireNamespace("ggrepel", quietly = TRUE)) {
#'   library(ggplot2)
#'   cohesion_by_race <- community_cohesion |>
#'     dplyr::filter(category %in% c("Greater New Haven", "Race/Ethnicity"))
#'
#'   ggplot(cohesion_by_race, aes(x = question, y = value)) +
#'     geom_point(aes(color = group), size = 8) +
#'     geom_text(aes(label = percent100(value)), color = "white", size = 3) +
#'     coord_flip()
#'
#'   (to_dodge <- dodge_lbls(cohesion_by_race,
#'                           x = question,
#'                           value = value,
#'                           group = group,
#'                           thresh = 0.015))
#'
#'   ggplot(cohesion_by_race, aes(x = question, y = value)) +
#'     geom_point(aes(color = group), size = 8) +
#'     geom_text(aes(label = percent100(value)),
#'               data = ~dplyr::anti_join(., to_dodge, by = c("question", "group")),
#'               color = "white", size = 3) +
#'     ggrepel::geom_text_repel(aes(label = percent100(value), color = group),
#'                              data = ~dplyr::semi_join(., to_dodge, by = c("question", "group")),
#'                              size = 3, direction = "x", nudge_x = 0.2, seed = 1) +
#'     coord_flip()
#' }
#' @export
#' @rdname dodge_lbls
#' @import rlang
#' @import dplyr

dodge_lbls <- function(data, x, value, group, thresh, digits = 2) {
  id_str <- rlang::as_name(enquo(x))
  df_left <-  dplyr::select(data, {{ x }}, x1 = {{ group }}, val1 = {{ value }})
  df_right <- dplyr::select(data, {{ x }}, x2 = {{ group }}, val2 = {{ value }})
  joined <- dplyr::inner_join(df_left, df_right, by = id_str)
  joined <- dplyr::filter(joined, x1 != x2)
  joined <- dplyr::mutate(joined, dplyr::across(dplyr::matches("^val\\d$"), round, digits))
  joined <- dplyr::mutate(joined, diff = abs(val1 - val2))
  joined <- dplyr::filter(joined, diff <= thresh)
  joined <- dplyr::select(joined, {{ x }}, {{ group }} := x1)
  joined
}


