#' @title Create labeled intervals based on standard deviations
#' @description Occasionally we make charts using standard deviations away from an average value to fill bars or geographies---not a hard task, but tedious. This function takes a data frame, then gets a midpoint value, either by calculating the mean or by filtering for an observation already in the data frame (such as a statewide value). It then calculates z-scores based on this midpoint and standard deviation, then cuts z-scores based on `brks`. Pay close attention to the argument `by`, which allows you to do these calculations grouped by some column; this is useful if you have a data frame of several different indicators. Alternatively, passing a grouped data frame will also do the calculations by group.
#' @param x A data frame or tibble
#' @param value Bare name of the numeric value column, Default: value
#' @param filters An optional named list of values to use for filtering. If given, the observation matching these values will be used as the midpoint. If NULL (the default), the midpoint will be calculated as the mean of values, grouped by `x`'s grouping columns (if any) and the arguments to `by` (also if any).
#' @param by Optional character vector. If given, this will be used as the group within which intervals are calculated. Default: NULL
#' @param brks Numeric vector of break points for cutting z-scores. This vector, plus `-Inf` and `Inf`, will be passed to `base::cut`'s `breaks` argument. Default: c(-2, -1/2, 1/2, 2)
#' @param labels Character vector of labels for the resulting factor. If NULL, levels will be in `base::cut`'s interval notation. The length of this vector should be one more than the length of `brks`. Default: NULL
#' @param na.rm Boolean passed on to `mean` if midpoints are being calculated. Default: TRUE
#' @param keep_calcs Boolean, whether to keep columns from calculations. Default: TRUE
#' @param ... Additional arguments passed to `base::cut`
#' @return A data frame or tibble with the same number of rows as `x`. If `keep_calcs` is true, the returned data frame will have numeric columns added for midpoint (`midpt`), standard deviation (`sd`), and z-score (`z`), and a factor column for the resulting intervals (`brk`). If false, the only column added will be the intervals.
#' @examples 
#' # Calculate intervals along the full dataset, based on calculated mean
#' stdev_brks(life_exp, 
#'            labels = c("Lower", "Somewhat lower", "Average", "Somewhat higher", "Higher"))
#'            
#' # Calculate intervals for each of the three indicators in the `question` column. 
#' # Both examples have the same result:
#' fin_insecurity |>
#'   stdev_brks(filters = list(category = "Connecticut"), by = "question")
#' 
#' fin_insecurity |>
#'   dplyr::group_by(question) |>
#'   stdev_brks(filters = list(category = "Connecticut"))
#' @export 
#' @seealso [base::cut()]
stdev_brks <- function(x,
                       value = value,
                       filters = NULL,
                       by = NULL,
                       brks = c(-2, -1/2, 1/2, 2),
                       labels = NULL,
                       na.rm = TRUE,
                       keep_calcs = TRUE,
                       ...) {
  # type checks
  if (!is.null(labels)) {
    if (!(length(labels) == (length(brks) + 1))) {
      cli::cli_abort("If supplying labels, {.arg labels} must have a length of 1 longer than that of {.arg brks}.")
    }
  }
  
  grp_vars <- dplyr::group_vars(x)
  by <- c(grp_vars, by)
  x_out <- dplyr::ungroup(x)
  
  if (!is.null(filters)) {
    if (!inherits(filters, "list") || is.null(names(filters))) {
      cli::cli_abort("{.arg filters} must be a named list, or {.val NULL} in order to skip filtering.")
    }
    filter_df <- as.data.frame(filters)
    # warning--only use first row
    if (nrow(filter_df) > 1) {
      filter_df <- dplyr::slice(filter_df, 1)
      filter_lbl <- purrr::imap_chr(filter_df, function(df, id) paste(id, df, sep = " = "))
      cli::cli_warn(c("Your filters have too many values. Only the first observation will be used as the midpoint.",
                      "i" = "Filtering for {filter_lbl}"))
    }
    
    midpt_df <- dplyr::semi_join(x_out, filter_df, by = names(filters))
    midpt_df <- dplyr::select(midpt_df, tidyselect::all_of(by), midpt = {{ value }})
    # remove midpoint for getting sd
    sd_df <- dplyr::anti_join(x_out, filter_df, by = names(filters))
  } else {
    midpt_df <- dplyr::group_by(x_out, dplyr::across(tidyselect::all_of(by)))
    midpt_df <- dplyr::summarise(midpt_df, midpt = mean({{ value }}, na.rm = na.rm))
    # no middle value to remove
    sd_df <- x
  }
  
  # check that there are no dupes in midpoint groups
  # e.g. for cws data shouldn't use category = "Gender" because there's more than one observation
  if (length(by) > 0 && any(duplicated(midpt_df[[by]]))) {
    cli::cli_abort("Your filters match too many observations. Only one observation should be used as the midpoint.")
  }
  sd_df <- dplyr::group_by(sd_df, dplyr::across(tidyselect::all_of(by)))
  sd_df <- dplyr::summarise(sd_df, sd = sd({{ value }}, na.rm = na.rm))
  
  # left join if there's a join column, cross join if null
  if (length(by) > 0) {
    x_out <- dplyr::left_join(x_out, midpt_df, by = by)
    x_out <- dplyr::left_join(x_out, sd_df, by = by)
  } else {
    x_out <- dplyr::cross_join(x_out, midpt_df)
    x_out <- dplyr::cross_join(x_out, sd_df)
  }
  
  x_out <- dplyr::mutate(x_out, z = ({{ value }} - midpt) / sd)
  x_out <- dplyr::mutate(x_out, brk = cut(z, breaks = c(-Inf, brks, Inf), labels = labels, ...))
  
  if (!keep_calcs) {
    x_out <- dplyr::select(x_out, -midpt, -sd, -z)
  }
  x_out <- dplyr::group_by(x_out, dplyr::across(tidyselect::any_of(grp_vars)))
  x_out
}
