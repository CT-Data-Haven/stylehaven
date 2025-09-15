#' Create pretty breaks labels
#'
#' This function maps over a vector of labels, such as those returned by `base::cut`. It works well in formatting `ggplot` scales, and can optionally pass parameters to an underlying call to `base::formatC`. Any `NA` values will be retained.
#' @param x A character vector.
#' @param format A function for formatting text, a string giving desired output format, or `NULL` (the default) for no formatting. Built-in shorthands are `"percent"`, `"dollar"`, and `"dollark"`, the last of which formats numbers like `$12.3k`. Alternatively, provide a character argument to be used by `base::formatC` and set `custom = TRUE`.
#' @param custom Logical, whether to use custom formatting, triggering a call to `formatC` with the arguments supplied to `format` and `...`. Defaults `FALSE`.
#' @param mult_by A number by which to multiply values in breaks. Defaults to 1, i.e. just the numbers input. Note that multiplication is carried out *before* rounding and formatting.
#' @param round_digits If not `NULL` (the default), the number of digits to round to. Note that this takes place after multiplying by `mult_by`.
#' @param sep A string by which to separate values in breaks.
#' @param ... Any additional arguments to pass on to the function given in `format`, or to `base::formatC` if `custom` is `TRUE`.
#' @return A character vector of formatted break labels.
#' @examples
#' percentage_brks <- c("[0.04,0.15]", "(0.15,0.25]", "(0.25,0.4]")
#' brk_labels(percentage_brks)
#' brk_labels(percentage_brks, format = "percent", mult_by = 100)
#'
#' scientific_brks <- c("[-15500,0]", "(0,20000]", "(20000,25000]")
#' brk_labels(scientific_brks, format = "e", custom = TRUE, digits = 2)
#' brk_labels(scientific_brks, format = stringr::str_pad, side = "left", pad = "0", width = 3)
#' @export
#' @keywords string-formatting
brk_labels <- function(
    x,
    format = NULL,
    custom = FALSE,
    mult_by = 1,
    round_digits = NULL,
    sep = " to ",
    ...) {
    if (!inherits(x, "character")) {
        cli::cli_abort("{.arg x} should be a character vector")
    }
    purrr::map_chr(x, function(lab) {
        if (is.na(lab)) {
            NA_character_
        } else {
            splits <- strsplit(lab, ",")[[1]]
            patt <- "[\\(\\)\\[\\]]"
            nums <- as.numeric(stringr::str_remove_all(splits, patt)) * mult_by
            # x1 <- as.numeric(stringr::str_remove_all(splits[1], "[\\(\\[]")) * mult_by
            # x2 <- as.numeric(stringr::str_remove_all(splits[2], "[\\)\\]]")) * mult_by

            if (!is.null(round_digits)) {
                # x1 <- round(x1, digits = round_digits)
                # x2 <- round(x2, digits = round_digits)
                nums <- round(nums, digits = round_digits)
            }

            if (rlang::is_function(format)) {
                out_nums <- rlang::exec(format, nums, ...)
            } else if (custom) {
                out_nums <- formatC(nums, format = format, ...)
            } else if (is.null(format)) {
                out_nums <- nums
            } else {
                out_nums <- dplyr::case_when(
                    format == "percent" ~ c(nums[1], paste0(nums[2], "%")),
                    format == "dollar" ~ paste0("$", nums),
                    format == "dollark" ~ sprintf("$%sk", nums),
                    TRUE ~ paste0(nums)
                )
            }

            paste(out_nums, collapse = sep)
        }
    })
}
