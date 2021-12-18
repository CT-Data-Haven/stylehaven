#' @title Make multi-hue, multi-shade qualitative color palettes
#' @description `palx` is based on the Palx javascript library, basically a port of its core functions using the `colorspace` package. Provided a base color, it pulls a set of hues from across the spectrum with the same lightness and saturation, plus a gray color. It then creates shades of those hues from nearly black to nearly white. You probably don't actually want every hue; it's just a good way to get a bunch of colors to choose from.
#'
#' `plot_palx` makes a grid with `ggplot` of hue vs shade tiles. This can also be done with `palx`, setting `plot = TRUE`.
#' @param hex A string of a color hex code (e.g. `"#6f54d6"`). This should be a single value, as it's the color the palette will be based around, though it's not guaranteed that this exact color will be in the final palette.
#' @param n_hues A number between 1 and 12. The number of hues returned will be this + 1, because a gray color will be added.
#' @param n_shades A number, for the number of shades to return for each hue. Defaults 9; too many more than that will probably become hard to distinguish.
#' @param row Numeric. If `NULL`, the default, all rows (shades) are returned. Otherwise, only the rows with these indices are returned. Just a shortcut for subsetting the list.
#' @param as_df Logical, whether to return a data frame (`as_df = TRUE`) or a list of character vectors. Defaults `FALSE`.
#' @param plot Logical, whether to call `plot_palx` before returning. This doesn't change what the function returns, it just prints out a ggplot chart and returns the colors as normal. Defaults `FALSE`.
#' @param data The output of calling `palx`, as either a list or data frame
#' @param labels Logical, whether to add labels on each tile giving colors' hex codes. Defaults `FALSE`.
#' @return If `as_df = TRUE`, a data frame with `n_shades` rows by one column per hue, plus a column giving the shade number. Otherwise, a named list (length `n_shades`) of character vectors, where each list item represents one shade.
#' @details Some notes about color:
#'
#' * Hue refers to what would commonly be the general name of a color, e.g. blue or yellow.
#' * Shade refers to how light or dark a color is, e.g. light blue, dark blue.
#' * Saturation refers to the strength of a color, or its distance from a neutral gray.
#'
#' The color spectrum is broken up into even chunks, but there's a set of wavelengths that appear pretty similar--this shows up in the lime-green-teal-cyan-blue colors. That's just how human eyeballs work I guess.
#'
#' These palettes shouldn't be used for sequential palettes, even pulling multiple shades from one hue. Good sequential palettes are corrected for perceptual differences in shade, whereas this just takes even steps. Also please don't use rainbow scales for sequential data.
#'
#' You likely only want one or two shades worth of colors (use the `row` arguments), either adjacent for colors that appear "even", or one light and one dark for a paired palette. Saturated colors that aren't too dark or too light work best.
#' @examples
#' palx("#9CCC0C") # returns a list
#' palx("#9CCC0C", as_df = TRUE) # returns a data frame
#'
#' # bad examples all with one hue that return weird / not very useful palettes
#' palx("#ccf4fa") # saturated but too light
#' palx("#6c888d") # not too light, but not saturated enough
#' palx("#0e91a7") # much better--high saturation, middle lightness
#' @source \url{https://github.com/jxnblk/palx}
#' @seealso [colorspace::lighten()]
#' @export
palx <- function(hex, n_hues = 8, n_shades = 9, row = NULL, as_df = FALSE, plot = FALSE, labels = FALSE) {
  max_hues <- length(hue_keys)
  if (n_hues > max_hues) {
    warning(sprintf("This function uses a maximum of %s hues. n_hues is being set to %s", max_hues, max_hues))
    n_hues <- max_hues
  }
  assertthat::assert_that(length(hex) == 1)
  assertthat::assert_that(n_shades >= 1)
  assertthat::assert_that(n_hues >= 1)

  crds <- colorspace::coords(methods::as(colorspace::hex2RGB(hex), "HLS"))
  h <- crds[,1]; l <- crds[,2]; s <- crds[,3]
  min_sat <- 1/8
  hues <- make_hues(h, n_hues)
  base_cols <- colorspace::HLS(hues, l, s,       names(hues))
  base_gray <- colorspace::HLS(h,    l, min_sat, "gray")
  base_colors <- bind_hls(c(base_cols, base_gray)) # coords

  shade_list <- make_shades(base_colors, n_shades)
  if (!is.null(row)) shade_list <- shade_list[row]

  if (plot) print(plot_palx(shade_list, labels))

  if (as_df) {
    palx_to_df(shade_list)
  } else {
    shade_list
  }
}

hue_keys <- stats::setNames(seq(30, 360, by = 30),
                     c("orange", "yellow", "lime", "green", "teal",
                       "cyan", "blue", "indigo", "violet", "fuschia", "pink", "red"))

palx_to_df <- function(shd_lst) {
  dplyr::bind_rows(shd_lst, .id = "shade") %>%
      dplyr::mutate(shade = as.numeric(regmatches(shade, regexpr("\\d+", shade))))
}

#' @rdname palx
#' @export
plot_palx <- function(data, labels = FALSE) {
  if (inherits(data, "data.frame")) {
    df <- data
  } else if (is.list(data)) {
    df <- palx_to_df(data)
  } else {
    stop("data should be the result of calling palx, either as a list or a data frame")
  }
  p <- df %>%
    dplyr::mutate(shade = sprintf("%02d", shade)) %>%
    tidyr::pivot_longer(cols = -shade, names_to = "hue",
                        names_transform = list(hue = forcats::as_factor)) %>%
    ggplot2::ggplot(ggplot2::aes(x = hue, y = shade)) +
    ggplot2::geom_tile(ggplot2::aes(fill = value)) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(mult = 0), position = "top") +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(mult = 0)) +
    ggplot2::theme(axis.ticks = ggplot2::element_blank())
  if (labels) {
    p +
      ggplot2::geom_label(ggplot2::aes(label = value), size = 2.8, alpha = 0.6, label.size = 0)
  } else {
    p
  }
}

make_hues <- function(h, n) {
  step <- 360 / n
  band <- 30
  off <- 2
  hue_vals <- purrr::map_dbl(1:n, ~floor((h + (. * step)) %% 360))
  hue_names <- names(hue_keys)[ceiling((hue_vals - off) / band)]
  hues <- stats::setNames(hue_vals, hue_names)
  # sort(hues)
  hues
}

make_shades <- function(crds, shds) {
  if (shds == 1) {
    lum <- 0
  } else {
    lum <- seq(-0.8, 0.9, length.out = shds)
  }

  hexes <- colorspace::hex(colorspace::HLS(crds))
  lum %>%
    purrr::map(~colorspace::lighten(hexes, amount = ., method = "relative", space = "HLS")) %>%
    purrr::map(rlang::set_names, rownames(crds)) %>%
    rlang::set_names(~paste0("shade", sprintf("%02d", seq_along(.))))
}

bind_hls <- function(x) {
  # x = list of hls objs
  x %>%
    purrr::map(colorspace::coords) %>%
    purrr::reduce(rbind)
}

