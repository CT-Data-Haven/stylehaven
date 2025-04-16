#' @title Make multi-hue, multi-shade qualitative color palettes
#' @description `palx` is based on the Palx javascript library, basically a port of its core functions using the `colorspace` package. Provided a base color, it pulls a set of hues from across the spectrum with the same lightness and saturation, plus a gray color. It then creates shades of those hues from nearly black to nearly white. You probably don't actually want every hue; it's just a good way to get a bunch of colors to choose from.
#'
#' You can view a palette graphically by running `plot(my_palx)`, or by setting `plot = TRUE`; the former returns a ggplot object that you can use again, while the latter just prints the plot once. This creates a grid of hue vs. shade tiles.
#'
#' For generating a palette I need to extract different hues at different shades from, such as to make a small palette for a set of charts, I find getting a list (`return_df = FALSE`) is most versatile.
#' @param color A string of a color name (`"red"`) or hex code (`"#6f54d6"`).' This should be a single value, as it's the color the palette will be based around, though it's not guaranteed that this exact color will be in the final palette.
#' @param n_hues A number between 1 and 12. The number of hues returned will be this + 1, because a gray color will be added.
#' @param n_shades A number, for the number of shades to return for each hue. Defaults 6; too many more than that will probably become hard to distinguish.
#' @param row Numeric. If `NULL`, the default, all rows (shades) are returned. Otherwise, only the rows with these indices are returned. Just a shortcut for subsetting the list.
#' @param return_df Logical, whether to return a data frame (`return_df = TRUE`) or a list of character vectors. Defaults `FALSE`.
#' @param plot Logical, whether to call `plot_palx` before returning. This doesn't change what the function returns, it just prints out a ggplot chart and returns the colors as normal. Defaults `FALSE`.
#' @param labels Logical, whether to add labels on each tile giving colors' hex codes. Defaults `FALSE`.
#' @return If `return_df = TRUE`, a tibble with `n_shades` rows by one column per hue, plus a column giving the shade number. Otherwise, a named list (length `n_shades`) of character vectors, where each list item represents one shade. Both the tibble and named list are extended with the "palx" class, so that users can conveniently run `plot(my_palx)` or `as_tibble(my_palx)`.
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
#' palette <- palx("#9CCC0C") # returns a list
#' palx("#9CCC0C", return_df = TRUE) # returns a data frame
#'
#' # convert `palette` to a tibble
#' tibble::as_tibble(palette)
#' # should yield a tibble with the additional class `palx`
#' class(tibble::as_tibble(palette))
#'
#' # plot `palette` using ggplot
#' plot(palette)
#'
#' # bad examples all with one hue that return weird / not very useful palettes
#' too_light <- palx("#ccf4fa", plot = TRUE) # saturated but too light
#' low_sat <- palx("#6c888d", plot = TRUE) # not too light, but not saturated enough
#' good <- palx("#0e91a7", plot = TRUE) # much better--high saturation, middle lightness
#' @source \url{https://github.com/jxnblk/palx}
#' @seealso [colorspace::lighten()]
#' @keywords viz-utils
#' @keywords color
#' @export
palx <- function(color, n_hues = 8, n_shades = 6, row = NULL, return_df = FALSE, plot = FALSE, labels = FALSE) {
    keys <- hue_keys()
    max_hues <- length(keys)
    if (n_hues > max_hues) {
        cli::cli_warn("This function uses a maximum of {max_hues} hues. {.arg n_hues} is being set to {max_hues}.")
        n_hues <- max_hues
    }

    if (length(color) > 1) cli::cli_abort("Argument {.arg color} should be of length 1.")
    if (n_shades < 1) cli::cli_abort("Argument {.arg n_shades} should be at least 1.")
    if (n_hues < 1) cli::cli_abort("Argument {.arg n_hues} should be at least 1.")

    is_named_color <- color %in% colors()
    hex <- ifelse(is_named_color, scales::col2hcl(color), check_hex(color))

    crds <- colorspace::coords(methods::as(colorspace::hex2RGB(hex), "HLS"))
    h <- crds[, 1]
    l <- crds[, 2]
    s <- crds[, 3]
    min_sat <- 1 / 8
    hues <- make_hues(h, n_hues)
    col_names <- gsub("\\d+_", "", names(hues))
    base_cols <- colorspace::HLS(hues, l, s, col_names)
    base_gray <- colorspace::HLS(h, l, min_sat, "gray")
    base_colors <- bind_hls(c(base_cols, base_gray)) # coords

    shade_list <- make_shades(base_colors, n_shades)
    if (!is.null(row)) shade_list <- shade_list[row]

    class(shade_list) <- c("palx", class(shade_list))
    if (return_df) {
        result <- as_tibble.palx(shade_list)
    } else {
        result <- shade_list
    }

    if (plot) print(plot.palx(result, labels))
    return(result)
}

########## HELPER FUNCTIONS ----
check_hex <- function(color) {
    if (!(grepl("^\\#[[:xdigit:]]{6}$", color) | grepl("^\\#[[:xdigit:]]{8}$", color))) {
        cli::cli_abort("{.arg color} should be a valid color name or hex code.",
            .envir = parent.frame(2L)
        )
    } else {
        color
    }
}

hue_keys <- function() {
    hues <- seq(30, 360, by = 30)
    lbls <- c("orange", "yellow", "lime", "green", "teal", "cyan", "blue", "indigo", "violet", "fuschia", "pink", "red")
    nums <- sprintf("%02d", c(4:12, 1:3))
    names(hues) <- paste(nums, lbls, sep = "_")
    hues
}

make_shades <- function(crds, shds) {
    if (shds == 1) {
        lum <- 0
    } else {
        lum <- seq(-0.8, 0.9, length.out = shds)
    }

    hexes <- colorspace::hex(colorspace::HLS(crds))
    lum <- purrr::map(lum, function(l) colorspace::lighten(hexes, amount = l, method = "relative", space = "HLS"))
    lum <- purrr::map(lum, rlang::set_names, rownames(crds))
    lum <- rlang::set_names(lum, function(x) sprintf("shade%02d", seq_along(x)))
    lum
}


make_hues <- function(h, n) {
    keys <- hue_keys()
    step <- 360 / n
    band <- 30
    off <- 2
    # hue_vals <- purrr::map_dbl(1:n, \(val){
    #   hue <- floor((h + (val * step)) %% 360)
    #   dplyr::if_else(hue == 0, 360, hue)
    # })
    # hue_idx <- ceiling((hue_vals - off) / band) + 1
    hue_vals <- seq(band, 360, length.out = n)
    hue_idx <- floor(hue_vals / band)
    hue_names <- names(keys)[hue_idx]
    hues <- stats::setNames(hue_vals, hue_names)
    hues <- hues[sort(names(hues))]
    hues
}


bind_hls <- function(x) {
    # x = list of hls objs
    x <- purrr::map(x, colorspace::coords)
    x <- purrr::reduce(x, rbind)
    x
}

############################## METHODS ----

######## AS_TIBBLE METHOD ----
#' @rdname palx
#' @param x For `as_tibble` or `plot` methods: The output of calling `palx`, as either a list or data frame
#' @param ... Not currently implemented
#' @importFrom tibble as_tibble
#' @exportS3Method tibble::as_tibble
#' @export
as_tibble.palx <- function(x, ...) {
    if (!inherits(x, "palx")) {
        cli::cli_abort("Argument {.arg x} should be the result of calling {.fun palx}, either as a list or a data frame.")
    }
    if (inherits(x, "tbl_df")) {
        return(x)
    }
    # if not already a tibble, comes in as a list
    tbl <- purrr::map(x, dplyr::bind_rows)
    tbl <- dplyr::bind_rows(tbl, .id = "shade")
    tbl$shade <- as.numeric(gsub("[a-z]+", "", tbl$shade))
    class(tbl) <- c("palx", class(tbl))
    return(tbl)
}


######## PLOT METHOD ----
#' @rdname palx
#' @exportS3Method base::plot
#' @export
plot.palx <- function(x, ..., labels = FALSE) {
    if (!inherits(x, "palx")) {
        cli::cli_abort("Argument {.arg x} should be the result of calling {.fun palx}, either as a list or a data frame.")
    }
    if (inherits(x, "data.frame")) {
        df <- x
    } else {
        df <- as_tibble.palx(x)
    }
    df <- dplyr::mutate(df, shade = sprintf("%02d", shade))
    df <- tidyr::pivot_longer(df, cols = -shade, names_to = "hue", names_ptypes = list(hue = factor()))
    gg <- ggplot2::ggplot(df, ggplot2::aes(x = hue, y = shade))
    gg <- gg + ggplot2::geom_tile(ggplot2::aes(fill = value))
    gg <- gg + ggplot2::scale_fill_identity()
    gg <- gg + ggplot2::scale_x_discrete(expand = ggplot2::expansion(), position = "top")
    gg <- gg + ggplot2::scale_y_discrete(expand = ggplot2::expansion())
    gg <- gg + ggplot2::theme(axis.ticks = ggplot2::element_blank())

    if (labels) {
        gg + ggplot2::geom_label(ggplot2::aes(fill = value),
            size = 2.8, alpha = 0.6, label.size = 0
        )
    } else {
        gg
    }
}
