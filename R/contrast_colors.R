#' @title Choose label colors based on contrast
#' @description This is a set of utility functions and classes for a situation that I too often gloss over: when making a chart with different fill colors (such as stacked bars with a sequential palette), it can be good to vary the label color to maintain good contrast between the background and foreground. For example, the ColorBrewer palettes often go from such a dark color to such a light color that black or dark gray labels won't be visible over the darkest bar, and white or light gray labels won't be visible over the lightest bar. This helps prepare for that by testing the contrast between a palette (presumably bar fill colors) and a light and dark option (presumably label text on those bars).
#'
#' A contrast ratio is calculated by `colorspace::contrast_ratio` according to the WCAG algorithm, and the minimum ratio defaults to 4.5 based on their accessibility guidelines. If `verbose` is true and any combination of palette color and label colors fails to meet this minimum, you'll get a message letting you know. For example, using medium gray colors for both dark and light labels is likely to lead to a situation where neither the dark nor the light label color would have enough contrast over the palette.
#'
#' `mutate_contrast` takes this a step further, working with a data frame to determine label colors from a factor column (or a column that can become a factor).
#'
#' `plot` methods create `ggplot` plots as visual references of the fill and label colors. For `cc_df` objects, there will be two columns of fill + label combinations with their ratios and a star for the label color with the greatest contrast. For `cc_vec` objects, this is a simplified version giving just one column of fills and the label colors with greatest contrast, but no ratio information.
#' @param pal String: either a vector of colors (either named R colors or hex codes), or the name of a palette from the `RColorBrewer`, `rcartocolor`, or `viridisLite` packages. If the name of a palette, the respective function will be called to fetch the vector of hex codes.
#' @param dark String giving a single valid color to use as the dark label option, either a named R color or hex code. Default: 'black'
#' @param light String giving a single valid color to use as the light label option, either a named R color or hex code. Default: 'white'
#' @param n For `contrast_colors`, if `pal` is the name of a palette, its corresponding palette will have `n` colors. Ignored otherwise. For `mutate_contrast`, this is determined by the number of levels in `col`. Default: 5
#' @param min_ratio Minimum contrast ratio, below which a pair of colors is deemed low-contrast. If `verbose` is true and any sets of colors fail to meet this minimum, you'll get a diagnostic message letting you know. Default: 4.5
#' @param verbose Logical, whether to print diagnostic info if there are issues with your contrast ratios, Default: TRUE
#' @param labels_only Logical, whether to return only labels, i.e. just a vector of the dark or light values, depending on which had the greatest contrast for each color in `pal`, or the full data frame used for calculations, including all ratios. Default: TRUE
#' @param reverse Boolean: if `TRUE`, palette will be reversed. This mostly only matters for `mutate_contrast`, as it determines which colors are associated with which factor levels. Default: FALSE
#' @param plot Logical, whether to print a `ggplot` swatch of tiles filled with `pal` and labels, using `plot` methods for the appropriate class. Doesn't change the return value. Default: FALSE
#' @param x For methods: An object returned by `contrast_colors`
#' @param ... Not currently used
#' @return
#' * For `contrast_colors`: If `labels_only`, a named character vector of colors, either the value of `dark` or `light`, giving the highest contrast ratio for each value of `pal`. This will be the same length as `pal` (or `n`, if you gave `pal` as the name of a palette to retrieve), and the names will be the fill colors. Otherwise, a data frame with the same number of rows as colors in `pal` and 6 columns of info about them, including fill and label colors (see examples).
#'   - If `labels_only = FALSE`, will also have the classes `contrast_colors` and `cc_df`
#'   - If `labels_only = TRUE`, will also have the classes `contrast_colors` and `cc_vec`.
#' * For `mutate_contrast`, the original data frame given as `data`, with columns added for fill (according to `palette`) and label color.
#' @examples
#' # using a pre-defined palette
#' qual_pal <- c("#009B9E", "#DAA51B", "#C75DAB", "#898DA7", "#2A39A7")
#' contrast_colors(qual_pal)
#'
#' # returning a data frame
#' contrast_colors(qual_pal, labels_only = FALSE)
#'
#' # printing the plot, brewer palette
#' contrast_colors("YlGnBu", plot = TRUE)
#'
#' # not enough contrast, carto palette
#' contrast_colors("Vivid", n = 7, dark = "gray40", plot = TRUE)
#'
#' # Use mutate_contrast to get color information attached to your data frame
#' library(ggplot2)
#'
#' health <- self_rated_health |>
#'     mutate_contrast(col = response, pal = "BuGn")
#'
#' # use same palette for fill scale
#' # lbl_color column has colors to use, so can use identity scale
#' ggplot(health, aes(x = group, y = value, fill = response)) +
#'     geom_col(position = position_fill(reverse = TRUE)) +
#'     geom_text(aes(label = percent100(value), group = group, color = lbl_color),
#'         position = position_fill(reverse = TRUE, vjust = 0.5)
#'     ) +
#'     scale_fill_brewer(palette = "BuGn") +
#'     scale_color_identity()
#'
#' # could actually use scale_fill_identity as well, since fill colors are in data,
#' # but it becomes messy when you want a legend
#'
#' # now use a manual palette, and give the same set of colors to scale_fill_manual
#' manual_pal <- c("#005CE7", "#9C45C7", "#CC6697", "#E69259", "#F2D25C")
#' health_w_pal <- self_rated_health |>
#'     mutate_contrast(col = response, pal = manual_pal)
#'
#' ggplot(health_w_pal, aes(x = group, y = value, fill = response)) +
#'     geom_col(position = position_fill(reverse = TRUE)) +
#'     geom_text(aes(label = percent100(value), group = group, color = lbl_color),
#'         position = position_fill(reverse = TRUE, vjust = 0.5)
#'     ) +
#'     scale_fill_manual(values = manual_pal) +
#'     scale_color_identity()
#' @export
#' @rdname contrast_colors
#' @keywords viz-utils
#' @keywords color
#' @source \url{https://www.w3.org/TR/UNDERSTANDING-WCAG20/visual-audio-contrast-contrast.html}
#' @seealso [colorspace::contrast_ratio()], [RColorBrewer::brewer.pal()], [rcartocolor::carto_pal()], [viridisLite::viridis()]
#'
contrast_colors <- function(pal,
                            dark = "black",
                            light = "white",
                            n = 5,
                            min_ratio = 4.5,
                            verbose = TRUE,
                            labels_only = TRUE,
                            reverse = FALSE,
                            plot = FALSE) {
    # check what pal is (vector of colors, name of brewer, name of carto)
    # can grDevices::col2rgb read it?
    if (is_color(pal)) {
        bg_colors <- pal
    } else if (pal %in% row.names(RColorBrewer::brewer.pal.info)) {
        # if not, is it a palette?
        if (is.null(n) || n < 3) {
            cli::cli_abort("If supplying {.arg pal} as a ColorBrewer palette, {.arg n} must be numeric and >= 3")
        }
        bg_colors <- RColorBrewer::brewer.pal(n = n, name = pal)
    } else if (pal %in% rcartocolor::metacartocolors[["Name"]]) {
        if (is.null(n) || n < 3) {
            # carto_pal actually takes null but I want to be safe for matching with factor levels
            cli::cli_abort("If supplying {.arg pal} as a CARTO palette, {.arg n} must be numeric and >= 3")
        }
        bg_colors <- rcartocolor::carto_pal(n = n, name = pal)
    } else if (pal %in% viridis_pals()) {
        if (is.null(n) || n < 1) {
            cli::cli_abort("If supplying {.arg pal} as a viridis palette, {.arg n} must be numeric and >= 1")
        }
        bg_colors <- viridisLite::viridis(n = n, option = pal)
    } else {
        # otherwise, error
        cli::cli_abort("{.arg pal} should be a vector of colors, or the name of a ColorBrewer, CARTO, or viridis palette.")
    }
    if (reverse) {
        bg_colors <- rev(bg_colors)
    }

    # check dark & light
    if (length(dark) > 1 | !is_color(dark)) {
        cli::cli_abort("{.arg dark} should be a single, valid color.")
    }
    if (length(light) > 1 | !is_color(light)) {
        cli::cli_abort("{.arg light} should be a single, valid color.")
    }

    fg_colors <- c(dark = dark, light = light)
    ratios <- lapply(fg_colors, function(col) {
        round(colorspace::contrast_ratio(col, bg_colors, plot = FALSE), digits = 2)
    })
    ratio_df <- as.data.frame(ratios)
    row.names(ratio_df) <- bg_colors
    ratio_df$max <- pmax(ratios$dark, ratios$light)
    ratio_df$lbl_color <- sapply(seq_len(nrow(ratio_df)), function(i) {
        max <- which.max(ratio_df[i, ])
        unname(fg_colors[max])
    })
    ratio_df <- tibble::rownames_to_column(ratio_df, var = "fill")
    # return vector of highest-contrast colors--length = n
    ratio_df$low_contrast <- ratio_df$max < min_ratio
    if (verbose) {
        if (any(ratio_df$low_contrast)) {
            bullets <- color_info(ratio_df, dark, light)
            cli::cli_warn(c(
                "!" = "At least one set of colors failed to meet the minimum contrast threshold of {min_ratio}.",
                "i" = "Consider adjusting the palette and/or using more distinct label colors.",
                bullets
            ))
        }
    }
    # what to return--just label colors, or full data frame?
    if (labels_only) {
        out <- ratio_df$lbl_color
        names(out) <- bg_colors
        class(out) <- c("contrast_colors", "cc_vec", class(out))
    } else {
        out <- ratio_df
        class(out) <- c("contrast_colors", "cc_df", class(out))
    }
    attr(out, "dark") <- dark
    attr(out, "light") <- light
    if (plot) {
        print(plot(out))
    }
    out
}

#' @param data A data frame
#' @param col Bare name of a column in `x` along which the palette will be mapped. If not already a factor, it will be coerced into one with levels in the same order in which they appear.
#' @export
#' @rdname contrast_colors
mutate_contrast <- function(data, col, pal,
                            dark = "black", light = "white",
                            min_ratio = 4.5, verbose = TRUE,
                            reverse = FALSE) {
    if (!inherits(data, "data.frame")) {
        cli::cli_abort("{.arg data} should be a data frame or tibble.")
    }
    # let contrast_colors handle most errors
    col_vct <- dplyr::pull(data, {{ col }})
    col_name <- rlang::as_label(rlang::enquo(col))

    if (!is.factor(col_vct)) {
        col_vct <- factor(col_vct, levels = unique(col_vct))
    }
    lvls <- levels(col_vct)
    n <- length(lvls)

    # if pal is a vector, should be same length as factor levels
    if (length(pal) > 1) {
        if (length(pal) < n) {
            cli::cli_abort("Too few colors supplied in {.arg pal}. {.arg {col_name}} has {n} levels; {.arg pal} only has {length(pal)} values.")
        }
        if (length(pal) > n) {
            cli::cli_warn("{.arg pal} has {length(pal)} values, but {.arg {col_name}} only has {n} levels. Extra colors will be dropped.")
            pal <- pal[1:n]
        }
    }

    cpal <- contrast_colors(pal, n = n, dark = dark, light = light, labels_only = FALSE, plot = FALSE, reverse = reverse)
    cpal$lvl <- factor(lvls, levels = lvls)
    cpal <- dplyr::select(cpal, fill, lbl_color, lvl)

    join <- stats::setNames("lvl", col_name)
    dplyr::left_join(data, cpal, by = join)
}

######## HELPER FUNCTIONS ----

color_info <- function(color_df, dark, light) {
    idx <- which(color_df$low_contrast)
    bg <- color_df$fill[idx]
    low_df <- color_df[idx, c("dark", "light")]
    # names(low_df) <- c("dark_ratio", "light_ratio")
    txt <- stringr::str_glue_data(low_df, "For {bg}, color #{idx}, ratio to `dark` is {dark}; ratio to `light` is {light}.")
    # cli::cli_ul(txt)
    rlang::set_names(txt, "*")
}

is_color <- function(x) {
    safe_color <- purrr::safely(grDevices::col2rgb, otherwise = NULL)
    res <- safe_color(x)
    return(is.null(res[["error"]]))
}

is_cc_df <- function(x) {
    inherits(x, "data.frame") &
        inherits(x, "cc_df") &
        inherits(x, "contrast_colors") &
        all(c("dark", "light") %in% names(x))
}

is_cc_vec <- function(x) {
    inherits(x, "character") &
        inherits(x, "cc_vec") &
        inherits(x, "contrast_colors") &
        !is.null(names(x))
}

viridis_pals <- function() {
    c("A", "magma", "B", "inferno", "C", "plasma", "D", "viridis", "E", "cividis", "F", "rocket", "G", "mako", "H", "turbo")
}

######## METHODS ----
#### PLOT METHODS ----
#' @rdname contrast_colors
#' @exportS3Method base::plot
#' @export
plot.cc_df <- function(x, ...) {
    if (!is_cc_df(x)) {
        cli::cli_abort("{.arg x} should be of type {.cls cc_df}, as returned by {.fun contrast_colors} with {.arg labels_only = FALSE}")
    }
    dark <- attr(x, "dark")
    light <- attr(x, "light")
    df <- dplyr::select(x, fill, dark, light, max)
    df <- tidyr::pivot_longer(df, cols = dark:light, names_to = "type", values_to = "ratio")
    df$lbl_color <- ifelse(df$type == "dark", dark, light)
    # df$fill <- forcats::as_factor(df$fill)
    # df$lbl_color <- forcats::as_factor(df$lbl_color)
    df$fill <- factor(df$fill, levels = unique(df$fill))
    df$lbl_color <- factor(df$lbl_color, levels = unique(df$lbl_color))
    df$star <- ifelse(df$ratio == df$max, " *", "")
    df$lbl <- sprintf("%02.02f%s", df$ratio, df$star)

    p <- ggplot2::ggplot(df, ggplot2::aes(x = lbl_color, y = forcats::fct_rev(fill)))
    p <- contrast_colors_gg(p)
    p <- p + ggplot2::geom_text(ggplot2::aes(label = lbl, color = lbl_color), fontface = "bold")
    p
}

#' @rdname contrast_colors
#' @exportS3Method base::plot
#' @export
plot.cc_vec <- function(x, ...) {
    if (!is_cc_vec(x)) {
        cli::cli_abort("{.arg x} should be of type {.cls cc_vec}, as returned by {.fun contrast_colors} with {.arg labels_only = TRUE}")
    }
    dark <- attr(x, "dark")
    light <- attr(x, "light")
    df <- data.frame(fill = names(x), lbl_color = x)
    df$fill <- factor(df$fill, levels = df$fill)

    p <- ggplot2::ggplot(df, ggplot2::aes(x = "lbl_color", y = forcats::fct_rev(fill)))
    p <- contrast_colors_gg(p)
    p <- p + ggplot2::geom_text(ggplot2::aes(label = lbl_color, color = lbl_color), fontface = "bold")
    p
}

##### PLOT METHOD HELPERS ----
contrast_colors_gg <- function(p) {
    p <- p + ggplot2::geom_tile(ggplot2::aes(fill = fill), linewidth = 1, color = "white")
    p <- p + ggplot2::scale_fill_identity()
    p <- p + ggplot2::scale_color_identity()
    p <- p + ggplot2::scale_x_discrete(expand = ggplot2::expansion(mult = 0))
    p <- p + ggplot2::scale_y_discrete(expand = ggplot2::expansion(mult = 0))
    p <- p + ggplot2::theme_minimal()
    p <- p + ggplot2::labs(y = "fill")
    p
}

#### PRINT METHODS ----
#' @rdname contrast_colors
#' @exportS3Method base::print
#' @export
print.cc_df <- function(x, ...) {
    print.data.frame(x)
    invisible(x)
}
#' @rdname contrast_colors
#' @exportS3Method base::print
#' @export
print.cc_vec <- function(x, ...) {
    # drop attributes
    x0 <- x
    attributes(x0) <- NULL
    names(x0) <- names(x)
    print(x0)
    invisible(x)
}

