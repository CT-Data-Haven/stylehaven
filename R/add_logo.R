#' @title Easily add a logo to a ggplot
#' @description
#' This function wraps around a few functions from `patchwork` and
#' `magick` to add a logo (or other annotation) to the bottom of a `ggplot` plot,
#' an otherwise tedious and easy to forget process. It's meant to be flexible in
#' the types of objects it can place; as a result, it's less flexible in their
#' placement and customization. For more specific needs, the source of this
#' function should be easy to build upon. Previously it used `cowplot`, but that
#' package is a bit too buggy.
#' @param plot A `ggplot` object onto which the logo will be placed.
#' @param image Either a string giving the path or URL to an image file to be
#' read by `magick::image_read`; the results of reading a file or manipulating
#' an image already with `magick::image_read` or other `magick` functions;
#' a `ggplot` object / grob; or `NULL`, the default. If `NULL`, the image will
#' come from the file at `system.file("extdata/logo.svg", package = "stylehaven")`.
#' As built, this is a logo for DataHaven, but that file can be replaced for
#' repackaging this library for other organizations or projects.
#' @param position String, either "left" or "right", giving the side on which
#' the logo should be aligned. Default: "right"
#' @param height Numeric: the height of the logo, as a percentage of the height
#' of the image given in `plot`. Adjust as necessary based on the dimensions
#' of the logo. Default: 0.05
#' @param width Numeric: the width of the logo, as a percentage of the image given in `plot`. If `NULL`, the default, the width will be calculated based on the aspect ratio of the image. Must be given explicitly for `ggplot` objects, as they don't inherently have their own dimensions.
#' @param place_inside Logical: if `TRUE` (default), logo will be drawn within
#' the plotting area; otherwise, an additional grob is built below the plot,
#' meaning the logo could later be cropped away, which may be undesirable.
#' @return A `ggplot` object.
#' @examples
#' library(ggplot2)
#' p <- ggplot(iris, ggplot2::aes(x = Sepal.Length)) +
#'     geom_density() +
#'     labs(title = "Test chart", caption = "Source: 2019 ACS 5-year estimates") +
#'     theme(plot.caption = element_text(hjust = 0))
#'
#' add_logo(p)
#' # this one is too tall--place it outside so it doesn't block axis
#' add_logo(p,
#'     magick::image_read(system.file("extdata/25th_logo.png", package = "stylehaven")),
#'     height = 0.1, place_inside = FALSE
#' )
#'
#' # This example logo is not all that attractive, but shows how you might
#' # attach a ggplot grob as a dynamically-created logo
#' dummy_data <- data.frame(
#'     town = letters[1:4],
#'     pop = c(21000, 40000, 81000, 36000)
#' )
#'
#' gg_logo <- ggplot(dummy_data, aes(x = town, y = pop)) +
#'     geom_col(width = 0.8, fill = "skyblue") +
#'     annotate(
#'         geom = "text", label = "DataHaven", x = 0.6, y = 6e4, hjust = 0,
#'         family = "mono", size = 5
#'     ) +
#'     theme_void()
#'
#' add_logo(p, gg_logo, width = 0.2, height = 0.1)
#' @export
#' @rdname add_logo
#' @keywords exporting
#' @seealso [magick::image_read()], [patchwork::plot_layout()]
add_logo <- function(plot, image = NULL,
                     position = c("right", "left"),
                     height = 0.05, width = NULL,
                     place_inside = TRUE) {
    position <- rlang::arg_match(position)
    if (!inherits(plot, "gg")) {
        cli::cli_abort("{.arg plot} should be a {.cls ggplot}-type object.")
    }
    # if outside, needs wrapped grob (patch)
    # if inside, just needs grob
    if (is.null(image)) {
        image <- system.file("extdata/logo.svg", package = "stylehaven")
    }
    if (inherits(image, "character")) {
        # path to image
        image <- magick::image_read(image)
    } else if (inherits(image, c("gg", "grob"))) {
        if (is.null(width)) {
            cli::cli_abort("For an {.arg image} argument of class {.cls gg}, {.arg width} needs to be supplied explicitly.")
        }
    } else if (inherits(image, "magick-image")) {

    } else {
        cli::cli_abort(c(
            "{.arg image} should be a {.cls character}, {.cls magick-image}, or {.cls gg} object, or {.val NULL}",
            "i" = "You've supplied an object of class {.cls {class(image)}}."
        ))
    }

    if (place_inside) {
        logo <- make_logo_grob_inside(image, position, height, width)
        out <- plot + logo
    } else {
        logo <- make_logo_grob_outside(image, position, width)
        out <- (plot + logo) +
            patchwork::plot_layout(ncol = 1, heights = c(1 - height, height))
    }
    out
}


# for inside placement
calc_img_width <- function(img_ptr, h_out) {
    info <- magick::image_info(img_ptr)
    # w_in / h_in = w_out / h_out; solve for w_out
    (info$width / info$height) * h_out
}


make_logo_grob_inside <- function(img, position, height, width) {
    if (is.null(width)) {
        width <- calc_img_width(img, height)
    }
    if (position == "right") {
        right <- 1
        left <- right - width
    } else {
        left <- 0
        right <- left + width
    }
    bottom <- 0
    top <- bottom + height
    if (!inherits(img, c("gg", "grob"))) {
        img <- grid::rasterGrob(img)
    }
    # img_grob <- grid::rasterGrob(img)
    patchwork::inset_element(img,
        top = top, right = right, bottom = bottom, left = left,
        align_to = "full", clip = FALSE, on_top = TRUE
    )
}

make_logo_grob_outside <- function(img, position, width) {
    # needs spacer
    if (position == "right") {
        x <- grid::unit(1, "npc")
        hjust <- 1
    } else {
        x <- grid::unit(0, "npc")
        hjust <- 0
    }
    # grob keeps its aspect ratio--only need to give height
    if (!inherits(img, c("gg", "grob"))) {
        img_grob <- grid::rasterGrob(img, x = x, hjust = hjust, height = grid::unit(1, "npc"), width = width)
    }
    img_el <- patchwork::wrap_elements(full = img_grob, clip = FALSE)
    if (position == "right") {
        out <- (patchwork::plot_spacer() | img_el)
    } else {
        out <- (img_el | patchwork::plot_spacer())
    }
    if (!is.null(width)) {
        out <- out + patchwork::plot_layout(widths = c(1 - width, width))
    }
    out
}
