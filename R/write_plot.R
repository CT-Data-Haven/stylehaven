#' @title Batch save plots to multiple formats.
#' @description This is a formalized version of a function I often have in utils scripts for different projects. It works well when paired with `purrr::map`, taking a list of plots and a list of parameters for each (width, height, filename), and saving each to both PDF (good as a vector format) and PNG (good as a decent resolution bitmap format) files using `ggplot2::ggsave`. By default, this will add a logo to the bottom of the plot using [`add_logo`]—look at the documentation to see exactly how to pass a logo. Note that some devices might act differently: for example, SVG devices might have some quirks.
#' @param plot A `ggplot` object to save
#' @param filename String, the filename without extension, e.g. `"cool_plot"` rather than `"cool_plot.png"`, since extensions will be added based on the `devs` argument.
#' @param width Numeric, width of saved plot in inches. Default: 7
#' @param height Numeric, height of saved plot in inches. Doesn't include any additional height for the logo. Default: 4.5
#' @param add_logo Logical: if `TRUE` (default), a logo will be added to each plot using `add_logo`.
#' @param use_abs_logo Logical, whether the logo size is being supplied as absolute (if `TRUE`) or relative to the plot height (if `FALSE`). If `TRUE` (default), the logo will be the height given in `logo_abs` in inches; otherwise, the logo will be sized as the ratio to the plot height as given in `logo_scale`. For batch writing plots, using an absolute size is probably best so the logo is the same size on all charts, regardless of each chart's height.
#' @param logo_abs The absolute height for the logo, in inches. Ignored if `use_abs_logo = FALSE`. Default: 0.15
#' @param logo_scale The ratio of the logo's height to the plot's height. Ignored if `use_abs_logo = TRUE`. Default: 0.05
#' @param logo_img If `NULL` (the default), the logo will be the image saved at `system.file("extdata/logo.svg", package = "stylehaven")`, which is a copy of DataHaven's logo. See [`stylehaven::add_logo`] for other ways to create and supply a logo, with one exception: this function won't handle `ggplot` objects as logos the way `add_logo` can.
#' @param logo_pos String, which side of the plot the logo should be placed on. Default: 'right'
#' @param place_inside Logical, whether the logo should be placed on the inside of the chart or the outside (default). Note that this is the opposite of the default in `add_logo`—often placing the logo inside the chart works well, but for batch writing it's probably safer to either default to outside placement, or use a parameter for this alongside the width & height.
#' @param dir Directory in which to write files. If `NULL`, the default, this will just be the working directory, as is the case with `ggplot2::ggsave`.
#' @param separate_dirs Logical, whether to save files into separate directories based on file type. If `TRUE` (default), the file `"cool_plot.png"` will be written to `"dir/png/cool_plot.png"`; otherwise, the file will be written just in `dir`. If the necessary directories don't already exist, they'll be created.
#' @param devs A named list, where the names are the extensions to use for output files and the values specify graphics devices to use, either as strings or functions (or a mix). See the `device` argument of [`ggplot2::ggsave()`] for examples. The defaults, `c(pdf = cairo_pdf, png = ragg::agg_png)`, are the ones that I've found to work well for writing out `ggplot`s, particularly high-resolution ones with custom fonts.
#' @param verbose Logical, whether to print the path to each file after it's written (defaults `TRUE`).
#' @param ... Additional arguments to pass on to `ggplot2::ggsave`, which in turn passes them to graphics devices.
#' @return Returns nothing. If `verbose = TRUE`, sends a message to the console.
#' @keywords exporting
#' @examples
#' \dontrun{
#' library(ggplot2)
#' plot1 <- ggplot(mtcars, aes(x = hp)) +
#'     geom_density()
#' write_plot(plot1, "hp_density")
#' write_plot(plot1, "hp_density", devs = list(png = "png", svg = svg))
#' ggsave("svg/plot.svg", plot1, device = svglite::svglite)
#'
#' # how I usually use this for a whole document of plots
#' plots <- list()
#' plots[["mpg_histogram"]] <- ggplot(mtcars, aes(x = mpg)) +
#'     geom_histogram()
#' plots[["hp_vs_mpg"]] <- ggplot(mtcars, aes(x = hp, y = mpg)) +
#'     geom_point()
#' plot_params <- list(
#'     mpg_histogram = list(w = 7, h = 4),
#'     hp_vs_mpg = list(w = 5, h = 5)
#' )
#' purrr::imap(plots, function(plt, id) {
#'     # using plot ID as the filename
#'     params <- plot_params[[id]]
#'     write_plot(plt,
#'         filename = id,
#'         width = params$w,
#'         height = params$h,
#'         logo_abs = 0.2
#'     )
#' })
#' }
#' @export
#' @seealso [add_logo()], [ggplot2::ggsave()]
write_plot <- function(plot,
                       filename,
                       width = 7,
                       height = 4.5,
                       add_logo = TRUE,
                       use_abs_logo = TRUE,
                       logo_abs = 0.15,
                       logo_scale = 0.05,
                       logo_img = NULL,
                       logo_pos = "right",
                       place_inside = FALSE,
                       dir = NULL,
                       separate_dirs = TRUE,
                       devs = list(pdf = grDevices::cairo_pdf, png = ragg::agg_png),
                       verbose = TRUE, ...) {
    # test graphics devices--use docker
    if (is.null(dir)) {
        dir <- "."
    }
    if (add_logo) {
        # if abs logo, calculate logo scale
        if (use_abs_logo) {
            logo_scale <- logo_abs / height
        } else {
            logo_abs <- logo_scale * height
        }
        if (place_inside) {
            height_out <- height
        } else {
            height_out <- height + logo_abs
        }
        plot_out <- stylehaven::add_logo(plot, image = logo_img, position = logo_pos, height = logo_scale, place_inside = place_inside)
    } else {
        height_out <- height
        plot_out <- plot
    }

    # afaict this actually gets handled before this point by ggplot
    # font <- plot$theme$text$family
    # if (!is.null(font) && !(font %in% sysfonts::font_families()) & verbose) {
    #   cli::cli_warn("You're using the font {font}, but don't seem to have it installed.",
    #                 "i" = "Consider adding it with {.fun stylehaven::font_add_weights} or {.fun sysfonts::font_add}.")
    # }

    if (verbose) cli::cli_ul()

    purrr::iwalk(devs, function(dev, ext) {
        fn <- paste(filename, ext, sep = ".")
        if (separate_dirs) {
            dir_out <- file.path(dir, ext)
            if (!dir.exists(dir_out)) {
                dir.create(dir_out)
            }
        } else {
            dir_out <- file.path(dir)
        }
        file_out <- file.path(dir_out, fn)

        suppressWarnings(ggplot2::ggsave(file_out, plot = plot_out, device = dev, width = width, height = height_out, bg = "white", ...))
        if (verbose) {
            cli::cli_li("{.file {file_out}} saved")
        }
    })

    if (verbose) cli::cli_end()

    invisible(NULL)
}
