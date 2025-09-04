#' Create `cowplot` label based on a ggplot theme
#'
#' This is a wrapper around `cowplot::draw_label()` that creates a `ggplot`-based label that inherits formatting from a given theme element. It's more or less been superceded by `ggplot`'s new `plot.title.position` theme argument.
#' @param label A string of text for label.
#' @param theme A ggplot theme; if `NULL` (the default), will get current theme with `ggplot2::theme_get()`.
#' @param element Name of a theme element; defaults to base text.
#' @param x x-position; defaults to 0.01
#' @param hjust Horizontal alignment; defaults 0
#' @param ... Any other arguments to pass to `cowplot::draw_label()`.
#' @return A `ggplot` object.
#' @examples
#' # TODO
#' @keywords viz-utils
#' @export
themed_label <- function(
    label,
    theme = NULL,
    element = "text",
    x = 0.01,
    hjust = 0,
    ...) {
    if (is.null(theme)) {
        theme <- ggplot2::theme_get()
    }
    # if theme isn't put in as theme(), invoke it
    if (is.function(theme)) {
        theme <- rlang::exec(theme)
    }
    if (!element %in% names(theme)) {
        cli::cli_abort("Element must be a valid ggplot theme element name")
    }

    elements <- ggplot2::calc_element(element, theme)

    lbl <- cowplot::draw_label(
        label,
        fontfamily = elements$family,
        fontface = elements$face,
        colour = elements$color,
        size = elements$size,
        x = x,
        hjust = hjust,
        ...
    )
    cowplot::ggdraw() + lbl
}
