#' @title Register a set of 4 font weights for use with {showtext}
#' @description Sometimes in a longer document with lots of charts, it's good to limit how much text is very bold and instead make use of semibold font faces. `font_add_weights` registers a pair of fonts from Google Fonts. They're from the same family, but one will have a typical set of weights (by default, regular = 400 and bold = 700), and the other will have bolder versions (default regular = 600, bold = 900). If a weight is unavailable, nothing will be registered and a message will print with the weights available for that font family.
#'
#' @param name Name of the font to search in Google Fonts. This function will register one family with this name, and another family with that name appended by "Semibold."
#' @param regular Numeric, defaults 400
#' @param semibold Numeric, defaults 600
#' @param bold Numeric, defaults 700
#' @param black Numeric, defaults 900
#' @return Returns nothing, but registers fonts with `sysfonts::font_add_google`. Call `sysfonts::font_families()` to confirm that the font family is loaded.
#' @details Font weights are from CSS styling, where values are multiples of 100 between 100 and 900. The defaults used here are the norms used in web typography. Not all fonts will be available in all the weights you might like, but many of the fonts on Google that are well suited to data visualization come in many weights.
#' @examples
#' if (interactive()) {
#' library(ggplot2)
#' font_add_weights("Source Sans Pro", black = 800)
#' showtext::showtext_auto()
#' ggplot(iris, aes(x = Sepal.Width)) +
#'   geom_histogram(binwidth = 0.25) +
#'   facet_wrap(vars(Species), nrow = 1) +
#'   labs(title = "Font weights in use are 400, 600, and 700",
#'        subtitle = "Sepal width by species") +
#'   theme_gray(base_family = "Source Sans Pro") +
#'   theme(plot.title = element_text(family = "Source Sans Pro", face = "bold"), # 700
#'         plot.subtitle = element_text(family = "Source Sans Pro Semibold"), # 600
#'         strip.text = element_text(family = "Source Sans Pro Semibold")) # 600
#' }
#' @export
#' @keywords function
#' @seealso sysfonts::font_add_google
font_add_weights <- function(name, regular = 400, semibold = 600, bold = 700, black = 900) {
  wts <- stats::setNames(c(regular, semibold, bold, black), c("regular", "semibold", "bold", "black"))
  defaults <- list(regular = 400, semibold = 600, bold = 700, black = 900)

  avail <- sysfonts::font_info_google(db_cache = FALSE)
  avail <- dplyr::mutate(avail, variants = gsub("regular", "400", variants))
  avail <- tidyr::separate_rows(avail, variants, sep = ", ")
  avail <- dplyr::filter(avail, family == name)
  avail <- dplyr::filter(avail, !grepl("[a-z]", variants))
  avail <- as.numeric(avail$variants)

  if (length(avail) < 1) {
    cli::cli_abort("{name} not found in the fonts database. Double check the name & spelling.")
  }

  is_unavail <- purrr::map_lgl(wts, function(x) !x %in% avail)
  unavail <- wts[is_unavail]
  if (sum(is_unavail) > 0) {
    cli::cli_abort(c(
      "The following weights are unavailable for this font: {.val unavail}.",
      "i" = "This font comes in the following weights: {.val avail}."
    ))
  }

  sb_name <- paste(name, "Semibold")
  cli::cli_alert_info("Registering the following fonts:")
  cli::cli_ul(c(
    "{.strong {name}} with regular weight {regular} and bold weight {bold}",
    "{.strong {sb_name}} with semibold weight {semibold} and black weight {black}"
  ))

  sysfonts::font_add_google(name, family = name, regular.wt = regular, bold.wt = bold)
  sysfonts::font_add_google(name, family = sb_name, regular.wt = semibold, bold.wt = black)
}
