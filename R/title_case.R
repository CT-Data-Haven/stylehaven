#' @title Title casing
#' @description This function is a wrapper around [clean_titles()] which corrects for the fact that not *all* words in a title should be capitalized (articles, prepositions, etc. not at the beginning of the title). It follows the APA style guide the best I could, but there may be exceptions to fix manually (see examples).
#' @param x A character vector to alter
#' @param lower Aside from a built-in list of articles and prepositions, any additional words that should be kept lowercase.
#' @param clean Boolean: if `TRUE` (the default), strings will be passed to `clean_titles` as an intermediate step, mostly to break snakecased text into words before converting to title casing.
#' @inheritDotParams clean_titles
#' @return A character vector
#' @examples
#' title_case(c("rates are rising in the area", "of all adults in the region"))
#' title_case("rates are going up in the area")
#' # This result is technically incorrect--"up" is used as an adverb here, and
#' # under APA should be capitalized. Write this one out manually.
#' @keywords string-formatting
#' @seealso [clean_titles()]
#' @export
title_case <- function(x, lower = NULL, clean = TRUE, ...) {
    # using APA style guide
    # https://titlecaseconverter.com/words-to-capitalize/
    lwr <- stringr::str_to_sentence(c("a", "an", "and", "as", "at", "but", "by", "for", "if", "in", "nor", "of", "off", "on", "out", "per", "so", "the", "til", "to", "up", "via", "v", "vs", "yet", lower))
    patt <- sprintf("(?<=.)\\b(%s)\\b", paste(lwr, collapse = "|"))

    if (clean) {
        x <- clean_titles(x, ...)
    }

    # x1 <- stringr::str_to_title(x)
    # don't want to override capitalization from clean_titles
    x1 <- gsub("(?<!')\\b([a-z])", "\\U\\1", x, perl = TRUE)
    gsub(patt, "\\L\\1", x1, perl = TRUE)
}
