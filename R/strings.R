#' @title String utilities
#' @description This is a collection of a few functions for common string transformations.
#' @param x A character vector to alter
#' @param lower Aside from a built-in list of articles and prepositions, any additional words that should be kept lowercase.
#' @details `title_case` is a wrapper around `stringr::str_to_title`, which corrects for the fact that not *all* words in a title should be capitalized (articles, prepositions, etc. not at the beginning of the title). It follows the APA style guide the best I could, but there may be exceptions to fix manually (see examples).
#' @return A character vector
#' @examples
#' title_case(c("Rates are rising in the area", "Of all adults in the region"))
#' title_case("Rates are going up in the area") # This result is technically
#' # incorrect--"up" is used as an adverb here, and under APA should be
#' # capitalized. Write this one out manually.
#' @export
title_case <- function(x, lower = NULL) {
# using APA style guide
# https://titlecaseconverter.com/words-to-capitalize/
  lwr <- stringr::str_to_sentence(c("a", "an", "and", "as", "at", "but", "by", "for", "if", "in", "nor", "of", "off", "on", "out", "per", "so", "the", "til", "to", "up", "via", "v", "vs", "yet", lower))
  patt <- sprintf("(?<=.)\\b(%s)\\b", paste(lwr, collapse = "|"))
  x1 <- stringr::str_to_title(x)
  gsub(patt, "\\L\\1", x1, perl = TRUE)
}

