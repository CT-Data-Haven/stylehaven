library(testthat)

test_that("add_logo with url as logo returns ggplot", {
  set.seed(123)
  town_df <- dplyr::tibble(name = letters[1:5], value = runif(length(5)))
  p <- ggplot2::ggplot(town_df, ggplot2::aes(x = name, y = value)) +
    ggplot2::geom_col()
  logo <- "https://placehold.co/200x50"
  p_out <- add_logo(p, logo)

  expect_s3_class(p_out, "gg")
})

test_that("add_logo with img as logo returns ggplot", {
  set.seed(123)
  town_df <- dplyr::tibble(name = letters[1:5], value = runif(length(5)))
  p <- ggplot2::ggplot(town_df, ggplot2::aes(x = name, y = value)) +
    ggplot2::geom_col()
  logo <- system.file("extdata/logo.svg", package = "stylehaven")
  p_out <- add_logo(p, logo)

  expect_s3_class(p_out, "gg")
})

test_that("add_logo with null as logo returns ggplot", {
  set.seed(123)
  town_df <- dplyr::tibble(name = letters[1:5], value = runif(length(5)))
  p <- ggplot2::ggplot(town_df, ggplot2::aes(x = name, y = value)) +
    ggplot2::geom_col()
  p_out <- add_logo(p)

  expect_s3_class(p_out, "gg")
})
