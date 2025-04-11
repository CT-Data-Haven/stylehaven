test_that("add_logo handles different image types", {
  p <- test_plot()
  def <- NULL
  url <- "https://placehold.co/200x50"
  path <- system.file("extdata/logo.svg", package = "stylehaven")
  magic <- magick::logo
  grob <- grid::rectGrob()
  gg <- test_plot(type = "bar") + ggplot2::theme(plot.background = ggplot2::element_rect(fill = "pink"))
  
  expect_s3_class(add_logo(p, def), "gg")
  expect_s3_class(add_logo(p, url), "gg")
  expect_s3_class(add_logo(p, path), "gg")
  expect_s3_class(add_logo(p, magic), "gg")
  expect_s3_class(add_logo(p, gg, width = 0.2), "gg")
  expect_s3_class(add_logo(p, grob, width = 0.2), "gg")
})

test_that("add_logo checks plot, image types", {
  p <- test_plot()
  img <- magick::logo
  expect_error(add_logo(p, image = TRUE))
  expect_error(add_logo(img))
})

test_that("add_logo requires width for gg, grob logo", {
  p <- test_plot()
  grob <- grid::rectGrob()
  gg <- test_plot(type = "bar") + ggplot2::theme(plot.background = ggplot2::element_rect(fill = "pink"))
  expect_error(add_logo(p, grob), "needs to be supplied explicitly")
  expect_error(add_logo(p, gg), "needs to be supplied explicitly")
})

test_that("add_logo handles placement & position arguments", {
  p <- test_plot()
  img <- magick::logo
  
  expect_s3_class(add_logo(p, img, place_inside = TRUE,  position = "left"), "gg")
  expect_s3_class(add_logo(p, img, place_inside = FALSE, position = "left"), "gg")
  expect_s3_class(add_logo(p, img, place_inside = TRUE,  position = "right"), "gg")
  expect_s3_class(add_logo(p, img, place_inside = FALSE, position = "right"), "gg")
})

test_that("make_logo_grob_inside calculates width if not supplied", {
  p <- test_plot()
  img <- "https://placehold.co/200x50" # use since it's got exact measurements
  h <- 0.2
  p_inside <- add_logo(p, img, height = h, place_inside = TRUE)
  p_info <- attr(p_inside, "inset_settings")
  w <- as.numeric(p_info$right - p_info$left)
  expect_equal(w / h, 200 / 50)
})

test_that("add_logo will override if supplied width", {
  p <- test_plot()
  img <- "https://placehold.co/200x50" # use since it's got exact measurements
  h <- 0.2
  w <- (200 / 50) * h
  p_with_arg <- add_logo(p, img, height = h, place_inside = TRUE, width = 1)
  p_wo_arg <- add_logo(p, img, height = h, place_inside = TRUE, width = NULL)
  
  p_with_info <- attr(p_with_arg, "inset_settings")
  p_wo_info <- attr(p_wo_arg, "inset_settings")
  
  expect_equal(as.numeric(p_with_info$right - p_with_info$left), 1)
  expect_equal(as.numeric(p_wo_info$right - p_wo_info$left), w)
})

test_that("add_logo makes reasonable looking plots", {
  p <- test_plot() +
    ggplot2::labs(caption = "Source: blah blah")
  
  skip_on_ci()
  expect_snapshot_output(add_logo(p, position = "left", place_inside = TRUE, height = 0.03))
  expect_snapshot_output(add_logo(p, position = "right", place_inside = FALSE))
})
