test_that("scale_barcontinuous correctly passes expansion args", {
  plots <- make_barcont_plots()
  ranges <- dplyr::select(plots, id, range) |>
    tibble::deframe()
  # value range: 0-6
  # if padded at bottom, lowest y isn't 0
  expect_true(min(ranges$vert_unscale$y) > 0)
  expect_equal(min(ranges$vert_scale$y), 0)
  expect_true(min(ranges$horiz_unscale$x) > 0)
  expect_equal(min(ranges$horiz_scale$x), 0)
  
  # unscaled goes to 6, scaled goes higher (artificially padded for testing)
  expect_equal(max(ranges$vert_unscale$.value), 6)
  expect_true(max(ranges$vert_scale$.value) > 6)
})

test_that("scale_barcontinuous passes ... args", {
  p <- test_plot(type = "bar") +
    scale_y_barcontinuous(labels = scales::label_dollar())
  range <- ggplot2::get_guide_data(p, "y")
  expect_true(all(range$.label == paste0("$", range$.value)))
})
