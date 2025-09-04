test_that("themed_label handles theme argument", {
  el1 <- themed_label("the title", element = "plot.title")
  el2 <- themed_label("the title", theme = ggplot2::theme_void(), element = "plot.title")

  expect_s3_class(el1, "gg")
  expect_s3_class(el2, "gg")
})

test_that("themed_label handles invalid element name", {
  expect_error(themed_label("caption", element = "source"), "valid ggplot theme")
})

test_that("themed_label handles theme as function", {
  el1 <- themed_label("the caption", theme = ggplot2::theme_classic, element = "plot.caption")
  el2 <- themed_label("the caption", theme = ggplot2::theme_classic(), element = "plot.caption")

  expect_s3_class(el1, "gg")
  expect_s3_class(el2, "gg")
})
