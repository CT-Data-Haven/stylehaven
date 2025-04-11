test_that("palx returns correct classes", {
  col <- "#9CCC0C"
  expect_s3_class(palx(col, as_df = FALSE), c("palx", "list"))
  expect_s3_class(palx(col, as_df = TRUE), c("palx", "data.frame"))
})

test_that("as_tibble and plot generics run without error on palx objects", {
  pal <- palx("#9CCC0C")
  # expect_equal fails because the plots have
  # attributes in different environments
  expect_identical(as_tibble(pal), as_tibble.palx(pal))
  expect_s3_class(plot(pal), "gg")
})

test_that("palx colors always have names", {
  # this color originally had an out-of-bounds color, yielding an unnamed value
  # set.seed(1)
  n_hues <- sample(2:12, size = 5)
  for (n in n_hues) {
    pal <- palx("#9c5fd3", n_hues = n)
    expect_false(any(is.na(names(pal[[1]]))))
  }
})
