test_that("palx returns correct classes", {
  expect_identical(class(palx("#9CCC0C")), c("palx", "list"))
  expect_identical(
    class(palx("#9CCC0C", as_df = TRUE)),
    c("palx", "tbl_df", "tbl", "data.frame"),
  )
})

test_that("as_tibble and plot generics run without error on palx objects", {
  pal <- palx("#9CCC0C")
  # expect_equal fails because the plots have
  # attributes in different environments
  expect_identical(as_tibble(pal), as_tibble.palx(pal))

  skip("need to figure out environments for comparing ggplots")
  expect_true(all.equal(plot_palx(pal), plot(pal)))
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
