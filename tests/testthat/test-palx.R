test_that("palx returns correct classes", {
  expect_identical(class(palx("#9CCC0C")), c("palx", "list"))
  expect_identical(
    class(palx("#9CCC0C", as_df = TRUE)),
    c("palx", "tbl_df", "tbl", "data.frame"),
  )
})

test_that("as_tibble and plot generics run without error on palx objects", {
  pal <- palx("#9CCC0C")
  expect_true(all.equal(plot_palx(pal), plot(pal)))
  expect_identical(as_tibble(pal), as_tibble.palx(pal))
})

