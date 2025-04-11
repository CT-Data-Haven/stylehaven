test_that("dodge_lbls validates thresh", {
  df <- dummy_dodge()
  expect_s3_class(dodge_lbls(df, x = name, value = value, group = group, thresh = 0.1), "data.frame")
  expect_error(dodge_lbls(df, x = name, value = value, group = group, thresh = -0.1))
})

test_that("dodge_lbls honors verbose argument", {
  df <- dummy_dodge()
  expect_silent(dummy <- dodge_lbls(df, x = name, value = value, group = group, thresh = 0.1, verbose = FALSE))
  expect_message(dummy <- dodge_lbls(df, x = name, value = value, group = group, thresh = 0.1, verbose = TRUE), "rows to dodge")
  expect_message(dummy <- dodge_lbls(df, x = name, value = value, group = group, thresh = 0.001, verbose = TRUE), "0 rows")
})

test_that("dodge_lbls handles combinations of more than 2 close values", {
  df <- dummy_dodge(ngrps = 3)
  expect_s3_class(dodge_lbls(df, x = name, value = value, group = group, thresh = 0.025), "data.frame")
})

test_that("dodge_lbls deduplicates", {
  df2 <- dummy_dodge(nrow = 10, ngrps = 2)
  df3 <- dummy_dodge(nrow = 6, ngrps = 3)
  dodge2 <- dodge_lbls(df2, x = name, value = value, group = group, thresh = 0.05)
  dodge3 <- dodge_lbls(df3, x = name, value = value, group = group, thresh = 0.05)
  
  not_dupe <- function(x1, x2) table(x1, x2) <= 1
  expect_true(all(not_dupe(dodge2$name, dodge2$group)))
  expect_true(all(not_dupe(dodge3$name, dodge3$group)))
})

test_that("calc_thresh returns logical", {
  set.seed(10)
  x1 <- runif(10)
  x2 <- runif(10)
  d <- calc_thresh(x1, x2, 2, 0.01)
  expect_true(inherits(d, "logical"))
})

test_that("calc_thresh returns expected values", {
  x1 <- c(0.10, 0.20, 0.30)
  x2 <- c(0.11, 0.19, 0.35)
  x3 <- c(0.14, 0.16, 0.26)
  expect_identical(calc_thresh(x1, x2, 2, 0.01),
                   c(TRUE, TRUE, FALSE))
  expect_identical(calc_thresh(x1, x2, 2, 0.05),
                   c(TRUE, TRUE, TRUE))
  expect_identical(calc_thresh(x1, x3, 2, 0.02),
                   c(FALSE, FALSE, FALSE))
  expect_identical(calc_thresh(x1, x3, 1, 0.02),
                   c(TRUE, TRUE, TRUE))
})
