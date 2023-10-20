test_that("stdev_brks returns correct dimensions", {
  df_calc <- stdev_brks(life_exp, keep_calcs = TRUE)
  df_drop <- stdev_brks(life_exp, keep_calcs = FALSE)
  expect_equal(nrow(df_calc), nrow(life_exp))
  expect_equal(nrow(df_drop), nrow(life_exp))
  expect_equal(ncol(df_calc), ncol(life_exp) + 4)
  expect_equal(ncol(df_drop), ncol(life_exp) + 1)
})

test_that("stdev_brks retains groups", {
  df_grp <- fin_insecurity |>
    dplyr::group_by(question) |>
    stdev_brks()
  df_not <- fin_insecurity |>
    stdev_brks(by = "question")
  expect_true(dplyr::is_grouped_df(df_grp))
  expect_false(dplyr::is_grouped_df(df_not))
  expect_identical(dplyr::ungroup(df_grp), df_not)
})

test_that("stdev_brks handles filters", {
  df <- dplyr::group_by(fin_insecurity, question)
  f_right <- list(category = "Connecticut", group = "Connecticut")
  # too many values
  f_warn <- list(group = c("Connecticut", "Greater New Haven"))
  # no names
  f_err1 <- list("Connecticut")
  # vector instead of list
  f_err2 <- c(group = "Connecticut")
  # dupes
  f_err3 <- list(category = "Gender")
  expect_s3_class(stdev_brks(df, filters = f_right), "data.frame")
  expect_warning(stdev_brks(df, filters = f_warn))
  expect_error(stdev_brks(df, filters = f_err1))
  expect_error(stdev_brks(df, filters = f_err2))
  expect_error(stdev_brks(df, filters = f_err3))
})
