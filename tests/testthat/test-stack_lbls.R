test_that("stack_lbls checks lengths of values, just", {
  df <- dummy_stack(4, 1)
  expect_error(stack_lbls(df$value, just = c(0, 0.5, 1)))
  expect_error(stack_lbls(df$value, just = c(0, 0, 0, 1, 1)))
  expect_silent(dummy <- stack_lbls(df$value, just = 0.5))
  expect_silent(dummy <- stack_lbls(df$value, just = rep(0.5, 4)))
})

test_that("stack_lbls checks numeric data type", {
  df <- dummy_stack(4, 1)
  expect_error(stack_lbls(df$group))
})

test_that("stack_lbls calculates correct values", {
  df <- dummy_stack(4, 1)
  # 4 values, all = 1
  v05 <- 0:3 + 0.5
  v00 <- 0:3 + 0
  v10 <- 0:3 + 1
  expect_identical(stack_lbls(df$value, just = 0.5), v05)
  expect_identical(stack_lbls(df$value, just = 0), v00)
  expect_identical(stack_lbls(df$value, just = 1), v10)
})

test_that("stack_lbls handles scaling vs not scaling", {
  n <- 4
  df <- dummy_stack(n, 2) |> dplyr::group_by(group)
  unfill_df <- dplyr::mutate(df, stacked = stack_lbls(value, fill = FALSE, just = 0))
  fill_df <- dplyr::mutate(df, stacked = stack_lbls(value, fill = TRUE, just = 0))
  expect_identical(range(unfill_df$stacked), c(0, n-1))
  expect_identical(range(fill_df$stacked), c(0, 1-(1/n)))
  
  # scaled should yield same values, since they're all the same relative to total
  mult_df <- df |> 
    dplyr::mutate(value = ifelse(group == "a", value, value * 2)) |>
    dplyr::mutate(stacked = stack_lbls(value, fill = TRUE, just = 0))
  expect_identical(mult_df$stacked, fill_df$stacked)
})

test_that("stack_lbls handles NAs", {
  no_na <- dummy_stack(5, 1)
  no_na$value[3] <- 0
  with_na <- no_na
  with_na$value[3] <- NA_real_
  lbl_no_na <- stack_lbls(no_na$value)
  expect_warning(dummy <- stack_lbls(with_na$value))
  suppressWarnings(lbl_with_na <- stack_lbls(with_na$value))
  expect_identical(lbl_no_na, lbl_with_na)
})
