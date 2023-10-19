test_that("endpoint_lbls correctly calculates ratios", {
  df_mult <- endpoint_lbls(cws_trend, x = year, value = value, group = group, mult = 0.1)
  df_add  <- endpoint_lbls(cws_trend, x = year, value = value, group = group, mult = NULL, add = 0.1)
  df_def <- endpoint_lbls(cws_trend, x = year, value = value, group = group)
  # 2015-2021
  x_mult <- unique(df_mult$x)
  x_add  <- unique(df_add$x)
  x_def  <- unique(df_def$x)
  range <- diff(range(cws_trend$year))
  expect_equal(x_mult, c(2015 - (0.1 * range), 2021 + (0.1 * range)))
  expect_equal(x_add,  c(2015 - 0.1,           2021 + 0.1))
  expect_equal(x_def,  c(2015 - (0.05 * range), 2021 + (0.05 * range)))
})
