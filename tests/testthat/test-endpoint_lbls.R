test_that("endpoint_lbls checks fun arg", {
  # 0, 0.5, 1
  pct_df <- dplyr::bind_rows("1" = dummy_pct(), "2" = dummy_pct(), .id = "x") |>
    dplyr::mutate(x = as.numeric(x))
  
  def_df <- endpoint_lbls(pct_df, x = x, value = value, group = name, long_side = "none")
  label_df <- endpoint_lbls(pct_df, x = x, value = value, group = name, fun = scales::label_percent(), long_side = "none")
  anon_df <- endpoint_lbls(pct_df, x = x, value = value, group = name, fun = \(x) paste(x * 100, "pct"), long_side = "none")
  
  expect_identical(unique(def_df$lbl), c("0", "0.5", "1"))
  expect_identical(unique(label_df$lbl), c("0%", "50%", "100%"))
  expect_identical(unique(anon_df$lbl), c("0 pct", "50 pct", "100 pct"))
  expect_error(endpoint_lbls(pct_df, x = x, value = value, group = name, fun = "percent100"))
})

test_that("endpoint_lbls checks mult & add args", {
  expect_error(endpoint_lbls(cws_trend, x = year, value = value, group = group, mult = NULL, add = NULL), "Must supply")
  df_mult <- endpoint_lbls(cws_trend, x = year, value = value, group = group, mult = 0.1)
  expect_message(df_both <- endpoint_lbls(cws_trend, x = year, value = value, group = group, mult = 0.1, add = 0.05), "Both .+ were supplied")
  df_both <- suppressMessages(endpoint_lbls(cws_trend, x = year, value = value, group = group, mult = 0.1, add = 0.05))
  expect_identical(df_mult, df_both)
})

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

test_that("endpoint_lbls correctly handles long side", {
  # just 0, 1; groups are A, B, C
  pct_df <- dplyr::bind_rows("1" = dummy_pct(nrow = 2), "2" = dummy_pct(nrow = 2), .id = "x") |>
    dplyr::mutate(x = as.numeric(x))
  df_left <- endpoint_lbls(pct_df, x, value, group = name, long_side = "left")
  df_right <- endpoint_lbls(pct_df, x, value, group = name, long_side = "right")
  df_both <- endpoint_lbls(pct_df, x, value, group = name, long_side = "both")
  df_none <- endpoint_lbls(pct_df, x, value, group = name, long_side = "none")
  
  expect_identical(df_left$lbl,  c("A: 0", "B: 1", "0", "1"))
  expect_identical(df_right$lbl, c("0", "1", "A: 0", "B: 1"))
  expect_identical(df_both$lbl,  c("A: 0", "B: 1", "A: 0", "B: 1"))
  expect_identical(df_none$lbl,  c("0", "1", "0", "1"))
})

test_that("endpoint_lbls matches just with side", {
  df <- endpoint_lbls(cws_trend, x = year, value = value, group = group) |>
    dplyr::distinct(year, just)
  min_yr <- min(df$year); max_yr <- max(df$year)
  # min year is right-just
  expect_equal(df$just[df$year == min_yr], 1)
  expect_equal(df$just[df$year == max_yr], 0)
})
