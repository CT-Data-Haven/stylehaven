test_that("contrast_colors matches multiple types of palettes", {
  qual_pal <- rcartocolor::carto_pal(n = 6, name = "Bold")
  p1 <- contrast_colors(qual_pal)
  # brewer
  p2 <- contrast_colors("Spectral")
  # carto
  p3 <- contrast_colors("Bold", n = 6)
  # virids
  p4 <- contrast_colors("magma")
  expect_s3_class(p1, "cc_vec")
  expect_s3_class(p2, "cc_vec")
  expect_s3_class(p3, "cc_vec")
  expect_s3_class(p4, "cc_vec")
  expect_identical(p3, p1)
  expect_error(contrast_colors("blah"))
})

test_that("contrast_colors enforces minimum palette sizes", {
  expect_error(contrast_colors("Spectral", n = NULL))
  # brewer
  expect_error(contrast_colors("Spectral", n = 1))
  # carto
  expect_error(contrast_colors("Bold", n = 2))
  # viridis
  expect_error(contrast_colors("magma", n = 0))
  
  # n doesn't matter if pal is a vector
  qual_pal <- rcartocolor::carto_pal(n = 6, name = "Bold")
  expect_length(contrast_colors(qual_pal, n = NULL), length(qual_pal))
})

test_that("contrast_colors checks dark, light lengths & types", {
  expect_error(contrast_colors("Spectral", dark = "dark"))
  expect_error(contrast_colors("Spectral", light = "light"))
  expect_error(contrast_colors("Spectral", dark = c("gray10", "gray20")))
  expect_error(contrast_colors("Spectral", light = c("gray80", "gray90")))
})

test_that("contrast_colors matches dimensions", {
  qual_pal <- rcartocolor::carto_pal(n = 6, name = "Bold")
  p1 <- contrast_colors("Bold", n = 3, labels_only = TRUE)
  p2 <- contrast_colors("Bold", n = 3, labels_only = FALSE)

  expect_length(p1, 3)
  expect_equal(dim(p2), c(3, 6))
  # warn for too many colors--handled by underlying function
  # viridis allows more colors than it should
  expect_warning(contrast_colors("Spectral", n = 15), "allowed maximum for palette")
  expect_warning(contrast_colors("Bold", n = 15))
  expect_silent(contrast_colors("inferno", n = 15))
})

test_that("contrast_colors alerts for low contrast", {
  expect_warning(dummy <- contrast_colors("Greys", dark = "gray"))
})

test_that("contrast_colors calls plot methods", {
  expect_silent(dummy <- contrast_colors("Bold", verbose = FALSE, plot = FALSE, labels_only = TRUE))
  expect_silent(dummy <- contrast_colors("Bold", verbose = FALSE, plot = FALSE, labels_only = FALSE))
  
  skip_on_ci()
  expect_snapshot_output(dummy <- contrast_colors("Bold", verbose = FALSE, plot = TRUE, labels_only = TRUE))
  expect_snapshot_output(dummy <- contrast_colors("Bold", verbose = FALSE, plot = TRUE, labels_only = FALSE))
})

test_that("plot.contrast_colors return ggplot objects", {
  cc_vec <- contrast_colors("GnBu", labels_only = TRUE)
  cc_df <- contrast_colors("GnBu", labels_only = FALSE)
  gg_vec <- plot(cc_vec)
  gg_df <- plot(cc_df)
  expect_s3_class(gg_vec, "gg")
  expect_s3_class(gg_df, "gg")
})

test_that("plot.contrast_colors check class", {
  expect_error(plot.cc_df(iris))
  expect_error(plot.cc_vec(letters))
})

test_that("mutate_contrast checks data types", {
  df <- dummy_pct(nrow = 20)
  df$brk <- ggplot2::cut_number(df$value, n = 4)
  df$char <- as.character(df$brk)
  
  # wrong type
  expect_error(mutate_contrast(df$value, df$value, "GnBu"), "data frame or tibble")
  
  # ok if col is factor
  expect_s3_class(mutate_contrast(df, brk, "GnBu"), "data.frame")
  # ok if col not factor
  expect_s3_class(mutate_contrast(df, char, "GnBu"), "data.frame")
})

test_that("mutate_contrast checks palette lengths", {
  df <- dummy_pct(nrow = 20)
  n_brk <- 4
  df$brk <- ggplot2::cut_number(df$value, n = n_brk)
  pal <- RColorBrewer::brewer.pal(n_brk, "GnBu")
  pal_long <- RColorBrewer::brewer.pal(n_brk + 3, "GnBu")
  pal_short <- RColorBrewer::brewer.pal(n_brk - 1, "GnBu")
  df1 <- mutate_contrast(df, brk, pal, verbose = FALSE)
  df2 <- mutate_contrast(df, brk, "GnBu", verbose = FALSE)
  expect_s3_class(df1, "data.frame")
  expect_s3_class(df2, "data.frame")
  expect_identical(df1, df2)
  # pal too long
  expect_warning(mutate_contrast(df, brk, pal = pal_long, verbose = FALSE))
  # pal too short
  expect_error(mutate_contrast(df, brk, pal = pal_short, verbose = FALSE))
})

test_that("mutate_contrast inherits messages from contrast_colors", {
  df <- dummy_pct(nrow = 20)
  df$brk <- ggplot2::cut_number(df$value, n = 4)
  expect_warning(mutate_contrast(df, brk, pal = "Greys", dark = "gray"))
})

test_that("mutate_contrast gets same output as contrast_colors", {
  # checking bc I wasn't actually passing args
  df <- dummy_pct(nrow = 20)
  n_brk <- 4
  df$brk <- ggplot2::cut_number(df$value, n = n_brk)
  pal <- RColorBrewer::brewer.pal(n_brk, "GnBu")
  cc_mute1 <- mutate_contrast(df, brk, pal) |>
    dplyr::distinct(fill, lbl_color) |>
    dplyr::arrange(fill)
  cc_cont1 <- contrast_colors(pal, n = n_brk, labels_only = FALSE) |>
    dplyr::distinct(fill, lbl_color) |>
    dplyr::arrange(fill)
  expect_identical(cc_mute1, as.data.frame(cc_cont1))
  
  cc_mute2 <- mutate_contrast(df, brk, pal, dark = "gray10", light = "gray95") |>
    dplyr::distinct(fill, lbl_color) |>
    dplyr::arrange(fill)
  cc_cont2 <- contrast_colors(pal, n = n_brk, labels_only = FALSE, dark = "gray10", light = "gray95") |>
    dplyr::distinct(fill, lbl_color) |>
    dplyr::arrange(fill)
  expect_identical(cc_mute2, as.data.frame(cc_cont2))
  
})
