test_that("contrast_colors matches multiple types of palettes", {
  qual_pal <- rcartocolor::carto_pal(n = 6, name = "Bold")
  p1 <- contrast_colors(qual_pal)
  # brewer
  p2 <- contrast_colors("Spectral")
  # carto
  p3 <- contrast_colors("Bold", n = 6)
  # virids
  p4 <- contrast_colors("magma")
  expect_type(p1, "character")
  expect_type(p2, "character")
  expect_type(p3, "character")
  expect_type(p4, "character")
  expect_identical(p3, p1)
  expect_error(contrast_colors("blah"))
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
  
  # too few colors
  expect_error(contrast_colors("Spectral", n = 1))
  expect_error(contrast_colors("Spectral", n = NULL))
  
  # n doesn't matter if pal is a vector
  expect_length(contrast_colors(qual_pal, n = NULL), 6)
})
