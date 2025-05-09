test_that("palx returns correct classes", {
    col <- "#9CCC0C"
    expect_s3_class(palx(col, return_df = FALSE), c("palx", "list"))
    expect_s3_class(palx(col, return_df = TRUE), c("palx", "data.frame"))
})

test_that("palx checks for valid colors", {
    col_hex <- "#3E84D2"
    col_lower <- tolower(col_hex)
    col_name <- "blue"
    col_alpha <- "#3E84D200"
    col_nosign <- "3E84D2"
    col_short <- "#12345"
    col_three <- "#456"
    col_nonhex <- "#12345M"
    col_wrong <- "blue9"
    expect_s3_class(palx(col_hex), "palx")
    expect_s3_class(palx(col_lower), "palx")
    expect_s3_class(palx(col_name), "palx")
    expect_s3_class(palx(col_alpha), "palx")
    expect_error(palx(col_nosign), "should be a valid color")
    expect_error(palx(col_short), "should be a valid color")
    expect_error(palx(col_three), "should be a valid color")
    expect_error(palx(col_nonhex), "should be a valid color")
    expect_error(palx(col_wrong), "should be a valid color")
})

test_that("as_tibble and plot generics run without error on palx objects", {
    pal <- palx("#9CCC0C", return_df = FALSE)
    # expect_equal fails because the plots have
    # attributes in different environments
    expect_identical(as_tibble(pal), as_tibble.palx(pal))
    expect_s3_class(plot(pal), "gg")
    expect_s3_class(as_tibble(pal), "palx")
    # if as_tibble(pal) is still recognized as palx, plotting should yield a gg
    expect_s3_class(plot(as_tibble(pal)), "gg")
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
