test_that("theme_din returns proper theme object", {
    thm1 <- theme_din()
    thm2 <- theme_din(xgrid = TRUE, ygrid = "dotted")
    expect_s3_class(thm1, "theme")
    expect_s3_class(thm2, "theme")
})
