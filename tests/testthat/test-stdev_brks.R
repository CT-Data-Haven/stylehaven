test_that("stdev_brks returns correct dimensions", {
    df_calc <- stdev_brks(life_exp, keep_calcs = TRUE)
    df_drop <- stdev_brks(life_exp, keep_calcs = FALSE)
    expect_equal(nrow(df_calc), nrow(life_exp))
    expect_equal(nrow(df_drop), nrow(life_exp))
    expect_equal(ncol(df_calc), ncol(life_exp) + 4)
    expect_equal(ncol(df_drop), ncol(life_exp) + 1)
})

test_that("stdev_brks retains groups", {
    life_x_county <- life_exp |>
        dplyr::mutate(county = substr(tract, 3, 5))
    df_grp <- life_x_county |>
        dplyr::group_by(county) |>
        stdev_brks()
    df_not <- life_x_county |>
        stdev_brks(by = "county")
    expect_true(dplyr::is_grouped_df(df_grp))
    expect_false(dplyr::is_grouped_df(df_not))
    expect_identical(dplyr::ungroup(df_grp), df_not)
})

test_that("stdev_brks handles filters", {
    f_ok <- list(name = "Connecticut")
    # too many values
    f_warn <- list(name = c("Connecticut", "Capitol"))
    # no names
    f_err1 <- list("state")
    # vector instead of list
    f_err2 <- c(name = "state")
    # dupes
    f_err3 <- list(level = "county")
    expect_s3_class(stdev_brks(median_age, filters = f_ok, by = "sex"), "data.frame")
    expect_warning(stdev_brks(median_age, filters = f_warn, by = "sex"))
    expect_error(stdev_brks(median_age, filters = f_err1, by = "sex"))
    expect_error(stdev_brks(median_age, filters = f_err2, by = "sex"))
    expect_error(stdev_brks(median_age, filters = f_err3, by = "sex"))

    expect_s3_class(stdev_brks(median_age, filters = list(name = "Connecticut", sex = "total")), "data.frame")
})

test_that("stdev_brks checks lengths of breaks, labels", {
    df <- dummy_std()
    expect_error(stdev_brks(df, brks = c(-2, -1, 1, 2), labels = c("1std", "2std")))
})
