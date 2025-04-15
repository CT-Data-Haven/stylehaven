test_that("num_error checks for numbers", {
    expect_silent(num_error(c(2, 2.1, NA, NA_real_)))
    expect_error(num_error(c(TRUE)), "should be numeric")
    expect_error(num_error("1"), "should be numeric")
})

test_that("names_in_df_error correctly catches names", {
    df <- dummy_pct()
    expect_silent(names_in_df_error(df, "df", "value"))
    expect_error(names_in_df_error(df, "df", "percentage"), "percentage")
    expect_error(names_in_df_error(df, "example_data_frame", "example_data_frame"))
})

test_that("utils-errors functions refer to their caller", {
    # idk why this behaves weirdly
    msg <- expect_error(round100("1"))
    expect_match(as.character(msg), "round100")
})
