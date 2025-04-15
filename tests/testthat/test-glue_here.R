test_that("glue_here handles variables in the environment", {
    wd <- local_wd()
    # with named args
    expect_equal(
        glue_here("demographics/new_haven_data_{yr}.csv", yr = 2025),
        file.path(wd, "demographics/new_haven_data_2025.csv")
    )

    # without named args
    # don't understand why variables are out of scope of function calls inside expect_equal
    env <- rlang::env(yr = 2023)
    withr::with_environment(env, {
        expect_equal(
            glue_here("demographics/bridgeport_data_{yr}.csv"),
            file.path(wd, "demographics/bridgeport_data_2023.csv")
        )
    })
})

test_that("glue_here handles multiple variables", {
    wd <- local_wd()
    yr <- 2025
    dim1 <- "income"
    dims <- c("income", "age")
    expect_equal(
        glue_here("demographics/{dim1}_{yr}.csv", dim1 = "income", yr = 2026),
        file.path(wd, "demographics/income_2026.csv")
    )
    expect_equal(
        glue_here("output/data_by_{dims}.csv", dims = c("income", "age")),
        file.path(wd, c("output/data_by_income.csv", "output/data_by_age.csv"))
    )
    expect_equal(
        glue_here(c("{dim1}/{dim1}_{yr}.txt", "{dim1}/{dim1}_{yr - 1}.txt"), dim1 = "income", yr = 2025),
        file.path(wd, c("income/income_2025.txt", "income/income_2024.txt"))
    )
})

test_that("glue_here handles snakecasing", {
    wd <- local_wd()
    env <- rlang::env(
        locs = c("St Vincent's", "Greater New Haven", "Yale-New Haven Hospital", "Lawrence + Memorial"),
        year = 2024
    )
    withr::with_environment(env, {
        expect_equal(
            glue_here("{locs}_{year}.txt", .snake = TRUE),
            file.path(wd, c("st_vincents_2024.txt", "greater_new_haven_2024.txt", "yale_new_haven_hospital_2024.txt", "lawrence_memorial_2024.txt"))
        )
        expect_equal(
            glue_here("{locs}_{year}.txt", .snake = FALSE),
            file.path(wd, c("St Vincent's_2024.txt", "Greater New Haven_2024.txt", "Yale-New Haven Hospital_2024.txt", "Lawrence + Memorial_2024.txt"))
        )
    })
})
