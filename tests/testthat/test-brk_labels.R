test_that("brk_labels checks vector type", {
    expect_error(brk_labels(c(1, 2, 3)))
    expect_error(brk_labels(factor(c("1", "2", "3"))))
})

test_that("brk_labels returns breaks formatted with shorthands", {
    percent_breaks <- c("[0,0.1]", "(0.1,0.25]", "(0.25,0.3333]")
    expect_equal(
        brk_labels(
            percent_breaks,
            format = "percent",
            mult_by = 100,
            round_digits = 0
        ),
        c("0 to 10%", "10 to 25%", "25 to 33%")
    )

    dollar_breaks <- c("[100,120]", "(120,145]", "(145,200]")
    expect_equal(
        brk_labels(dollar_breaks, format = "dollar", sep = "-"),
        c("$100-$120", "$120-$145", "$145-$200")
    )

    dollark_breaks <- c("[1200,2000]", "(2000,9000]", "(9000,12800]")
    expect_equal(
        brk_labels(dollark_breaks, format = "dollark", mult_by = 1e-3),
        c("$1.2k to $2k", "$2k to $9k", "$9k to $12.8k")
    )
    expect_equal(
        brk_labels(
            dollark_breaks,
            format = "dollark",
            mult_by = 1e-3,
            round_digits = 0
        ),
        c("$1k to $2k", "$2k to $9k", "$9k to $13k")
    )
})

test_that("brk_labels returns breaks formatted with custom", {
    scientific_breaks <- c("[0.01,0.5]", "(0.55,125]")
    expect_equal(
        brk_labels(scientific_breaks, format = "e", custom = T, digits = 1),
        c("1.0e-02 to 5.0e-01", "5.5e-01 to 1.2e+02")
    )
})

test_that("brk_labels handles null format", {
    int_breaks <- c("[1,3.5]", "(3.5,5.2]")
    expect_equal(
        brk_labels(int_breaks, round_digits = 0),
        c("1 to 4", "4 to 5")
    )
    expect_equal(brk_labels(int_breaks), c("1 to 3.5", "3.5 to 5.2"))
})

test_that("brk_labels executes formatting function with args", {
    brks <- c("[1,2]", "[2,4]")
    x_fun <- function(x, reps = 1) {
        xs <- paste(rep("x", times = reps), collapse = "")
        paste0(x, xs)
    }
    expect_equal(brk_labels(brks, format = x_fun), c("1x to 2x", "2x to 4x"))
    expect_equal(brk_labels(brks, format = x_fun, reps = 2), c("1xx to 2xx", "2xx to 4xx"))
})
