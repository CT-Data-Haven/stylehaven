# main purpose of this is percent_thresh, so bulk of testing uses that
test_that("thresher handles correct number, type of args", {
    expect_error(thresher("0.25", 0.01))
    expect_error(thresher(0.25, c(0.01, 0.99), less_than = c(TRUE, FALSE)))
    expect_error(thresher(0.25, 0.01, lbl_type = "other"))
    expect_error(thresher(0.25, thresh = NULL))
    expect_error(thresher(0.25, c(0.1, 0.2, 0.3)))
})

test_that("percent_thresh handles side to suppress", {
    x <- c(0.03, 0.004, 0.29, 0.991)
    expect_equal(
        percent_thresh(x, 0.99, less_than = FALSE),
        c("3%", "0%", "29%", ">99%")
    )
    expect_equal(
        percent_thresh(x, 0.99, less_than = TRUE),
        c("<99%", "<99%", "<99%", "99%")
    )
})

test_that("percent_thresh handles multiple thresholds", {
    x <- c(0.03, 0.004, 0.29, 0.991)
    expect_equal(
        percent_thresh(x, 0.01),
        c("3%", "<1%", "29%", "99%")
    )
    expect_equal(
        percent_thresh(x, c(0.01, 0.99)),
        c("3%", "<1%", "29%", ">99%")
    )
    expect_equal(
        percent_thresh(x, c(0.01, 0.99)),
        percent_thresh(x, c(0.01, 0.99), less_than = FALSE)
    )
})


test_that("percent_thresh handles different types of signs, including english", {
    x <- c(0.03, 0.004, 0.29, 0.991)
    expect_equal(
        percent_thresh(x, c(0.01, 0.99), txt = TRUE),
        c("3 percent", "less than 1 percent", "29 percent", "more than 99 percent")
    )
    # txt + html is irrelevant
    expect_equal(
        percent_thresh(x, 0.01, less_than = TRUE, txt = TRUE, html = TRUE),
        percent_thresh(x, 0.01, less_than = TRUE, txt = TRUE, html = FALSE)
    )
    expect_equal(
        percent_thresh(x, c(0.01, 0.99), txt = FALSE, html = TRUE),
        c("3%", "&lt;1%", "29%", "&gt;99%")
    )

    y <- c(1400, 900, 800)
    expect_equal(
        dollar_thresh(y, 1000, less_than = FALSE),
        c(">$1,000", "$900", "$800")
    )
})


test_that("number_thresh passes ... arguments", {
    x <- c(1100, 800, 900)
    expect_equal(
        number_thresh(x, 1000, less_than = FALSE, big.mark = ";"),
        c(">1;000", "800", "900")
    )
})

test_that("percent_thresh passes ... arguments", {
    x <- c(3, 0.4, 29.1, 99.1)
    expect_equal(
        percent_thresh(x, thresh = 1, scale = 1),
        c("3%", "<1%", "29%", "99%")
    )
    expect_equal(
        percent_thresh(x / 100, thresh = c(0.01, 0.99), accuracy = 0.1, decimal.mark = ","),
        c("3,0%", "<1,0%", "29,1%", ">99,0%")
    )
})

test_that("dollar_thresh passes ... arguments", {
    x <- c(10000, 9000, 900)
    expect_equal(
        dollar_thresh(x, 1000, scale = 1 / 1000, suffix = "k"),
        c("$10k", "$9k", "<$1k")
    )
})
