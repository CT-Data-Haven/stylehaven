test_that("number_formats return expected strings", {
    num <- c(1234, 12345)
    expect_equal(
        comma(num),
        c("1,234", "12,345")
    )

    pct <- c(0.05, 0.002, 0.422)
    expect_equal(
        round100(pct),
        c("5", "0", "42")
    )
    expect_equal(
        percent100(pct),
        c("5%", "0%", "42%")
    )
    expect_equal(
        percent_lt1(pct),
        c("5%", "<1%", "42%")
    )
    expect_equal(
        percent_txt(pct),
        c("5 percent", "0 percent", "42 percent")
    )
    expect_equal(
        percent_eng(c(0.05, 0.25)),
        c("Five percent", "Twenty-five percent")
    )
    expect_equal(
        percent_eng(c(0.05, 0.25), sentence_case = FALSE),
        c("five percent", "twenty-five percent")
    )

    dol <- c(10000, 100100, 300000)
    expect_equal(
        dollark(dol),
        c("$10k", "$100k", "$300k")
    )
    expect_equal(
        dollark(dol, accuracy = 0.1),
        c("$10.0k", "$100.1k", "$300.0k")
    )
    dol01 <- c(10.20, 10.8)
    expect_equal(
        dollar1(dol01),
        c("$10", "$11")
    )
})

test_that("number_formats pass ... args", {
    pct <- c(0.05, 0.002, 0.422)
    expect_equal(
        percent_lt1(pct, accuracy = 0.1),
        c("5.0%", "<1.0%", "42.2%")
    )
    expect_equal(
        percent_lt1(pct, accuracy = 0.1, decimal.mark = ","),
        c("5,0%", "<1,0%", "42,2%")
    )

    dol <- c(10000, 100100, 300000)
    expect_equal(
        dollark(dol, prefix = "USD "),
        c("USD 10k", "USD 100k", "USD 300k")
    )
})
