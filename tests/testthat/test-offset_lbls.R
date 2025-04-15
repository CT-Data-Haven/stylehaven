test_that("ratio_to_max correctly calculates limits", {
    x <- 1:6
    expect_equal(ratio_to_max(x, frac = 0.1), 6 * 0.1)
    y <- c(NA_real_, 1:6)
    expect_equal(ratio_to_max(y, frac = 0.1), 6 * 0.1)
})

test_that("offset_lbls adds expected columns to data frame", {
    orig <- dummy_pct()
    lbl <- offset_lbls(orig, value)
    expect_named(lbl, c(names(orig), "is_small", "off", "value_off", "just", "lbl"))
    expect_false("y" %in% names(lbl))
})

test_that("offset_lbls tests function type, uses reasonable default", {
    orig <- dummy_pct(5)
    no_fun <- offset_lbls(orig, value, fun = NULL)
    pct_fun <- offset_lbls(orig, value, fun = scales::label_percent(accuracy = 1))
    expect_identical(no_fun$lbl, c("0", "0.25", "0.5", "0.75", "1"))
    expect_identical(pct_fun$lbl, c("0%", "25%", "50%", "75%", "100%"))
    expect_error(offset_lbls(orig, value, fun = "percent"))
})

test_that("offset_lbls labels NA", {
    orig <- dummy_pct(3)
    orig$value[2] <- NA_real_
    default <- offset_lbls(orig, value)
    custom <- offset_lbls(orig, value, na = "Not available")
    with_fun <- offset_lbls(orig, value, na = "Not avail.", fun = scales::label_percent(accuracy = 1))
    with_null <- offset_lbls(orig, value, na = NULL)
    expect_identical(default$lbl, c("0", "N/A", "1"))
    expect_identical(custom$lbl, c("0", "Not available", "1"))
    expect_identical(with_fun$lbl, c("0%", "Not avail.", "100%"))
    expect_identical(with_null$lbl, c("0", NA_character_, "1"))
})

test_that("offset_lbls correctly calculates ratios and margins", {
    orig <- dummy_int(5, -1) # negative offset to get 0
    orig$value[4] <- NA_real_
    default <- offset_lbls(orig, value)
    expect_true(any(default$is_small))
    expect_true(all(default$off[default$is_small] > 0))
    expect_true(all(default$off[!default$is_small] < 0))
    expect_true(all(default$off[default$is_small] > 0))
    no_na <- na.omit(default)
    expect_true(all(no_na$value_off == (no_na$value + no_na$off)))

    # increasing thresh means increasing smalls
    hi_thresh <- offset_lbls(orig, value, thresh = 0.5)
    expect_true(sum(hi_thresh$is_small) > sum(default$is_small))

    # relative size of margin
    m1 <- offset_lbls(orig, value, margin = 1)
    m2 <- offset_lbls(orig, value, margin = 2)
    expect_true(unique(m1$off[m1$is_small]) == -1 * unique(m1$off[!m1$is_small]))
    expect_true(unique(m2$off[m2$is_small]) == -2 * unique(m2$off[!m2$is_small]))
})

test_that("offset_lbls correctly justifies values", {
    orig <- dummy_int(5, -1) # negative offset to get 0
    orig$value[4] <- NA_real_
    default <- offset_lbls(orig, value)

    expect_true(all(default$just[default$is_small] == 0))
    expect_true(all(default$just[!default$is_small] == 1))
})

test_that("offset_lbls operates across grouped data", {
    orig <- dplyr::bind_rows(
        a = dummy_int(4, 0),
        b = dummy_int(4, 10),
        .id = "group"
    )
    grouped <- offset_lbls(dplyr::group_by(orig, group), value, margin = 1) # simpler
    ungrouped <- offset_lbls(orig, value, margin = 1)
    grp_off <- dplyr::distinct(grouped, group, off)
    ungrp_off <- dplyr::distinct(ungrouped, group, off)

    expect_true(dplyr::is_grouped_df(grouped))
    # should have bigger offset for higher-val group
    expect_gt(unique(abs(grp_off$off[grp_off$group == "b"])), unique(abs(grp_off$off[grp_off$group == "a"])))
    # should have only 1 size offset when all calculated together
    expect_length(unique(abs(ungrp_off$off)), 1)
})
