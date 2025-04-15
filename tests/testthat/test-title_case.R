test_that("title_case uses accurate capitalization", {
    x <- c("greater new haven", "The first day of spring", "Baltimore's finest", "double-dutch")
    expect_equal(
        title_case(x),
        c("Greater New Haven", "The First Day of Spring", "Baltimore's Finest", "Double-Dutch")
    )
})

test_that("title_case handles short words", {
    x <- c("the first day of spring", "the first day Of spring", "of spring", "offer stands")
    expect_equal(
        title_case(x),
        c("The First Day of Spring", "The First Day of Spring", "Of Spring", "Offer Stands")
    )
})

test_that("title_case takes text out of snakecase via clean_titles", {
    x <- c("locations_in_walking_distance", "approve_of_police", "good to raise kids")
    expect_equal(
        title_case(x),
        c("Locations in Walking Distance", "Approve of Police", "Good to Raise Kids")
    )
    expect_equal(
        title_case(x, clean = FALSE),
        c("Locations_in_walking_distance", "Approve_of_police", "Good to Raise Kids")
    )
})

test_that("title_case passes or honors args to clean_titles", {
    x_delim <- c("locations.in.walking.distance")
    x_running <- c("lives in DMHAS region")
    x_split <- c("locationsInWalkingDistance")
    expect_equal(
        title_case(x_delim, space = "\\."),
        c("Locations in Walking Distance")
    )
    expect_equal(
        title_case(x_running),
        c("Lives in DMHAS Region")
    )
    expect_equal(
        title_case(x_split),
        c("Locations in Walking Distance")
    )
})
