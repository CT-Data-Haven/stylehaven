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
      c("The First Day of Spring", "The First Day of Spring", "Of Spring", "Offer Stands"))
})

