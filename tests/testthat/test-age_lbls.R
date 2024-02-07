test_that("age_lbls handles punctuation & spacing", {
  a1 <- c("under18", "under_age18", "ages18plus", "other_thing", "age18")
  brk1 <- c("Under 18", "Under age 18", "Ages 18+", "Other thing", "Age 18")
  expect_identical(age_lbls(a1), brk1)
})

test_that("age_lbls handles numbers", {
  a1 <- c("ages00_17", "ages00_05", "all_ages")
  brk1 <- c("Ages 0-17", "Ages 0-5", "All ages")
  expect_identical(age_lbls(a1), brk1)
})
