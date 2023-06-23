test_that("round_sum100 rounds correctly", {
  s1 <- sum(round_sum100(c(0.24, 0.61, 0.15) * 100, 0))
  s2 <- sum(round_sum100(c(9.2124, 40.292, 50.2), 0))
  expect_true(all(c(s1, s2) == 100))
})

test_that("round_sum100 prints messages", {
  expect_message(round_sum100(c(0.24, 0.6, 0.15) * 100, verbose = TRUE), "Sums to 99, not 100.")
  expect_silent(s2 <- sum(round_sum100(c(9.2124, 40.292, 50.2), 0, verbose = FALSE)))
})

test_that("round_sum100 warns if any NA values", {
  expect_warning(round_sum100(c(0.24, 0.61, NA) * 100))
})
