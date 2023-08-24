test_that("scaffold_project lists correct directories", {
  expect_message(scaffold_project(dryrun = TRUE), "input_data")
  expect_message(scaffold_project(dryrun = TRUE), "_utils")
  expect_message(scaffold_project(dryrun = TRUE, plots = TRUE), "plots")
})

test_that("scaffold_project writes correct directories", {
  tmp <- file.path(tempdir(check = TRUE), "A")
  dir.create(tmp)
  withr::defer(unlink(tmp, recursive = TRUE), envir = parent.frame())
  scaffold_project(dir = tmp)
  expect_true(all(file.exists(file.path(tmp, c("analysis", "_utils")))))
})
