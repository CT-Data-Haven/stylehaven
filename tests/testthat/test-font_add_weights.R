test_that("font_add_weights registers fonts", {
  # make sure fonts aren't already loaded
  fams_before <- sysfonts::font_families()
  font_add_weights("Noto Sans")
  fams_after <- sysfonts::font_families()
  expect_false("Noto Sans" %in% fams_before)
  expect_true("Noto Sans" %in% fams_after)
  expect_true("Noto Sans Semibold" %in% fams_after)
})

test_that("font_add_weights handles missing or bad calls", {
  expect_error(font_add_weights("XX"))
  
  # originally I thought this was a warning, but error is more appropriate
  expect_error(font_add_weights("Roboto"), "unavailable for this font: 600")
  
  expect_message(font_add_weights("Barlow Semi Condensed"), "regular weight")
  expect_message(font_add_weights("Barlow Semi Condensed"), "semibold weight")
  expect_message(font_add_weights("Barlow Semi Condensed"), "Barlow Semi Condensed Semibold")
})
