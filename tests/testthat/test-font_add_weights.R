test_that("font_add_weights registers fonts", {
    sysfonts:::clean_fonts()
    fams_before <- sysfonts::font_families()
    # check that not already registered
    expect_false("Winky Sans" %in% fams_before)

    font_add_weights("Winky Sans")
    fams_after <- sysfonts::font_families()
    expect_true("Winky Sans" %in% fams_after)
    expect_true("Winky Sans Semibold" %in% fams_after)
})

test_that("font_add_weights handles missing or bad calls", {
    expect_error(font_add_weights("XX"))

    # originally I thought this was a warning, but error is more appropriate
    # need to use a font that doesn't have variable weights now that so many do
    expect_error(font_add_weights("Bytesized"), "unavailable for this font:")

    expect_message(font_add_weights("Barlow Semi Condensed"), "regular weight")
    expect_message(font_add_weights("Barlow Semi Condensed"), "semibold weight")
    expect_message(font_add_weights("Barlow Semi Condensed"), "Barlow Semi Condensed Semibold")
})
