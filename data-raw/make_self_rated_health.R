# health <- dcws::fetch_cws(grepl("\\brate your overall health", question),
#                           .year = 2024, .name = "Connecticut", .unnest = TRUE,
#                           .category = c("Total", "Gender")) |>
#   dcws::sub_nonanswers() |>
#     dplyr::mutate(value = round(value, 2)) |>
#   dplyr::select(category, group, response, value)

self_rated_health <- structure(list(
    category = structure(c(
        1L, 1L, 1L, 1L, 1L, 2L,
        2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L
    ), levels = c(
        "Total",
        "Gender"
    ), class = "factor"), group = structure(c(
        1L, 1L, 1L,
        1L, 1L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L,
        4L
    ), levels = c("Connecticut", "Male", "Female", "Nonbinary"), class = "factor"),
    response = structure(c(
        1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L,
        5L, 1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 5L
    ), levels = c(
        "Excellent",
        "Very good", "Good", "Fair", "Poor"
    ), class = "factor"),
    value = c(
        0.19, 0.34, 0.29, 0.14, 0.04, 0.2, 0.34, 0.29,
        0.13, 0.04, 0.18, 0.34, 0.29, 0.16, 0.04, 0.09, 0.2, 0.37,
        0.22, 0.12
    )
), row.names = c(NA, -20L), class = c(
    "tbl_df",
    "tbl", "data.frame"
))
# self_rated_health$response <- forcats::fct_rev(self_rated_health$response)
usethis::use_data(self_rated_health, overwrite = TRUE)
