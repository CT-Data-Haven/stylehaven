# cws_trend <- dcws::fetch_cws(grepl("(responsive local government|job done by the police|condition of public parks)", question),
#   .name = "Greater New Haven", .year = c(2015, 2024),
#   .category = c("Total", "Age"), .unnest = TRUE, .drop_ct = FALSE
# ) |>
#   dplyr::mutate(response = forcats::fct_collapse(response, excellent_good = c("Excellent", "Good"))) |>
#   dplyr::mutate(question = dplyr::case_when(
#       grepl("responsive local government", question) ~ "local_govt_responsive",
#       grepl("job done by the police", question) ~ "police_approval",
#       grepl("public parks", question) ~ "good_parks"
#   )) |>
#   dplyr::mutate(question = forcats::as_factor(question)) |>
#   dplyr::group_by(question, year, name, category, group, response) |>
#   dplyr::summarise(value = sum(value)) |>
#   dcws::sub_nonanswers(nons = c("Don't know enough about it in order to say", "Refused")) |>
#   dplyr::ungroup() |>
#   dplyr::filter(response == "excellent_good") |>
#   dplyr::mutate(value = round(value, 2)) |>
#   dplyr::mutate(group = forcats::fct_relevel(group, "Connecticut", "Greater New Haven")) |>
#   dplyr::select(question, year, category, group, value)

cws_trend <- structure(list(question = structure(c(
    1L, 1L, 1L, 1L, 1L, 1L,
    1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
    2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L
), levels = c(
    "local_govt_responsive",
    "police_approval", "good_parks"
), class = "factor"), year = c(
    2015,
    2015, 2015, 2015, 2015, 2015, 2024, 2024, 2024, 2024, 2024, 2024,
    2015, 2015, 2015, 2015, 2015, 2015, 2024, 2024, 2024, 2024, 2024,
    2024, 2015, 2015, 2015, 2015, 2015, 2015, 2024, 2024, 2024, 2024,
    2024, 2024
), category = structure(c(
    1L, 1L, 2L, 2L, 2L, 2L, 1L,
    1L, 2L, 2L, 2L, 2L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 1L, 2L, 2L, 2L,
    2L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 1L, 2L, 2L, 2L, 2L
), levels = c(
    "Total",
    "Age"
), class = "factor"), group = structure(c(
    1L, 2L, 3L, 4L,
    5L, 6L, 1L, 2L, 3L, 4L, 5L, 6L, 1L, 2L, 3L, 4L, 5L, 6L, 1L, 2L,
    3L, 4L, 5L, 6L, 1L, 2L, 3L, 4L, 5L, 6L, 1L, 2L, 3L, 4L, 5L, 6L
), levels = c(
    "Connecticut", "Greater New Haven", "Ages 18-34",
    "Ages 35-49", "Ages 50-64", "Ages 65+"
), class = "factor"), value = c(
    0.49,
    0.47, 0.42, 0.4, 0.52, 0.59, 0.51, 0.48, 0.32, 0.44, 0.52, 0.67,
    0.78, 0.74, 0.69, 0.7, 0.77, 0.81, 0.72, 0.65, 0.46, 0.62, 0.72,
    0.84, 0.76, 0.71, 0.65, 0.69, 0.75, 0.79, 0.74, 0.71, 0.62, 0.67,
    0.74, 0.86
)), row.names = c(NA, -36L), class = c(
    "tbl_df", "tbl",
    "data.frame"
))

usethis::use_data(cws_trend, overwrite = TRUE)
