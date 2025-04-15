# commenting out to avoid extra dependencies
# dfs <- list(
#   state = tidycensus::get_acs("state",
#                               variables = c(total = "B01002_001", male = "B01002_002", female = "B01002_003"),
#                               year = 2023, state = "CT"),
#   county = tidycensus::get_acs("county",
#                                variables = c(total = "B01002_001", male = "B01002_002", female = "B01002_003"),
#                                year = 2023, state = "CT") |>
#     dplyr::mutate(NAME = stringr::str_replace(NAME, " Planning Region.+$", " COG")),
#   town = tidycensus::get_acs("county subdivision",
#                  variables = c(total = "B01002_001", male = "B01002_002", female = "B01002_003"),
#                  year = 2023, state = "CT") |>
#     dplyr::mutate(county = stringr::str_extract(NAME, "([\\w\\s]+)(?: Planning Region)") |>
#                     trimws() |>
#                     stringr::str_replace("Planning Region", "COG"),
#                   NAME = stringr::str_remove(NAME, " town,.+$"))
# )
# df <- dplyr::bind_rows(dfs, .id = "level")
# df <- dplyr::rename_with(df, tolower)
# df <- dplyr::select(df, level, county, name, sex = variable, value = estimate)
# df <- dplyr::mutate(df, dplyr::across(c(level, sex), forcats::as_factor))
# df <- dplyr::filter(df, (level == "state") |
#                               (level == "county" & name %in% c("Capitol COG", "Greater Bridgeport COG")) |
#                               (level == "town" & county %in% c("Capitol COG", "Greater Bridgeport COG")))
# df <- dplyr::filter(df, !grepl("not defined,", name))

median_age <- structure(list(level = structure(c(
    1L, 1L, 1L, 2L, 2L, 2L, 2L,
    2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
    3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
    3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
    3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
    3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
    3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
    3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
    3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
    3L, 3L, 3L, 3L, 3L, 3L
), levels = c("state", "county", "town"), class = "factor"), county = c(
    NA, NA, NA, NA, NA, NA, NA,
    NA, NA, "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG",
    "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG",
    "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG",
    "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG",
    "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG",
    "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG",
    "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG",
    "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG",
    "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG",
    "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG",
    "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG",
    "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG",
    "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG",
    "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG",
    "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG",
    "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG",
    "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG",
    "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG",
    "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG",
    "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG",
    "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG",
    "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG",
    "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG", "Capitol COG",
    "Greater Bridgeport COG", "Greater Bridgeport COG", "Greater Bridgeport COG",
    "Greater Bridgeport COG", "Greater Bridgeport COG", "Greater Bridgeport COG",
    "Greater Bridgeport COG", "Greater Bridgeport COG", "Greater Bridgeport COG",
    "Greater Bridgeport COG", "Greater Bridgeport COG", "Greater Bridgeport COG",
    "Greater Bridgeport COG", "Greater Bridgeport COG", "Greater Bridgeport COG",
    "Greater Bridgeport COG", "Greater Bridgeport COG", "Greater Bridgeport COG"
), name = c(
    "Connecticut", "Connecticut", "Connecticut", "Capitol COG",
    "Capitol COG", "Capitol COG", "Greater Bridgeport COG", "Greater Bridgeport COG",
    "Greater Bridgeport COG", "Andover", "Andover", "Andover", "Avon",
    "Avon", "Avon", "Berlin", "Berlin", "Berlin", "Bloomfield", "Bloomfield",
    "Bloomfield", "Bolton", "Bolton", "Bolton", "Canton", "Canton",
    "Canton", "Columbia", "Columbia", "Columbia", "Coventry", "Coventry",
    "Coventry", "East Granby", "East Granby", "East Granby", "East Hartford",
    "East Hartford", "East Hartford", "East Windsor", "East Windsor",
    "East Windsor", "Ellington", "Ellington", "Ellington", "Enfield",
    "Enfield", "Enfield", "Farmington", "Farmington", "Farmington",
    "Glastonbury", "Glastonbury", "Glastonbury", "Granby", "Granby",
    "Granby", "Hartford", "Hartford", "Hartford", "Hebron", "Hebron",
    "Hebron", "Manchester", "Manchester", "Manchester", "Mansfield",
    "Mansfield", "Mansfield", "Marlborough", "Marlborough", "Marlborough",
    "New Britain", "New Britain", "New Britain", "Newington", "Newington",
    "Newington", "Plainville", "Plainville", "Plainville", "Rocky Hill",
    "Rocky Hill", "Rocky Hill", "Simsbury", "Simsbury", "Simsbury",
    "Somers", "Somers", "Somers", "Southington", "Southington", "Southington",
    "South Windsor", "South Windsor", "South Windsor", "Stafford",
    "Stafford", "Stafford", "Suffield", "Suffield", "Suffield", "Tolland",
    "Tolland", "Tolland", "Vernon", "Vernon", "Vernon", "West Hartford",
    "West Hartford", "West Hartford", "Wethersfield", "Wethersfield",
    "Wethersfield", "Willington", "Willington", "Willington", "Windsor",
    "Windsor", "Windsor", "Windsor Locks", "Windsor Locks", "Windsor Locks",
    "Bridgeport", "Bridgeport", "Bridgeport", "Easton", "Easton",
    "Easton", "Fairfield", "Fairfield", "Fairfield", "Monroe", "Monroe",
    "Monroe", "Stratford", "Stratford", "Stratford", "Trumbull",
    "Trumbull", "Trumbull"
), sex = structure(c(
    1L, 2L, 3L, 1L, 2L,
    3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L,
    1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L,
    2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L,
    3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L,
    1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L,
    2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L,
    3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L,
    1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L,
    2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L
), levels = c(
    "total", "male",
    "female"
), class = "factor"), value = c(
    41.2, 39.8, 42.5, 40.2,
    38.7, 41.7, 40, 39, 41.2, 52.1, 43.8, 53.7, 45.6, 44.9, 47.2,
    48.2, 45.8, 49, 47.1, 43.7, 50.1, 45.2, 46, 45, 45.4, 42.2, 50.9,
    49.6, 53.7, 46.4, 46.1, 46.3, 45.9, 42.1, 44.7, 39.1, 38.3, 37.3,
    39.6, 48.8, 46.3, 52.7, 39.3, 37.3, 42.8, 41.9, 39.6, 45.5, 43.1,
    42.1, 43.7, 44, 43.9, 44.2, 44.9, 45, 43.9, 33.4, 31.2, 35.7,
    45.5, 44.2, 49.1, 37.1, 35.6, 38.9, 21.2, 21.5, 20.9, 42.8, 42.4,
    43.4, 34.8, 34.3, 35.2, 45.2, 42.7, 47.4, 42.9, 40.9, 44.9, 43.9,
    43.3, 45, 42.8, 43.9, 41.6, 43.8, 41.6, 47.4, 44.6, 44, 45.4,
    40.3, 39.7, 41.1, 45.7, 44.2, 48.7, 45, 42.5, 50.2, 40.1, 40.4,
    39.7, 41.1, 36.6, 45.1, 40.3, 39.1, 41.6, 44.1, 43.4, 44.9, 36.8,
    34.5, 37.8, 42.7, 40.6, 44.6, 42.1, 41.5, 42.5, 36.4, 35.8, 37.2,
    45.5, 46.4, 44.8, 41, 40, 41.9, 42.6, 41.5, 43.6, 46.5, 43.7,
    48.6, 41.9, 41.2, 42.7
)), row.names = c(NA, -141L), class = c(
    "tbl_df",
    "tbl", "data.frame"
))

usethis::use_data(median_age, overwrite = TRUE)
