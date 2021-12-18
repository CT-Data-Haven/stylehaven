cws_trend <- readr::read_csv("data-raw/files/2015_2021_cws_trend_sub.csv") %>%
  dplyr::mutate(dplyr::across(category:indicator, forcats::as_factor)) %>%
  dplyr::select(year, indicator, dplyr::everything())

usethis::use_data(cws_trend, overwrite = TRUE)
