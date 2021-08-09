# median household income, median rent by number of bedrooms
# income needed for median rent to be affordable under HUD 30% guideline
yr <- 2019
hud_ratio <- 0.3

fetch <- list(income = "B25119", med_rent = "B25031", renters = "B25042") %>%
  purrr::map(cwi::multi_geo_acs, year = yr, counties = "New Haven") %>%
  purrr::map(cwi::label_acs, year = yr)

income <- fetch$income %>%
  dplyr::filter(stringr::str_detect(label, "Renter")) %>%
  dplyr::select(level, name, income = estimate)

med_rent <- fetch$med_rent %>%
  cwi::separate_acs(into = c(NA, NA, "bedrooms")) %>%
  tidyr::replace_na(list(bedrooms = "total")) %>%
  dplyr::filter((moe / estimate) <= 0.3)

renters <- fetch$renters %>%
  cwi::separate_acs(into = c("tenure", "bedrooms"), drop_total = TRUE) %>%
  dplyr::filter(tenure == "Renter occupied") %>%
  tidyr::replace_na(list(bedrooms = "total"))

rent_x_income <- tibble::lst(med_rent, renters) %>%
  dplyr::bind_rows(.id = "variable") %>%
  dplyr::mutate(bedrooms = forcats::as_factor(bedrooms) %>%
                  forcats::fct_relabel(stringr::str_replace, " bedrooms?", "br")) %>%
  tidyr::pivot_wider(id_cols = c(level, name, bedrooms), names_from = variable, values_from = estimate) %>%
  dplyr::left_join(income, by = c("level", "name")) %>%
  dplyr::filter(bedrooms %in% c("total", "2br"),
                level == "3_towns",
                !is.na(income)) %>%
  dplyr::mutate(annual_rent = med_rent * 12,
                income_needed = round(annual_rent / hud_ratio)) %>%
  dplyr::select(-level, -med_rent)

usethis::use_data(rent_x_income, overwrite = TRUE)
