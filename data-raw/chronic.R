path <- file.path("data-raw", "files", "DataHaven2018 GreaterNewHaven Crosstabs Pub.xlsx")
cws <- cwi::read_xtabs(path, process = TRUE)
chronic <- cws %>%
  dplyr::filter(code %in% c("Q23E", "Q23C", "Q23A")) %>%
  dplyr::mutate(dplyr::across(category:response, forcats::as_factor),
                group = group %>%
                  forcats::fct_relabel(stringr::str_replace, "^(\\b)(?=\\d+)", "Ages ") %>%
                  forcats::fct_recode(Black = "Black/Afr Amer", Latino = "Hispanic", "High school or less" = "High School or less", "$75K-$100K" = "$75K-100K", "Kids in home" = "Yes", "No kids in home" = "No", Men = "M", Women = "F"),
                question = forcats::fct_recode(code, hypertension = "Q23A", asthma = "Q23E", diabetes = "Q23C"),
                category = forcats::fct_recode(category, "Children in home" = "Children in HH")) %>%
  cwi::sub_nonanswers() %>%
  dplyr::filter(response == "Yes") %>%
  dplyr::select(-response)

usethis::use_data(chronic, overwrite = TRUE)
