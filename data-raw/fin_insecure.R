path <- file.path("data-raw", "files", "DataHaven2018 GreaterNewHaven Crosstabs Pub.xlsx")
cws <- cwi::read_xtabs(path, process = TRUE)
fin_insecure <- cws %>%
  dplyr::filter(code %in% c("Q62", "Q64", "Q66")) %>%
  dplyr::mutate(dplyr::across(category:response, forcats::as_factor),
                group = group %>%
                  forcats::fct_relabel(stringr::str_replace, "^(\\b)(?=\\d+)", "Ages ") %>%
                  forcats::fct_recode(Black = "Black/Afr Amer", Latino = "Hispanic", "High school or less" = "High School or less", "$75K-$100K" = "$75K-100K", "Kids in home" = "Yes", "No kids in home" = "No", Men = "M", Women = "F"),
                question = forcats::fct_recode(code, food_insecurity = "Q62", housing_insecurity = "Q64", no_bank_account = "Q66"),
                category = forcats::fct_recode(category, "Children in home" = "Children in HH")) %>%
  cwi::sub_nonanswers() %>%
  dplyr::filter(response == "Yes") %>%
  dplyr::mutate(value = ifelse(question == "no_bank_account", 1 - value, value)) %>%
  dplyr::select(-response)

usethis::use_data(fin_insecure, overwrite = TRUE)
