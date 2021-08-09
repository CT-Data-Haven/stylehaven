path <- file.path("data-raw", "files", "DataHaven2018 GreaterNewHaven Crosstabs Pub.xlsx")
cws <- cwi::read_xtabs(path, process = TRUE)
walkability <- cws %>%
  dplyr::filter(code %in% c("Q17B", "Q17C")) %>%
  dplyr::mutate(dplyr::across(category:response, forcats::as_factor),
                group = group %>%
                  forcats::fct_relabel(stringr::str_replace, "^(\\b)(?=\\d+)", "Ages ") %>%
                  forcats::fct_recode(Black = "Black/Afr Amer", Latino = "Hispanic", "High school or less" = "High School or less", "$75K-$100K" = "$75K-100K", "Kids in home" = "Yes", "No kids in home" = "No", Men = "M", Women = "F"),
                response = forcats::fct_collapse(response, agree = c("Strongly agree", "Somewhat agree")),
                question = forcats::as_factor(code) %>%
                  forcats::fct_recode("Safe sidewalks" = "Q17B", "Safe places to bike" = "Q17C"),
                category = forcats::fct_recode(category, "Children in home" = "Children in HH")) %>%
  dplyr::group_by(question, category, group, response) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::filter(response == "agree") %>%
  dplyr::ungroup()

usethis::use_data(walkability, overwrite = TRUE)
