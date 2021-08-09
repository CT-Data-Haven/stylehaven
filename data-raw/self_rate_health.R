income_lvls <- list(
  "<$30K" = c("<$15K", "$15K-$30K"),
  # keeping 30-50
  "$50K-$100K" = c("$50K-$75K", "$75K-$100K"),
  "$100K+" = c("$100K-$200K", "$200K+")
)

path <- file.path("data-raw", "files", "DataHaven2018 GreaterNewHaven Crosstabs Pub.xlsx")
cws <- cwi::read_xtabs(path, process = TRUE)
wts <- cwi::read_weights(path) %>%
  dplyr::mutate(group = forcats::as_factor(group) %>%
                  forcats::fct_relabel(stringr::str_replace_all, c(" to " = "-", "^(\\b)(?=\\d+)" = "Ages ", ",000" = "K", " (or more|and older)" = "+", "Less than " = "<")) %>%
                  forcats::fct_recode(Men = "Male", Women = "Female", Black = "African American/Black", Latino = "Hispanic", "Kids in home" = "Yes", "No kids in home" = "No") %>%
                  forcats::fct_relabel(stringr::str_remove, " degree"))

self_rate_health <- cws %>%
  dplyr::filter(code == "Q19", !grepl("Summary", response)) %>%
  dplyr::mutate(dplyr::across(category:response, forcats::as_factor),
        group = group %>%
          forcats::fct_relabel(stringr::str_replace, "^(\\b)(?=\\d+)", "Ages ") %>%
          forcats::fct_recode(Black = "Black/Afr Amer", Latino = "Hispanic", "High school or less" = "High School or less", "$75K-$100K" = "$75K-100K", "Kids in home" = "Yes", "No kids in home" = "No", Men = "M", Women = "F"),
        category = forcats::fct_recode(category, "Children in home" = "Children in HH")) %>%
  dplyr::left_join(wts, by = "group") %>%
  tidyr::replace_na(list(weight = 1)) %>%
  cwi::collapse_n_wt(c(code, category:response), .lvls = income_lvls) %>%
  cwi::sub_nonanswers()

usethis::use_data(self_rate_health, overwrite = TRUE)

