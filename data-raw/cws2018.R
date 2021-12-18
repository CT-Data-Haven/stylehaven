path <- file.path("data-raw", "files", "DataHaven2018 GreaterNewHaven Crosstabs Pub.xlsx")
walkability_codes <- list(safe_sidewalks = "Q17B", safe_biking = "Q17C")
chronic_codes <- list(hypertension = "Q23A", asthma = "Q23E", diabetes = "Q23C")
fin_insecurity_codes <- list(food_insecurity = "Q62", housing_insecurity = "Q64", no_bank_account = "Q66")
community_codes <- list(satisfied_with_area = "Q1", trust_neighbors = "Q17G", positive_role_models = "Q17H")
self_rate_codes <- list(self_rated_health = "Q19")
codes <- tibble::lst(walkability_codes, chronic_codes, fin_insecurity_codes, community_codes, self_rate_codes) %>%
  purrr::map(tibble::enframe, name = "question", value = "code") %>%
  purrr::map(tidyr::unnest, code) %>%
  rlang::set_names(stringr::str_remove, "_codes")

income_lvls <- list(
  "<$30K" = c("<$15K", "$15K-$30K"),
  # keeping 30-50
  "$50K-$100K" = c("$50K-$75K", "$75K-$100K"),
  "$100K+" = c("$100K-$200K", "$200K+"))

wts <- cwi::read_weights(path) %>%
  dplyr::mutate(group = forcats::as_factor(group) %>%
                  forcats::fct_relabel(stringr::str_replace_all, c(" to " = "-", "^(\\b)(?=\\d+)" = "Ages ", ",000" = "K", " (or more|and older)" = "+", "Less than " = "<")) %>%
                  forcats::fct_recode(Men = "Male", Women = "Female", Black = "African American/Black", Latino = "Hispanic", "Kids in home" = "Yes", "No kids in home" = "No") %>%
                  forcats::fct_relabel(stringr::str_remove, " degree"))

cws_all <- cwi::read_xtabs(path, process = TRUE) %>%
  dplyr::mutate(dplyr::across(category:response, forcats::as_factor),
                group = group %>%
                  forcats::fct_relabel(stringr::str_replace, "^(\\b)(?=\\d+)", "Ages ") %>%
                  forcats::fct_recode(Black = "Black/Afr Amer", Latino = "Hispanic", "High school or less" = "High School or less", "$75K-$100K" = "$75K-100K", "Kids in home" = "Yes", "No kids in home" = "No", Men = "M", Women = "F"),
                category = forcats::fct_recode(category, "Children in home" = "Children in HH")) %>%
  dplyr::select(-question) %>%
  dplyr::filter(!grepl("^Summary", response))

cws_list <- codes %>%
  purrr::map(dplyr::inner_join, cws_all, by = "code") %>%
  purrr::map(dplyr::mutate, response = forcats::fct_drop(response))

out <- list()
out[["walkability"]] <- cws_list$walkability %>%
  dplyr::mutate(response = forcats::fct_collapse(response, agree = c("Strongly agree", "Somewhat agree"))) %>%
  dplyr::group_by(question, category, group, response) %>%
  dplyr::summarise(value = sum(value)) %>%
  cwi::sub_nonanswers() %>%
  dplyr::filter(response == "agree")

out[["chronic_disease"]] <- cws_list$chronic %>%
  cwi::sub_nonanswers() %>%
  dplyr::filter(response == "Yes")

out[["fin_insecurity"]] <- cws_list$fin_insecurity %>%
  cwi::sub_nonanswers() %>%
  dplyr::mutate(value = ifelse(question == "no_bank_account", 1 - value, value)) %>%
  dplyr::filter(response == "Yes")

out[["community_cohesion"]] <- cws_list$community %>%
  dplyr::mutate(response = forcats::fct_collapse(response, agree = c("Strongly agree", "Somewhat agree"))) %>%
  dplyr::group_by(question, category, group, response) %>%
  dplyr::summarise(value = sum(value)) %>%
  cwi::sub_nonanswers() %>%
  dplyr::filter((question == "satisfied_with_area" & response == "Yes") | (question != "satisfied_with_area" & response == "agree"))

out <- out %>%
  purrr::map(dplyr::ungroup) %>%
  purrr::map(dplyr::select, question, category, group, value) %>%
  purrr::map(dplyr::mutate, value = round(value, digits = 2))

# can't map over list for use_data
list2env(out, .GlobalEnv)
usethis::use_data(walkability, overwrite = TRUE, version = 3)
usethis::use_data(chronic_disease, overwrite = TRUE, version = 3)
usethis::use_data(fin_insecurity, overwrite = TRUE, version = 3)
usethis::use_data(community_cohesion, overwrite = TRUE, version = 3)

# self-rated health is different--keep responses
self_rated_health <- cws_list$self_rate %>%
  dplyr::left_join(wts, by = "group") %>%
  dplyr::mutate(response = forcats::fct_relevel(forcats::as_factor(response), "Excellent", "Very good", "Good", "Fair")) %>%
  cwi::collapse_n_wt(category:response, .lvls = income_lvls, .fill_wts = TRUE) %>%
  cwi::sub_nonanswers()

usethis::use_data(self_rated_health, overwrite = TRUE, version = 3)
