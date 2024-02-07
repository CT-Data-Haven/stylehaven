exp <- readr::read_csv("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NVSS/USALEEP/CSV/CT_A.CSV") |>
  janitor::clean_names() |>
  dplyr::select(tract = tract_id, value = e_0)

life_exp <- cwi::xwalk |>
  dplyr::filter(county == "New Haven County") |>
  dplyr::distinct(town, tract) |>
  dplyr::left_join(exp, by = "tract")

usethis::use_data(life_exp, overwrite = TRUE)
