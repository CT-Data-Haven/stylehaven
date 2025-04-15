life_exp <- readr::read_csv("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NVSS/USALEEP/CSV/CT_A.CSV") |>
    dplyr::select(tract = 1, value = 5) |>
    dplyr::filter(grepl("^09009", tract))

usethis::use_data(life_exp, overwrite = TRUE)
