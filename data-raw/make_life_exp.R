life_exp <- read.csv("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NVSS/USALEEP/CSV/CT_A.CSV", colClasses = "character")
life_exp <- tibble::as_tibble(life_exp) |>
    dplyr::select(tract = 1, value = 5) |>
    dplyr::mutate(value = as.numeric(value)) |>
    dplyr::filter(grepl("^09009", tract))

usethis::use_data(life_exp, overwrite = TRUE)
