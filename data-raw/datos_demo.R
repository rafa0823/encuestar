## code to prepare `datos_demo` dataset goes here

datos_demo <- readr::read_csv2(file = "data-raw/datos_campo.csv")

usethis::use_data(datos_demo, overwrite = TRUE)
