## code to prepare `cuestionario_demo` dataset goes here

cuestionario_demo <- readr::read_csv2(file = "data-raw/cuestionario.csv")

usethis::use_data(cuestionario_demo, overwrite = TRUE)
