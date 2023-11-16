## code to prepare `encuesta_demo` dataset goes here

encuesta_demo <- readr::read_rds(file = "data-raw/enc_chis_sep23.rda")

usethis::use_data(encuesta_demo, overwrite = TRUE)
