## code to prepare `diseno_demo` dataset goes here

diseno_demo <- readr::read_rds("data-raw/diseno9.rda")

usethis::use_data(diseno_demo, overwrite = TRUE)
