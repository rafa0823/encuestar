## code to prepare `shp_demo` dataset goes here

shp_demo <- readr::read_rds("data-raw/shp.rda")

usethis::use_data(shp_demo, overwrite = TRUE)
