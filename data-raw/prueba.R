## code to prepare `prueba` dataset goes here
prueba <- readxl::read_excel("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Clientes/Chiapas/Quetzal/Bases de datos/Bases de datos/CHIAPAS_1oct_3_pm.xlsx")



usethis::use_data(prueba, overwrite = TRUE)
