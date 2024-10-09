library(dplyr)

variables_surveyToGo <-
  readxl::read_xlsx(path = "./data-raw/catalogo_variables_SurveyToGo.xlsx")

variables_opinometro <-
  readxl::read_xlsx(path = "./data-raw/catalogo_variables_Opinometro.xlsx")

variables_encuestar <-
  tibble(variable = c("INT",
                      paste0("GPS_", "INT_", c("LA", "LO", "ALT", "BEAR", "SPEED", "DATE")),
                      "intento_efectivo",
                      "SECCION",
                      "generacion",
                      "amai_jefegrado",
                      "amai_cantidadwc",
                      "amai_cantidadautos",
                      "amai_internet",
                      "amai_trabajo",
                      "amai_cantidadcuartos",
                      "suma_amai",
                      "nivel_socioec",
                      "rango_edad",
                      "cluster_0",
                      "distancia",
                      "strata_1",
                      "cluster_2",
                      "total",
                      "fpc_2",
                      "fpc_0",
                      "region")) |>
  mutate(plataforma = "encuestar",
         primer_nivel = "plataforma",
         segundo_nivel = dplyr::if_else(condition = grepl(pattern = "INT|intento_efectivo",
                                                          x = variable),
                                        true = "plataforma",
                                        false = "auxiliar"),
         segundo_nivel = dplyr::if_else(condition = variable == "SECCION",
                                        true = "sistema",
                                        false = segundo_nivel))

catalogo_variables <-
  bind_rows(variables_surveyToGo, variables_opinometro, variables_encuestar)

usethis::use_data(catalogo_variables, overwrite = TRUE, internal = TRUE)
