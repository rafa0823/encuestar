test_that("Cálculo de estimación con pesos", {

  bd_analizar_frecuencias <- analizar_frecuencias(diseno = encuesta_demo$muestra$diseno,
                                                  pregunta = principal_ocupacion)

  # Estructura de la base de datos
  testthat::expect_equal(names(bd_analizar_frecuencias), c("respuesta", "media", "ee", "pregunta"))

  # Cálculo de valores
  testthat::expect_lte(object = max(unique(bd_analizar_frecuencias$media)), expected = 1)

  # Normalizado
  testthat::expect_equal(object = bd_analizar_frecuencias |>
                           summarise(total = sum(media)) |>
                           pull() , expected = 1)



})
