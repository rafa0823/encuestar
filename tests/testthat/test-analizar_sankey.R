test_that("Estructura de salida", {

  bd_analizar_respuestaAbierta <- encuestar:::analizar_sankey(diseno = survey::svydesign(ids = ~1, data = datos_demo),
                                                                var_1 = "sexo", var_2 = "conocimiento_era")

  # Estructura de la base de datos de salida
  testthat::expect_equal(names(bd_analizar_respuestaAbierta), c("sexo", "conocimiento_era", "n"))

  testthat::expect_equal(typeof(bd_analizar_respuestaAbierta[,1] |>  pull()), "character")
  testthat::expect_equal(typeof(bd_analizar_respuestaAbierta[,2] |>  pull()), "character")
  testthat::expect_equal(typeof(bd_analizar_respuestaAbierta[,3] |>  pull()), "double")

})
