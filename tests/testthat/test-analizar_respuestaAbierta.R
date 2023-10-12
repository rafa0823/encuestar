test_that("Primer testeo de respuesta abierta", {

  bd_analizar_respuestaAbierta <- encuestar:::analizar_respuestaAbierta(bd = datos_demo,
                                                                        variable = "problema_masimportante_otro",
                                                                        palabrasVacias = c("muy", "anteriores", "mas", "todas", "falta"),
                                                                        totalPalabras = 10)
  # Estructura de la base de datos
  testthat::expect_equal(names(bd_analizar_respuestaAbierta), c("palabras", "n", "colores"))

})
