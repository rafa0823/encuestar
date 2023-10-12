test_that("Función graficar_barras", {
  testthat::expect_type(graficar_barras_frecuencia(bd = encuestar::analizar_frecuencias(diseno = encuesta_demo$muestra$diseno,
                                                                               pregunta = principal_ocupacion),
                                          porcentajes_afuera = F, tema = tema_default, titulo = ""), type = "list")
})
