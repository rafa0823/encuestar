test_that("Estructura de la base de datos de salida", {

  bd_analizar_candidatoPartido <- encuestar:::analizar_candidatoPartido(diseno = survey::svydesign(ids = ~1, data = datos_demo),
                                                                        diccionario = cuestionario_demo,
                                                                        llave_partido = "partido",
                                                                        llave_conocimiento = "conocimiento",
                                                                        respuesta_conoce = "Sí",
                                                                        candidatos = c("era", "sasil"),
                                                                        corte_otro = 0.01)

  # Estructura
  testthat::expect_equal(object = length(bd_analizar_candidatoPartido), expected = 2)

  testthat::expect_equal(names(bd_analizar_candidatoPartido), c("conoce", "partido"))

  testthat::expect_equal(bd_analizar_candidatoPartido$conoce |> distinct(respuesta) |> pull() |> as.character(), c("Sí"))

})

test_that("Cómputo correcto de las estimaciones", {

  bd_analizar_candidatoPartido <- encuestar:::analizar_candidatoPartido(diseno = survey::svydesign(ids = ~1, data = datos_demo),
                                                                        diccionario = cuestionario_demo,
                                                                        llave_partido = "partido",
                                                                        llave_conocimiento = "conocimiento",
                                                                        respuesta_conoce = "Sí",
                                                                        candidatos = c("era", "sasil"),
                                                                        corte_otro = 0.01)

  # Conocimiento
  testthat::expect_lte(object = max(bd_analizar_candidatoPartido$conoce$media), expected = 1)

  # Partido
  testthat::expect_lte(object = max(bd_analizar_candidatoPartido$partido$media), expected = 1)



})

test_that("Suma cerrada entre aspectos",{
  # La suma de las proporciones para cada aspecto debe ser igual en cada caso. De lo contrario, hay un error de cómputo.

  bd_analizar_candidatoPartido <- encuestar:::analizar_candidatoPartido(diseno = survey::svydesign(ids = ~1, data = datos_demo),
                                                                        diccionario = cuestionario_demo,
                                                                        llave_partido = "partido",
                                                                        llave_conocimiento = "conocimiento",
                                                                        respuesta_conoce = "Sí",
                                                                        candidatos = c("era", "sasil"),
                                                                        corte_otro = 0.01)

  # Summa cerrada
  testthat::expect_equal(object = length(bd_analizar_candidatoPartido$partido |>
                                           summarise(total = sum(media), .groups = "drop") |>
                                           distinct(total) |>
                                           pull() |>
                                           unique()), expected = 2)

})

test_that("Suma normalizada de las estimaciones", {
  # La suma de las proporciones para cada aspecto, además de ser igual para cada caso, debe ser igual a 1.

  bd_analizar_candidatoPartido <- encuestar:::analizar_candidatoPartido(diseno = survey::svydesign(ids = ~1, data = datos_demo),
                                                                        diccionario = cuestionario_demo,
                                                                        llave_partido = "partido",
                                                                        llave_conocimiento = "conocimiento",
                                                                        respuesta_conoce = "Sí",
                                                                        candidatos = c("era", "sasil"),
                                                                        corte_otro = 0.01)

  # Partido
  testthat::expect_equal(object = bd_analizar_candidatoPartido$partido |>
                         pull(sup) |>
                         max(), expected = 1)

})
