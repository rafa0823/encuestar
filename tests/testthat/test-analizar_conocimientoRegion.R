test_that("Estructura de la base de datos de salida", {

  diseno <- survey::svydesign(ids = ~1, data = datos_demo |> mutate(region = sample(x = seq.int(1:6), size  = n(), replace = T)))

  Preguntas <- Pregunta2$new(encuesta = encuesta_demo, tema = tema_default)

  candidatos <- cuestionario_demo |>
    filter(grepl(pattern = "Bloque 3", x = bloque)) |>
    distinct(llaves) |>
    pull() |>
    gsub(pattern = "conocimiento_", replacement = "")

  bd_analizar_conocimientoRegion <- analizar_conocimientoRegion(patron_llaveConocimiento = "conocimiento",
                                                                aspectos_llaveConocimiento = candidatos,
                                                                filtro_respuestaConocimiento = "Sí",
                                                                diseno = Preguntas$Regiones$diseno,
                                                                diccionario = cuestionario_demo)

  # Estructura
  testthat::expect_equal(object = names(bd_analizar_conocimientoRegion), expected = c("region", "n", "pct", "aspecto", "tema"))

  testthat::expect_equal(object = bd_analizar_conocimientoRegion |> ungroup() |>
                           distinct(region) |> nrow(), expected = Preguntas$Regiones$shp |>
                           as_tibble() |> nrow())

})

test_that("Cálculo correcto de las estimaciones", {

  diseno <- survey::svydesign(ids = ~1, data = datos_demo |> mutate(region = sample(x = seq.int(1:6), size  = n(), replace = T)))

  Preguntas <- Pregunta2$new(encuesta = encuesta_demo, tema = tema_default)

  candidatos <- cuestionario_demo |>
    filter(grepl(pattern = "Bloque 3", x = bloque)) |>
    distinct(llaves) |>
    pull() |>
    gsub(pattern = "conocimiento_", replacement = "")

  bd_analizar_conocimientoRegion <- analizar_conocimientoRegion(patron_llaveConocimiento = "conocimiento",
                                                                aspectos_llaveConocimiento = candidatos,
                                                                filtro_respuestaConocimiento = "Sí",
                                                                diseno = Preguntas$Regiones$diseno,
                                                                diccionario = cuestionario_demo)

  # Cálculo correcto de los porcentajes
  testthat::expect_lte(object = bd_analizar_conocimientoRegion |>
                         ungroup() |>
                         distinct(pct) |>
                         pull() |>
                         max(), 1)

})
