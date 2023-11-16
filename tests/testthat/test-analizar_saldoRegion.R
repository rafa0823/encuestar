test_that("Estructura de la base de datos", {

  diseno <- survey::svydesign(ids = ~1, data = datos_demo |> mutate(region = sample(x = seq.int(1:6), size  = n(), replace = T)))

  Preguntas <- Pregunta2$new(encuesta = encuesta_demo, tema = tema_default)

candidatos <- cuestionario_demo |>
  filter(grepl(pattern = "Bloque 5", x = bloque)) |>
  filter(grepl(pattern = "opinion", x = llaves)) |>
  distinct(llaves) |>
  pull() |>
  gsub(pattern = "opinion_", replacement = "")

  bd_analizar_saldoRegion <- encuestar:::analizar_saldoRegion(patron_llaveOpinion = "opinion",
                                                  aspectos_llaveOpinion = candidatos,
                                                  ns_nc = "Ns/Nc",
                                                  cat_negativo = c("Muy mala", "Mala"),
                                                  cat_regular = "Regular",
                                                  cat_positivo = c("Buena", "Muy buena"),
                                                  diseno = Preguntas$Regiones$diseno,
                                                  diccionario = Preguntas$Regiones$diccionario)

  # Estructura
  testthat::expect_equal(object = names(bd_analizar_saldoRegion), expected = c("region", "saldo", "aspecto", "tema"))

  testthat::expect_equal(object = bd_analizar_saldoRegion |> ungroup() |>
                           distinct(region) |> nrow(), expected = Preguntas$Regiones$shp |>
                           as_tibble() |> nrow())
})


test_that("Cálculo correcto de las estimaciones", {

  diseno <- survey::svydesign(ids = ~1, data = datos_demo |> mutate(region = sample(x = seq.int(1:6), size  = n(), replace = T)))

  Preguntas <- Pregunta2$new(encuesta = encuesta_demo, tema = tema_default)

  candidatos <- cuestionario_demo |>
    filter(grepl(pattern = "Bloque 5", x = bloque)) |>
    filter(grepl(pattern = "opinion", x = llaves)) |>
    distinct(llaves) |>
    pull() |>
    gsub(pattern = "opinion_", replacement = "")

  bd_analizar_saldoRegion <- encuestar:::analizar_saldoRegion(patron_llaveOpinion = "opinion",
                                                  aspectos_llaveOpinion = candidatos,
                                                  ns_nc = "Ns/Nc",
                                                  cat_negativo = c("Muy mala", "Mala"),
                                                  cat_regular = "Regular",
                                                  cat_positivo = c("Buena", "Muy buena"),
                                                  diseno = Preguntas$Regiones$diseno,
                                                  diccionario = Preguntas$Regiones$diccionario)

  # Cálculo correcto de los porcentajes
  testthat::expect_lte(object = bd_analizar_saldoRegion |>
                         ungroup() |>
                         distinct(saldo) |>
                         pull() |>
                         max(), 1)

})
