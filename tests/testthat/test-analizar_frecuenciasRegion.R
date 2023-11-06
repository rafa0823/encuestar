test_that("Estructura de la base de datos", {

  diseno <- survey::svydesign(ids = ~1, data = datos_demo |> mutate(region = sample(x = seq.int(1:6), size  = n(), replace = T)))

  Preguntas <- Pregunta2$new(encuesta = encuesta_demo, tema = tema_default)

  bd_analizar_frecuenciasRegion <- analizar_frecuenciasRegion(regiones = Preguntas$Regiones$shp,
                                                              variable = "voto_partido",
                                                              diseno = Preguntas$Regiones$diseno)

  # Estructura

  # La base de salida debe contener los ID's de las regiones, su geometría
  testthat::expect_contains(object = names(bd_analizar_frecuenciasRegion), expected =  c("region", "geometry"))

  # Conservar el número de regiones (hacer un left_join correcto)}
  testthat::expect_equal(object = nrow(bd_analizar_frecuenciasRegion), expected = nrow(Preguntas$Regiones$shp))

})


test_that("Cómputo correcto de las estimaciones", {

  diseno <- survey::svydesign(ids = ~1, data = datos_demo |> mutate(region = sample(x = seq.int(1:6), size  = n(), replace = T)))

  Preguntas <- Pregunta2$new(encuesta = encuesta_demo, tema = tema_default)

  bd_analizar_frecuenciasRegion <- analizar_frecuenciasRegion(regiones = Preguntas$Regiones$shp,
                                                              variable = "recibir_num",
                                                              diseno = Preguntas$Regiones$diseno) |>
    as_tibble() |>
    select(!c(`n()`, geometry, contains("se"))) |>
    janitor::clean_names()

  tbl_analizar_frecuenciasRegion <- bd_analizar_frecuenciasRegion |>
    mutate(tot = rowSums(across(.cols = !region), na.rm = T),
           max = max(across(.cols = !region))) |>
    ungroup()

  # Suma normalizada a través de las regiones
  testthat::expect_lte(object = tbl_analizar_frecuenciasRegion |>
                         distinct(tot) |>
                         filter(tot == max(tot)) |>
                         pull(), expected = 1)

  # Cálculo correcto
  testthat::expect_lte(object = tbl_analizar_frecuenciasRegion |>
                         distinct(max) |>
                         pull(),
                       expected = 1)

})
