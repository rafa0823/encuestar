test_that("Estructura de la base de datos de salida", {

  browser()

  devtools::load_all()

  diseno <- survey::svydesign(ids = ~1, data = datos_demo |> mutate(region = sample(x = seq.int(1:6), size  = n(), replace = T)))

  Preguntas <- Pregunta2$new(encuesta = encuesta_demo, tema = tema_default)

  variable = "voto_partido"

  bd_calcular_ganadorRegion <- encuestar:::calcular_ganadorRegion(diseno = diseno,
                                                                  regiones = Preguntas$Regiones$shp,
                                                                  variable = variable,
                                                                  lugar = 1)

  # Estructura
  testthat::expect_equal(object = length(bd_calcular_ganadorRegion), expected = 5)

  testthat::expect_equal(object = names(bd_calcular_ganadorRegion), c("region", "n()", "geometry", variable, "n"))


})
