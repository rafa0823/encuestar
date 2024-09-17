pool <- pool::dbPool(
  drv = odbc::odbc(),
  Driver= 'ODBC Driver 17 for SQL Server',
  Database = "SVNET",
  Server = "tcp:morant.database.windows.net",
  UID = "emorones",
  PWD = "Mor@nt2024",
  Port = 1433,
  timeout = 120
)



obtener_respuesta <- function(pool, codigos, encuesta_id){
  query_claves <- paste0("REPLACE(JSON_VALUE(r.Resultado, '$.", codigos, "'), 'ñ', 'n') AS ", codigos, collapse = ", ")
  encuesta_id <- encuesta_id |> toString()

  query <- glue::glue("
  SELECT
    r.Id,
    r.EncuestaId,
    r.FechaInicio,
    r.FechaFin,
    r.FechaCreada,
    r.UbicacionAplicada,
    r.UsuarioNum,
    r.isComplete,
    ", query_claves, "
  FROM
    Registros r
  WHERE EncuestaId in ({encuesta_id})
")

  dplyr::tbl(pool, dplyr::sql(query)) |>
    janitor::clean_names()
}

# id de prueba 163

hola <- obtener_respuesta(pool = pool,
                          codigos = "asd",
                          encuesta_id = 163) |>
  dplyr::collect()


hola |>
  tidyr::separate(col = ubicacion_aplicada,
                  into = c("Latitude", "Longitude"),
                  sep = ",",
                  remove = FALSE) %>%
  mutate(sexo = sample(c("F", "M"),
                       size = nrow(.),
                       replace = TRUE),
         edad = sample(seq.int(from = 18, to = 99, by = 1),
                       size = nrow(.),
                       replace = TRUE)) |>
  select(SbjNum = ubicacion_aplicada,
         Latitude, Longitude,
         sexo, edad)


  glimpse()
  select(ubicacion_aplicada, Latitude, Longitude)
  glimpse()
