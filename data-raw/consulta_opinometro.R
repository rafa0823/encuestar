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


  DBI::dbListTables(conn = pool)


ds2 <-
  dplyr::tbl(src = pool, "ds_e2") |>
    dplyr::collect()


tibble("Dato" = c("Registros", "Duracion (minutos")) |>
  bind_cols(ds2 |>
              filter(lubridate::as_date("2023-07-01") <= fecha_inicio, fecha_inicio <= lubridate::as_date("2023-08-01")) |>
              count(fecha = lubridate::as_date(fecha_inicio)) |>
              tidyr::pivot_wider(names_from = fecha, values_from = n) |>
              bind_rows(ds2 |>
                          filter(lubridate::as_date("2023-07-01") <= fecha_inicio, fecha_inicio <= lubridate::as_date("2023-08-01")) |>
                          transmute(fecha = lubridate::as_date(fecha_inicio),
                                    duracion = difftime(fecha_fin, fecha_inicio, units = "mins")) |>
                          group_by(fecha) |>
                          summarise(duracion = sum(duracion, na.rm = TRUE)/lubridate::dhours(1)) |>
                          tidyr::pivot_wider(names_from = fecha, values_from = duracion))) |>
  writexl::write_xlsx(path = "data-raw/dialogo_social_e2_registros_duracion.xlsx")



