
# Preambulo -----------------------------------------------------------------------------------

library(dplyr)
# Cargar la version de desarrollo de la librería encuestar que deberá estar instalada en sistema
library(encuestar)

# Insumos -------------------------------------------------------------------------------------

#+ Se carga el archivo shp con la cartografia de la zona de la encuesta
shp_hermosillo_agosto <- readr::read_rds("./data-raw/shp.rda")

#+ Se carga el diseno de la encuesta generado con la paqueteria muestreaR
diseno_hermosillo_agosto <- readr::read_rds("./data-raw/diseno.rda")

#+ Se carga el diccionario de variables en la encuesta
diccionario_hermosillo_agosto <-
  readxl::read_xlsx(path = "./data-raw/diccionario_enc_demo.xlsx") |> #dicc_enc_hermosillo_agosto.xlsx
  dplyr::filter(!grepl(pattern = "Registro de ubicación|Filtros", x = bloque)) |>
  filter(llaves == "problema_principal")

# data-raw ------------------------------------------------------------------------------------

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

## Metodo antiguo -----------------------------------------------------------------------------

bd_raw_id163 <-
  obtener_respuesta(pool = pool,
                    codigos = "variable_nula",
                    encuesta_id = 163) |>
  dplyr::collect() |>
  select(!variable_nula) |>
  filter(ubicacion_aplicada != "No aplica") |>
  mutate(ubicacion_aplicada = dplyr::if_else(condition = ubicacion_aplicada == ",",
                                             true = NA_character_,
                                             false = ubicacion_aplicada)) |>
  tidyr::separate(col = ubicacion_aplicada,
                  into = c("Latitude", "Longitude"),
                  sep = ",",
                  remove = FALSE) %>%
  transmute(SbjNum = id,
            Date = lubridate::as_datetime(fecha_inicio),
            Latitude, Longitude,
            VStart = lubridate::as_datetime(fecha_inicio),
            VEnd = lubridate::as_datetime(fecha_fin),
            Srvyr = usuario_num)

bd_id163 <-
  bd_raw_id163 |>
  bind_rows(bd_raw_id163) %>%
  mutate(SbjNum = row_number(),
         edad = sample(seq.int(from = 18, to = 80, by = 1),
                       size = nrow(.),
                       replace = TRUE),
         sexo = sample(c("Mujer", "Hombre"),
                       size = nrow(.),
                       replace = TRUE),
         problema_principal = sample(c("A", "B", "C"),
                                     size = nrow(.),
                                     replace = TRUE),
         SECCION = sample(diseno_hermosillo_agosto$muestra$SECCION |>
                            distinct(cluster_2) |>
                            mutate(cluster_2 = as.character(cluster_2)) |>
                            pull(),
                          size = nrow(.),
                          replace = TRUE))

## Metodo con base en variables ---------------------------------------------------------------

variables_id163 <-
  tbl(src = pool, "Registros") |>
  filter(EncuestaId == 163) |>
  collect() %>%
  purrr::pmap_df(function(Id, fecha, Resultado, UsuarioNum, ...){
    aux <- Resultado |>
      jsonlite::fromJSON() |>
      as_tibble()
    }) |>
  select(!c(starts_with("Pregunta"), ObtenerGPS)) |> # Omitir variables que no tienen nombre o desconocidas
  colnames()

bd_raw_id163 <-
  obtener_respuesta(pool = pool,
                    codigos = variables_id163,
                    encuesta_id = 163) |>
  left_join(tbl(pool, "Usuarios") |>
              semi_join(
                tbl(pool, "UsuariosEncuesta") |>
                  filter(EncuestaId == 163), join_by(Id == UsuarioId)) |>
              select(Num, Nombre, APaterno, AMaterno),
            by = c("usuario_num" = "Num")) |>
  dplyr::collect() |>
  filter(ubicacion_aplicada != "No aplica") |>
  mutate(ubicacion_aplicada = dplyr::if_else(condition = ubicacion_aplicada == ",",
                                             true = NA_character_,
                                             false = ubicacion_aplicada)) |>
  tidyr::separate(col = ubicacion_aplicada,
                  into = c("Latitude", "Longitude"),
                  sep = ",",
                  remove = FALSE) %>%
  transmute(
    SbjNum = id,
    Date = lubridate::as_datetime(fecha_inicio),
    Latitude, Longitude,
    VStart = lubridate::as_datetime(fecha_inicio),
    VEnd = lubridate::as_datetime(fecha_fin),
    Srvyr = paste0(Nombre, APaterno, AMaterno),
    across(all_of(variables_id163))
  )

bd_id163 <-
  bd_raw_id163 %>%
  mutate(SbjNum = row_number(),
         edad = sample(seq.int(from = 18, to = 80, by = 1),
                       size = nrow(.),
                       replace = TRUE),
         sexo = sample(c("Mujer", "Hombre"),
                       size = nrow(.),
                       replace = TRUE),
         problema_principal = sample(c("A", "B", "C"),
                                     size = nrow(.),
                                     replace = TRUE),
         SECCION = sample(diseno_hermosillo_agosto$muestra$SECCION |>
                            distinct(cluster_2) |>
                            mutate(cluster_2 = as.character(cluster_2)) |>
                            pull(),
                          size = nrow(.),
                          replace = TRUE))

# Base de eliminadas --------------------------------------------------------------------------
#+ Se agrega la lista de encuestas eliminadas por auditoria
eliminadas <-
  readxl::read_excel(path = "./data-raw/bd_eliminadas_hermosillo_agosto.xlsx")

#+ En caso de existir varibales que no se vayan a utilizar, se agrega la lista de variables que no se vayan a considerar
# Omitir variables
quitar <- c()

# Clusters a los que forzar entrevistas
mantener <- ""

# Clase -------------------------------------------------------------------
#+ Se crea la clase encuesta, que contiene la base de datos, asi como el diseno muestral, y la app de auditoria
encuesta_demo <- Encuesta$new(respuestas = bd_id163,
                              # n_simulaciones = 200,
                              quitar_vars = quitar,
                              mantener = mantener,
                              bd_correcciones = NULL,
                              muestra = diseno_hermosillo_agosto,
                              auditoria_telefonica = eliminadas,
                              cuestionario = diccionario_hermosillo_agosto,
                              shp = shp_hermosillo_agosto,
                              sin_peso = F,
                              tipo_encuesta = "ine",
                              mantener_falta_coordenadas = F, # mantener entrevistas sin coordenadas
                              rake = T, ######### con postestratificacion
                              patron = "\\(No leer\\)| \\(No leer\\)|\\(ROTAR\\)|\\(No leer)|:",
                              auditar = c("")
)

encuesta_demo$auditoria$run_app()

# usethis::use_data(encuesta_demo, encuesta_demo, internal = TRUE, overwrite = TRUE)
usethis::use_data(encuesta_demo, overwrite = TRUE)
