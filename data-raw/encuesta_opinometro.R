# SCRIPT DE PRUEBAS PARA GENERAR UNA CLASE ENCUESTA USANDO COMO INSUMO EL OPINOMETRO
# Al generar la clase, se escriben los archivos ./R/constantes.R, ./R/funciones.R y un folder
# llamado ./auditoria/ los cuales no forman parte edl desarrollo salvo que se esté trabajando en la
# encuesta demo

# Preambulo -----------------------------------------------------------------------------------

library(dplyr)
library(encuestar)

# Insumos -------------------------------------------------------------------------------------

shp_hermosillo_agosto <-
  readr::read_rds("./data-raw/shp.rda")

diseno_hermosillo_agosto <-
  readr::read_rds("./data-raw/diseno.rda")

diccionario_hermosillo_agosto <-
  readxl::read_xlsx(path = "./data-raw/diccionario_enc_demo.xlsx") |>
  dplyr::filter(!grepl(pattern = "Registro de ubicación|Filtros", x = bloque)) |>
  filter(llaves == "problema_principal")

# data-raw ------------------------------------------------------------------------------------

pool <-
  pool::dbPool(
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
         SECCION = sample(diseno_hermosillo_agosto$muestra$SECCION |>
                            distinct(cluster_2) |>
                            mutate(cluster_2 = as.character(cluster_2)) |>
                            pull(),
                          size = nrow(.),
                          replace = TRUE),
         problema_principal = sample(c("A", "B", "C"),
                                     size = nrow(.),
                                     replace = TRUE),
         MUNI = sample(c("mun_a", "mun_a", "mun_a"),
                                     size = nrow(.),
                                     replace = TRUE)
         )

# Base de eliminadas --------------------------------------------------------------------------

eliminadas <-
  tibble(SbjNum = sample(bd_id163$SbjNum,
                         size = 12,
                         replace = TRUE),
         razon = sample(c("razon_a", "razon_b", "razon_c"),
                        size = 12,
                        replace = TRUE))

quitar <- c()

mantener <- ""

# Clase -------------------------------------------------------------------

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
