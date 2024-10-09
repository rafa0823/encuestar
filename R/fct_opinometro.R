#' Title
#'
#' @param pool
#' @param id_cuestionario
#'
#' @return
#' @export
#'
#' @examples
determinarVariables_cuestinoarioOpinometro <- function(pool, id_cuestionario){
  tbl(src = pool, "Encuesta") |>
    filter(Id == id_cuestionario) |>
    collect() %>%
    purrr::pmap_df(function(JsonData, Descripcion, ...){
      aux <-
        JsonData |>
        jsonlite::fromJSON() |>
        as_tibble()|>
        tidyr::unnest(pages)|>
        as_tibble()|>
        tidyr::unnest(elements, names_sep = '')}) |>
    pull(elementsname)
}
#' Title
#'
#' @param pool
#' @param codigos
#' @param encuesta_id
#'
#' @return
#'
#' @examples
consultar_respuestas <- function(pool, codigos, encuesta_id){
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
    left_join(tbl(pool, "Usuarios") |>
                semi_join(tbl(pool, "UsuariosEncuesta") |>
                            filter(EncuestaId == encuesta_id),
                          join_by(Id == UsuarioId)) |>
                select(UsuarioNum = Num, Nombre, APaterno, AMaterno),
              by = "UsuarioNum") |>
    janitor::clean_names() |>
    collect()
}
#' Title
#'
#' @param bd_respuestasOpinometro
#' @param variables_cuestionario
#'
#' @return
#' @export
#'
#' @examples
rectificar_respuestasOpinometro <- function(bd_respuestasOpinometro, variables_cuestionario){
  bd_respuestasOpinometro |>
    mutate(intentos = stringr::str_trim(string = intentos, side = "both")) |>
    filter(intentos == "Abrieron la puerta, aceptaron la entrevista y cumple el perfil") |>
    filter(ubicacion_aplicada != "No aplica") |>
    mutate(ubicacion_aplicada = dplyr::if_else(condition = ubicacion_aplicada == ",",
                                               true = NA_character_,
                                               false = ubicacion_aplicada)) |>
    tidyr::separate(col = ubicacion_aplicada,
                    into = c("Latitude", "Longitude"),
                    sep = ",",
                    remove = TRUE) |>
    transmute(SbjNum = id,
              Date = lubridate::as_datetime(fecha_inicio, tz = "America/Mexico_City"),
              Srvyr = paste(nombre, a_paterno, a_materno, sep = " "),
              VStart = lubridate::as_datetime(fecha_inicio),
              VEnd = lubridate::as_datetime(fecha_fin),
              Duration = as.character(difftime(VEnd, VStart, units = "hours")),
              Latitude,
              Longitude,
              across(all_of(variables_cuestionario)),
              corte = update(Sys.time(), minute = floor(lubridate::minute(Sys.time())/15)*15, second = 0, tz = "America/Mexico_City"),
              SECCION = as.character(cluster)) |>
    filter(Date <= corte)
}
#' Title
#'
#' @param bd_respuestasOpinometro
#'
#' @return
#' @export
#'
#' @examples
calcular_tasaRechazo_opinometro <- function(bd_respuestasOpinometro){
  bd_respuestasOpinometro |>
    transmute(SbjNum = id,
              Srvyr = paste(nombre, a_paterno, a_materno, sep = " "),
              intentos = stringr::str_trim(string = intentos, side = "both")) |>
    mutate(intento_efectivo = dplyr::case_when(intentos == "Abrieron la puerta, aceptaron la entrevista y cumple el perfil" ~ "efectivo",
                                               .default = "no efectivo")) |>
    mutate(flag = cumsum(intento_efectivo == "efectivo")) %>%
    group_by(flag) %>%
    mutate(intento_efectivo = n()) %>%
    ungroup() %>%
    select(SbjNum, intento_efectivo)
}
