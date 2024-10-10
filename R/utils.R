#' Obtener telemetría de intento efectivo de encuesta
#'
#' @param bd_respuestas Base de datos con formato exportado de SurveyToGo
#' @param id Id único que identifica cada entrevista, generalmente se llama SbjNum
#' @param intento_efectivo Veces que se intenta levantar una entrevista hasta que esta comienza
#'
#' @return
#' @export
#'
#' @examples
obtener_ubicacionEfectiva_surveyToGo = function(bd_respuestas, id, intento_efectivo) {
  bd_respuestas |>
    filter(SbjNum == id) |>
    select(SbjNum,
           paste0("INT",
                  intento_efectivo),
           paste0("GPS_INT",
                  intento_efectivo,
                  "_",
                  c("LA", "LO", "ALT", "BEAR", "SPEED", "DATE"))) |>
    mutate(across(.cols = !c(SbjNum), .fns = ~ as.character(.x)),
           intento_efectivo = intento_efectivo) |>
    relocate(intento_efectivo, .after = SbjNum) |>
    rename_with(~ gsub(pattern = as.character(intento_efectivo), replacement = "", x = .),
                .cols = everything())
}
#' Title
#'
#' @param nombre
#' @param extension
#' @param tolerancia
#'
#' @return
#' @export
#'
#' @examples
formato_archivo = function(nombre, extension, tolerancia = 10) {
  paste0(nombre,
         "_",
         gsub(pattern = "-", replacement = "", x = Sys.Date()),
         "_",
         format(Sys.time(), "%H"),
         as.character(floor(as.integer(format(Sys.time(), "%M")) / tolerancia) * tolerancia),
         "h.",
         extension)
}
#' Calcular el porcentaje de intentos rechazados de inciar una encuesta
#'
#' @param bd_respuestas_efectivas [tibble::tibble()] que contiene las respuesyas efectivas
#' @param usuario_plataforma Logical. Para determinar si el calculo se hace por usuario de plataforma o por
#'  total de respuestas
#'
#' @return [tibble::tibble()] con variable rechazo en escala de porcentaje natural
#'
#' @examples
calcular_tasa_rechazo <- function(bd_respuestas_efectivas, por_usuario = FALSE){
  bd_respuestas_efectivas %>%
    {
      if(!por_usuario) {
        summarise(.data = .,
                  rechazo = (sum(as.integer(intento_efectivo)) - n())/sum(as.integer(intento_efectivo)))
      }
      else {
        group_by(.data = .,
                 Srvyr) %>%
          summarise(rechazo = (sum(as.integer(intento_efectivo)) - n())/sum(as.integer(intento_efectivo))) |>
          arrange(desc(rechazo))
      }
    }
}
