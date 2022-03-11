if(getRversion() >= "2.15.1")  utils::globalVariables(c("respuesta", "media", "llaves"))

#' Title
#'
#' @param encuesta
#' @param pregunta
#'
#' @return
#' \item{estimacion}{Tabla con las estimaciones de frecuencia para cada categoría respondida}
#' @export
#'
#' @examples

analizar_frecuencias <- function(encuesta, pregunta){
  estimacion <-survey::svymean(enquo(pregunta),
                               design = encuesta$muestra$diseno, na.rm = T) %>%
    tibble::as_tibble(rownames = "respuesta") %>%
    rename(media=2, ee=3) %>%
    mutate(respuesta = stringr::str_replace(
      pattern = rlang::expr_text(ensym(pregunta)),
      replacement = "",
      string = respuesta),
      pregunta = rlang::expr_text(ensym(pregunta)),
      respuesta = stringr::str_replace_all(respuesta, " \\(No leer\\)",""),
      respuesta=forcats::fct_reorder(.f = respuesta,
                                     .x = media,
                                     .fun = max)
      )
  p <- estimacion %>% pull(pregunta) %>% unique
  p <- encuesta$cuestionario$diccionario %>%
    filter(llaves == p) %>% pull(pregunta)
  estimacion <- estimacion %>% mutate(pregunta = p)
  return(estimacion)
}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("aspecto"))

#' Title
#'
#' @param encuesta
#' @param pregunta
#' @param aspectos
#'
#' @return
#' @export
#'
#' @examples

analizar_frecuencias_aspectos <- function(encuesta, pregunta, aspectos){
  p <- rlang::expr_text(ensym(pregunta))
  llaves <- glue::glue("{p}_{aspectos}")

  estimaciones <- map_df(llaves,
                      ~{
                        aux <- encuesta$cuestionario$diccionario %>% tidyr::unnest(respuestas) %>% filter(grepl(.x,respuestas)) %>% pull(respuestas) %>% str_replace("\\s*\\{[^\\)]+\\} ","")
                        if(length(aux) == 0) aux <- .x
                        survey::svymean(survey::make.formula(.x),
                                        design = encuesta$muestra$diseno, na.rm = T) %>%
                          tibble::as_tibble(rownames = "respuesta") %>%
                          rename(media=2, ee=3) %>%
                          mutate(
                            aspecto = aux,
                            respuesta = stringr::str_replace(
                            pattern = .x,
                            replacement = "",
                            string = respuesta),
                            respuesta=forcats::fct_reorder(.f = respuesta,
                                                           .x = media,
                                                           .fun = max))
                      })

  p <- encuesta$cuestionario$diccionario %>%
    filter(llaves %in% !!llaves) %>% transmute(pregunta, aspecto = as.character(llaves))
  estimaciones <- estimaciones %>% mutate(aspecto = as.character(aspecto)) %>% left_join(p)
}
