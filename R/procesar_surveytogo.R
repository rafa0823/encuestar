

#' Title
#'
#' @param encuesta
#' @param pregunta
#'
#' @return
#' @export
#'
#' @examples
analizar_frecuencias <- function(encuesta, pregunta){
  estimacion <-survey::svymean(enquo(pregunta),
                               design = encuesta$diseño, na.rm = T) %>%
    tibble::as_tibble(rownames = "respuesta") %>%
    rename(media=2, ee=3) %>%
    mutate(respuesta = stringr::str_replace(
      pattern = rlang::expr_text(ensym(pregunta)),
      replacement = "",
      string = respuesta),
      pregunta = rlang::expr_text(ensym(pregunta)),
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
                        aux <- encuesta$cuestionario$diccionario %>% unnest(respuestas) %>% filter(grepl(.x,respuestas)) %>% pull(respuestas) %>% str_replace("\\s*\\{[^\\)]+\\} ","")
                        if(length(aux) == 0) aux <- .x
                        survey::svymean(survey::make.formula(.x),
                                        design = encuesta$diseño, na.rm = T) %>%
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
    filter(llaves %in% !!llaves) %>% pull(pregunta) %>% unique
  estimaciones <- estimaciones %>% mutate(pregunta = p)
}
