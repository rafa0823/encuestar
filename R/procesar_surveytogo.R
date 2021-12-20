

analizar_frecuencias <- function(encuesta, pregunta){
  # browser()
  # encuesta$diccionario %>%
  #   filter(pregunta==rlang::expr_text(ensym(pregunta)))
  estimacion <-survey::svymean(enquo(pregunta),
                               design = encuesta$diseño) %>%
    tibble::as_tibble(rownames = "respuesta") %>%
    rename(media=mean, ee=SE) %>%
    mutate(respuesta = stringr::str_replace(
      pattern = rlang::expr_text(ensym(pregunta)),
      replacement = "",
      string = respuesta),
      respuesta=forcats::fct_reorder(.f = respuesta,
                                     .x = media,
                                     .fun = max))

}

analizar_frecuencias_aspectos <- function(encuesta, pregunta, aspectos){
  llaves <- glue::glue("{rlang::expr_text(ensym(pregunta))}_{aspectos}")

  estimaciones <- map_df(llaves,
                      ~{
                        survey::svymean(survey::make.formula(.x),
                                        design = encuesta$diseño) %>%
                          tibble::as_tibble(rownames = "respuesta") %>%
                          rename(media=mean, ee=SE) %>%
                          mutate(
                            aspecto=glue::glue("temporal{.x}"),
                            respuesta = stringr::str_replace(
                            pattern = .x,
                            replacement = "",
                            string = respuesta),
                            respuesta=forcats::fct_reorder(.f = respuesta,
                                                           .x = media,
                                                           .fun = max))
                      })

}
