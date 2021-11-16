

analizar_frecuencias <- function(encuesta, pregunta){
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
