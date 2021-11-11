# Por lo pronto no se ha realizado la clase.
# Se asume que un SurveyToGo es una lista que contiene los siguiente elementos
# encuesta=base de datos tal como la entrega el sistema de STG
# diccionario= base de datos con las columnas, nombre, llave
#

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
