
revisando_cuestionario <- function(doc){
  bd <- officer::docx_summary(doc) %>%
    as_tibble() %>%
    filter(!is.na(style_name),style_name %in% c("Morant_Bloque","Morant_Pregunta",
                                                "Morant_respuestas_abiertas",
                                                "Morant_respuestas_numericas",
                                                "Morant_respuetas_multiples")) %>%
    select(-c(level:row_span)) %>%
    mutate(bloque=ifelse(style_name=="Morant_Bloque", text, NA)) %>%
    fill(bloque,.direction = c("down")) %>%
    filter(style_name!="Morant_Bloque") %>%
    mutate(pregunta=ifelse(style_name=="Morant_Pregunta", text, NA)) %>%
    fill(pregunta,.direction = c("down")) %>%
    filter(!style_name=="Morant_Pregunta") %>%
    separate(style_name, c("a", "b", "tipo_pregunta"), sep = "_") %>%
    group_by(bloque, pregunta, tipo_pregunta) %>%
    summarise(respuestas=list(text)) %>%
    ungroup() %>%
    mutate(llaves=stringr::str_extract(pregunta, "(?<=\\{).+?(?=\\})"),
           llaves=stringr::str_squish(llaves),
           pregunta=stringr::str_remove(pregunta, "\\{.+\\}"),
           pregunta=stringr::str_squish(pregunta))

  return(bd)
}


revisando_aspectos_cuestionario <- function(base){


bd <- officer::docx_summary(base) %>%
  filter(!is.na(style_name),
         !is.na(text),
         !text=="",
         style_name %in% c( "Grid Table Light")) %>%
  select(-c(level:row_span)) %>%
  mutate(style_name=if_else(str_detect(text, "\\{"),"pregunta",
                            if_else(str_detect(text, "\\("), "aspecto", "respuesta"))) %>%
  filter(style_name=="aspecto") %>%
  mutate(llaves=stringr::str_extract(text, "(?=\\().*?(?<=\\))"),
         llaves=gsub("\\(", "", llaves),
         llaves=gsub("\\)", "", llaves),
         aspecto=stringr::str_remove(text, "\\(.+\\)"),
         aspecto=stringr::str_squish(aspecto)) %>%
  select(doc_index, aspecto, llaves)



segunda_tabla <- officer::docx_summary(base) %>%
  filter(!is.na(style_name),style_name %in% c(
    "Morant_Bloque", "Grid Table Light")) %>%
  select(-c(level:row_span)) %>%
  mutate(bloque=ifelse(style_name=="Morant_Bloque", text, NA)) %>%
  fill(bloque,.direction = c("down")) %>%
  filter(!style_name=="Morant_Bloque" & !text=="") %>%
  mutate(pregunta=ifelse(str_detect(text, "\\{"), text, NA),
         llaves=stringr::str_extract(pregunta, "(?<=\\{).+?(?=\\})"),
         llaves=stringr::str_squish(llaves),
         pregunta=stringr::str_remove(pregunta, "\\{.+\\}"),
         pregunta=stringr::str_squish(pregunta)) %>%
  filter(!is.na(pregunta)) %>%
  select(doc_index, bloque, pregunta, llaves) %>%
  left_join(nest(bd, !doc_index), by="doc_index") %>%
  rename("aspecto"="data")

return(segunda_tabla)

}




