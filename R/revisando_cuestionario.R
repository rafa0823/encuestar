
revisando_cuestionario <- function(base){
  bd <- officer::docx_summary(base) %>%
  filter(!is.na(style_name),style_name %in% c("Morant_Bloque","Morant_Pregunta",
                                              "Morant_respuetas")) %>%
  select(-c(level:row_span)) %>%
  mutate(bloque=ifelse(style_name=="Morant_Bloque", text, NA)) %>%
  fill(bloque,.direction = c("down")) %>%
  filter(!style_name=="Morant_Bloque") %>%
  mutate(pregunta=ifelse(style_name=="Morant_Pregunta", text, NA)) %>%
  fill(pregunta,.direction = c("down")) %>%
  filter(!style_name=="Morant_Pregunta") %>%
  group_by(bloque, pregunta) %>%
  summarise(respuestas=list(text)) %>%
  ungroup() %>%
  mutate(llaves=stringr::str_extract(pregunta, "\\{.+\\}"))

  return(bd)
}

