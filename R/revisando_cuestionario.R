if(getRversion() >= "2.15.1")  utils::globalVariables(c("style_name","level","row_span",
                                                        "text","bloque","tipo_pregunta",
                                                        "tema"))
#' Title
#'
#' @param doc
#'
#' @return
#' @export
#'
#' @examples

diccionario_cuestionario <- function(doc, patron){
  diccionario <- doc %>%
    # officer::docx_summary(doc) %>%
    # as_tibble() %>%
    filter(!is.na(style_name),style_name %in% c("Morant_Bloque","Morant_Pregunta",
                                                "Morant_respuestas_aspectos",
                                                "Morant_respuestas_abiertas",
                                                "Morant_respuestas_numericas",
                                                "Morant_respuestas_multiples",
                                                "Morant_respuestas_orden")) %>%
    select(-c(level:row_span)) %>%
    mutate(bloque=ifelse(style_name=="Morant_Bloque" & text != "", text, NA)) %>%
    fill(bloque,.direction = c("down")) %>%
    filter(style_name!="Morant_Bloque") %>%
    mutate(bloque = factor(bloque,levels = unique(bloque))) %>%
    mutate(pregunta=ifelse(style_name=="Morant_Pregunta", text, NA)) %>%
    fill(pregunta,.direction = c("down")) %>%
    filter(!style_name=="Morant_Pregunta") %>%
    separate(style_name, c("a", "b", "c"), sep = "_") %>%
    rename("tipo_pregunta"="c") %>%
    mutate(llaves=stringr::str_extract(pregunta, "(?<=\\{).+?(?=\\})"),
           pregunta=stringr::str_remove(pregunta, "\\{.+\\}"),
           llaves= if_else(str_detect(pattern = "\\{",string =  text), true = stringr::str_extract(text, "(?<=\\{).+?(?=\\})"), false = llaves),
           text=stringr::str_remove(text, "\\{.+\\}"),
           llaves=stringr::str_squish(llaves),
           pregunta=stringr::str_squish(pregunta),
           text=stringr::str_squish(text)
    ) %>% mutate(llaves = factor(llaves, unique(llaves))) %>% filter(text != "")

  tipo_r <- diccionario %>% distinct(tipo_pregunta) %>% pull(1)
  aspectos <- diccionario %>%
    semi_join(diccionario%>% filter(tipo_pregunta == "aspectos"), by = "pregunta") %>%
    select(tipo_pregunta, text, pregunta, bloque) %>%
    pivot_wider(names_from = tipo_pregunta, values_from = text)


  if(nrow(aspectos)>0){
    for(i in tipo_r){
      aspectos <- aspectos %>% unnest(all_of(i), keep_empty = T)
    }
    tr <- aspectos %>% select(-pregunta,-bloque, -aspectos) %>% names

    aspectos <- aspectos %>%
      rowwise() %>%
      mutate(tipo_pregunta = tr[which(!is.na(c_across(cols = all_of(tr))))]) %>%
      unite(text, tr) %>%
      mutate(text = stringr::str_replace_all(pattern = "_NA|NA_",replacement = "",string = text)) %>%
      left_join(diccionario %>% select(aspectos = text, pregunta, bloque, llaves)) %>%
      rename(tema = aspectos) %>% distinct(.keep_all = T)

  }


  diccionario <- diccionario %>%
    anti_join(diccionario %>% filter(tipo_pregunta == "aspectos"), by = "pregunta") %>%
    select(bloque, pregunta, tipo_pregunta, llaves, text) %>%
    bind_rows(aspectos) %>% {if("tema" %in% names(.)) {
      group_by(.,bloque, pregunta, tipo_pregunta, llaves, tema)
    } else {
      group_by(.,bloque, pregunta, tipo_pregunta, llaves)
    }} %>%
    { if(!is.na(patron)){
      mutate(., text = stringr::str_squish(gsub(x = text, pattern = patron,"")))
    } else{
      .
    }} %>%
    summarise(respuestas=list(text)) %>%
    ungroup() %>% arrange(llaves)

  return(diccionario)
}
