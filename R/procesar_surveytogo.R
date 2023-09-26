if(getRversion() >= "2.15.1")  utils::globalVariables(c("respuesta", "media", "llaves"))

#' Analizar frecuencias
#'
#' @param diseno Diseño muestral que contiene la información de pesos de cada individuo
#' @param pregunta Nombre de la variable asociada al análisis de la muestra
#'
#' @return
#' \item{estimacion}{Tabla con las estimaciones de frecuencia para cada categoría respondida}
#' @export
#'
#' @examples

analizar_frecuencias <- function(diseno, pregunta){

  estimacion <- survey::svymean(enquo(pregunta),
                                design = diseno, na.rm = T) %>%
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

  ja <- try(
    rlang::expr_text(ensym(pregunta)),T
  )

  if(class(ja) != "try-error"){
    p <- rlang::expr_text(ensym(pregunta))
    llaves <- glue::glue("{p}_{aspectos}")
  } else{
    llaves <- aspectos
  }


  estimaciones <- map_df(llaves,
                         ~{
                           if(class(ja) != "try-error"){
                             aux <- encuesta$cuestionario$diccionario %>% tidyr::unnest(respuestas) %>% filter(grepl(.x,respuestas)) %>% pull(respuestas) %>% str_replace("\\s*\\{[^\\)]+\\} ","")
                           } else{
                             aux <- .x
                           }
                           if(length(aux) == 0) aux <- .x
                           prev <- survey::svymean(survey::make.formula(.x),
                                                   design = encuesta$muestra$diseno, na.rm = T)

                           prev %>%
                             tibble::as_tibble(rownames = "respuesta") %>%
                             rename(media=2, ee=3) %>%
                             left_join(
                               prev %>% confint() %>% tibble::as_tibble(rownames = "respuesta") %>%
                                 rename(inf=2, sup=3)
                             ) %>%
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


analizar_candidato_partido <- function(diseno, llave_partido, llave_conocimiento, respuesta_conoce, candidatos, corte_otro){
  partido <- paste(llave_partido,candidatos,sep = "_")
  conoce <- paste(llave_conocimiento,candidatos, sep = "_")

  conoce <- purrr::map_df(.x = conoce,.f = ~{
    aux <- survey::svymean(survey::make.formula(.x),
                           design = diseno, na.rm = T)
    int <- aux %>% confint()
    aux %>%
      tibble::as_tibble(rownames = "respuesta") %>%
      rename(media=2, ee=3) %>%
      left_join(
        int %>% tibble::as_tibble(rownames = "respuesta") %>% rename(inf = 2, sup = 3)
      ) %>%
      mutate(
        aspecto = .x,
        respuesta = stringr::str_replace(
          pattern = .x,
          replacement = "",
          string = respuesta))
  }) %>% filter(respuesta == respuesta_conoce)

  partido <- purrr::map_df(.x = partido,.f = ~{
    survey::svymean(survey::make.formula(.x),
                    design = diseno, na.rm = T) %>%
      tibble::as_tibble(rownames = "respuesta") %>%
      rename(media=2, ee=3) %>%
      mutate(
        aspecto = .x,
        respuesta = stringr::str_replace(
          pattern = .x,
          replacement = "",
          string = respuesta),
        respuesta=forcats::fct_lump(respuesta, w = media, prop = corte_otro, other_level = "Otro")) %>%
      count(respuesta, aspecto, wt = media, name = "media") %>% arrange(desc(media))
  }) %>%

    group_by(aspecto) %>% mutate(sup = cumsum(media),
                                 inf = lag(sup, default = 0),
                                 label = (inf +sup)/2)

  bases <- list(conoce = conoce, partido =  partido)

  return(list(conoce = conoce, partido =  partido))
}

organizar_opinion_saldo <- function(bd, llave_opinion, grupo_positivo, grupo_negativo){
  bd %>%
    mutate(!!rlang::sym(llave_opinion) := case_when(respuesta %in% grupo_positivo ~"Positiva",
                                                    respuesta %in% grupo_negativo ~"Negativa",
    )) %>% filter(!is.na(!!rlang::sym(llave_opinion))) %>%
    mutate(persona = stringr::str_replace(aspecto, glue::glue("{llave_opinion}_"), "")) %>%
    count(persona,tema, grupo = !!rlang::sym(llave_opinion), wt = media, name = "saldo") %>%
    mutate(saldo = if_else(grupo == "Negativa", -saldo, saldo))
}

ordenar_opinion_xq <- function(base, llave_opinion, llave_xq, aspectos, grupo_positivo, grupo_negativo){
  opiniones <- paste(llave_opinion, aspectos, sep = "_")
  xqs <- paste(llave_xq, aspectos, sep = "_")

  base %>%
    select(SbjNum, all_of(c(opiniones, xqs))) %>%
    pivot_longer(-SbjNum) %>% separate(name, c("llave", "persona")) %>%
    mutate(llave = stringr::str_replace(string = llave, replacement = "texto",pattern = llave_xq)) %>%
    pivot_wider(names_from = llave, values_from = value) %>%
    filter_all(all_vars(!is.na(.))) %>%
    mutate(grupo = case_when(!!rlang::sym(llave_opinion) %in% grupo_positivo ~"Positiva",
                             !!rlang::sym(llave_opinion) %in% grupo_negativo ~"Negativa",
    )) %>% filter(!is.na((grupo)))
}

#cada opinion entre candidatos
combinaciones_opiniones <- function(bd_texto, n_palabras){
  opinion <- bd_texto %>% split(.$grupo) %>% map_df(~{
    data_corpus <- corpus(.x, text_field = "texto") %>%
      tokens( remove_punct = TRUE) %>%
      tokens_remove(stopwords("spanish")) %>% tokens_group(groups = persona) %>%
      dfm()

    .x$persona %>% unique %>% map_df(~{
      textstat_keyness(data_corpus, measure = "lr", target =.x) %>%  tibble() %>% mutate(persona = .x) %>%
        slice(seq_len(n_palabras))
    }) %>% mutate(grupo = unique(.x$grupo))
  }) %>% group_by(persona, grupo) %>% summarise(p_calve = paste(feature, collapse = "\n"))
}

#cada opinion por candidatos
combinaciones_candidatos <- function(bd_texto, n_palabras){
  opinion <- bd_texto %>% split(.$persona) %>% map_df(~{
    data_corpus <- corpus(.x, text_field = "texto") %>%
      tokens( remove_punct = TRUE) %>%
      tokens_remove(stopwords("spanish")) %>% tokens_group(groups = grupo) %>%
      dfm()

    .x$grupo %>% unique %>% map_df(~{
      textstat_keyness(data_corpus, measure = "lr", target =.x) %>%  tibble() %>% mutate(grupo = .x) %>%
        slice(seq_len(n_palabras))
    }) %>% mutate(persona = unique(.x$persona))
  }) %>% group_by(persona, grupo) %>% summarise(p_calve = paste(feature, collapse = "\n"))
}

#todas las combinaciones
combinaciones_todas <- function(bd_texto, n_palabras){
  aux_junto <- bd_texto %>% unite(junto,persona,grupo)

  data_corpus <- corpus(aux_junto, text_field = "texto") %>%
    tokens( remove_punct = TRUE) %>%
    tokens_remove(stopwords("spanish")) %>% tokens_group(groups = junto) %>%
    dfm()

  aux_junto$junto %>% unique %>% map_df(~{
    textstat_keyness(data_corpus, measure = "lr", target =.x) %>%  tibble() %>% mutate(grupo = .x) %>%
      slice(seq_len(n_palabras))
  }) %>% group_by(grupo)%>% summarise(p_calve = paste(feature, collapse = "\n")) %>%
    separate(grupo, into = c("persona","grupo"))
}

#' Title
#'
#' @param bd_texto
#' @param tipo
#' @param n_palabras
#'
#' @return
#' @export
#' @import quanteda quanteda.textstats
#' @examples
pclave_combinaciones_saldo <- function(bd_texto, tipo, n_palabras){
  if(tipo == "opiniones"){
    res <- combinaciones_opiniones(bd_texto, n_palabras)
  }
  if(tipo == "candidatos"){
    res <- combinaciones_candidatos(bd_texto, n_palabras)
  }
  if(tipo == "todas"){
    res <- combinaciones_todas(bd_texto, n_palabras)
  }
  return(res)
}

analizar_frecuencia_region <- function(variable, diseno, diccionario){
  survey::svytable(survey::make.formula(c(variable,"region")), design = diseno) %>%
    as_tibble() %>% group_by(region) %>% mutate(pct = n/sum(n)) %>% mutate(llaves = variable) %>%
    left_join(
      diccionario %>% select(llaves, pregunta)
    )
}

#' Title
#'
#' @param llave
#' @param variable
#' @param respuesta
#' @param diseno
#' @param diccionario
#'
#' @return
#' @export
#'
#' @examples
analizar_conocimiento_region <- function(llave, variable, respuesta, diseno, diccionario){
  junto <- paste(llave, variable, sep = "_")

  junto %>%
    map_df(~{
      survey::svytable(survey::make.formula(c(.x,"region")), design = diseno) %>%
        as_tibble() %>% group_by(region) %>% mutate(pct = n/sum(n))%>% mutate(aspecto = .x) %>%
        filter(!!rlang::sym(.x) == respuesta) %>% select(-1)
    }) %>%
    left_join(
      diccionario %>% select(aspecto = llaves, tema)
    )
}

#' Title
#'
#' @param llave_opinion
#' @param candidatos
#' @param diseno
#' @param ns_nc
#' @param cat_negativo
#' @param cat_positivo
#'
#' @return
#' @export
#'
#' @examples
analizar_saldo_region <- function(llave_opinion, candidatos, ns_nc, cat_negativo, cat_regular, cat_positivo, diseno, diccionario){
  if(llave_opinion ==  "") llaves <- candidatos else llaves <- paste(llave_opinion, candidatos,sep = "_")

  res <- llaves %>% map_df(~{
    survey::svytable(survey::make.formula(c(.x,"region")), design = diseno) %>%
      as_tibble() %>% group_by(region) %>% mutate(pct = n/sum(n)) %>%
      filter(! (!!rlang::sym(.x) %in% c(ns_nc, cat_regular))) %>%
      ungroup %>%
      mutate(pct = if_else(!!rlang::sym(.x) %in% cat_negativo, -pct,pct)) %>%
      count(region, wt = pct,name = "saldo") %>% mutate(aspecto = .x)
  })

  if(is.null(names(candidatos))){
    res <- res %>%
      left_join(
        diccionario %>% select(aspecto = llaves, tema)
      )
  } else{
    res <- res %>% mutate(tema = names(candidatos[match(aspecto, candidatos)]))
  }
  return(res)
}

#' Title
#'
#' @param var
#' @param diseno
#'
#' @return
#' @export
#'
#' @examples

analizar_ganador_region <- function(regiones, var, lugar, diseno){
  formula <- survey::make.formula(c("region", rlang::expr_text(ensym(var))))

  regiones %>% left_join(
    survey::svytable(formula, design = diseno) %>% as_tibble %>%
      group_by(region) %>% filter(dense_rank(-n) == lugar)
  )
}
#' Title
#'
#' @param var
#' @param diseno
#'
#' @return
#' @export
#'
#' @examples
analizar_promedio_region <- function(regiones, var, diseno){
  formula <- survey::make.formula(rlang::expr_text(ensym(var)))
  regiones %>% left_join(
    survey::svyby(formula, ~region, design = diseno,FUN = survey::svymean, na.rm  = T) %>%
      as_tibble()
  )
}

#' Title
#'
#' @param bd
#' @param var
#'
#' @return
#' @export
#'
#' @examples
analizar_pclave_region <- function(bd, var){

  data_corpus <- corpus(bd, text_field = var) %>%
    tokens( remove_punct = TRUE) %>%
    tokens_remove(stopwords("spanish")) %>% tokens_group(groups = region) %>%
    dfm()

  bd$region %>% unique %>% map_df(~{
    textstat_keyness(data_corpus, measure = "lr", target =.x) %>%  tibble() %>% mutate(grupo = .x)
  }) %>% group_by(grupo) %>% summarise(pclave = paste(feature,collapse = ", "))
}

#' Title
#'
#' @param bd
#' @param vars
#' @param stimuli
#'
#' @return
#' @export
#'
#' @examples
analizar_blackbox_1d <- function(bd, vars, stimuli){
  stmli <- bd %>% select(stimuli =all_of(stimuli))
  basic <- bd %>% select(all_of(vars))
  nas <- basic %>% mutate(across(everything(), ~if_else(is.na(.x), 9999,.x)))

  issue <- basicspace::blackbox(nas, missing=c(9999),verbose=FALSE,dims=1,minscale=ncol(nas)/2+1)

  individuals <- issue$individuals %>% pluck(1) %>% as_tibble %>%
    bind_cols(stmli)

  orden <- individuals %>% group_by(stimuli = all_of(stimuli)) %>%
    summarise(media = mean(c1,na.rm = T)) %>% arrange(desc(media))

  individuals <- individuals %>% mutate(stimuli = factor(x = stimuli,levels =  orden %>% pull(stimuli)), stimuli*-1)
  return(
    list(
      stimuli = issue$stimuli %>% pluck(1),
      fits = issue$fits,
      individuals = individuals,
      slf = orden
    )
  )
}

analizar_morena <- function(encuesta, personajes, atributos){

  #opinión positiva
  o_p <- analizar_frecuencias_aspectos(encuesta$encuesta, opinion, personajes) %>%
    filter(respuesta == "Buena") %>%
    mutate(ganador = media == max(media), puntos = if_else(ganador, 2, 0)) %>%
    separate(aspecto, c("atributo", "personaje"), remove = F) %>%
    select(atributo, personaje, media, ganador, puntos, aspecto)

  #atributos
  atr_p <- atributos %>%
    pmap_df(function(atributo, puntos){
      analizar_frecuencias_aspectos(encuesta$encuesta, !!rlang::sym(atributo), personajes) %>%
        filter(respuesta %in% c("Mucho", "Algo")) %>% mutate(media = if_else(respuesta == "Algo", media *.5,media)) %>%
        group_by(aspecto) %>%
        summarise(media = sum(media)) %>%
        mutate(ganador = media == max(media),
               puntos = if_else(ganador,puntos,0)) %>%
        separate(aspecto, c("atributo", "personaje"),remove = F)
    })

  #buen candidato
  cand <- analizar_frecuencias_aspectos(encuesta$encuesta, buencandidato, personajes) %>%
    filter(respuesta == "Sí") %>%
    mutate(ganador = media == max(media), puntos = if_else(ganador, 1, 0)) %>%
    separate(aspecto, c("atributo","personaje"),remove = F) %>%
    select(atributo, personaje, media, ganador, puntos, aspecto)

  #votaria
  voto <- analizar_frecuencias_aspectos(encuesta$encuesta, votaria, personajes) %>%
    filter(respuesta == "Sí votaría") %>%
    mutate(ganador = media == max(media), puntos = if_else(ganador, 2, 0)) %>%
    separate(aspecto, c("atributo","personaje"),remove = F) %>%
    select(atributo, personaje, media, ganador, puntos, aspecto)

  #preferencia
  pref <- analizar_frecuencias(encuesta$encuesta, candidato_preferencia) %>%
    filter(!respuesta %in% c("Ns/Nc", "Ninguno", "Otro")) %>%
    mutate(ganador = media == max(media),
           puntos = if_else(ganador, 2.75, 0)) %>%
    transmute(atributo = "preferencia", tema = respuesta, media, ganador, puntos)

  #juntar
  atr <- o_p %>%
    bind_rows(atr_p) %>%
    bind_rows(cand) %>%
    bind_rows(voto) %>%
    left_join(dicc %>% select(llaves, tema), by = c("aspecto" = "llaves")) %>%
    bind_rows(pref)

  return(atr)

}

analizar_frecuencia_multirespuesta <- function(diseno, patron_inicial){

  aux <- diseno$variables %>%
    tibble::rownames_to_column() %>%
    mutate(weight = weights(diseno)) %>%
    pivot_longer(-c(rowname, weight)) %>%
    filter(!is.na(value)) %>%
    mutate(seleccion = 1) %>%
    select(-name) %>%
    pivot_wider(names_from = value, values_from  = seleccion, values_fill = 0) %>%
    select(-rowname) %>% summarise(across(-weight, ~sum(.x*weight))) %>%
    pivot_longer(everything(), names_to = "respuesta", values_to = "value") %>%
    mutate(media = value/sum(weights(diseno)),
           respuesta=forcats::fct_reorder(.f = respuesta,
                                          .x = media,
                                          .fun = max))
  return(aux)

}

analizar_cruce_puntos <-  function(encuesta_diseño, cruce, variables, vartype, valor_variables){

  variables <- enquos(variables)

  res <- encuesta_diseño |>
    group_by(!!rlang::sym(cruce)) |>
    summarise(across(!!!variables,
                     ~ srvyr::survey_mean(.x == !!valor_variables, vartype = vartype, na.rm = TRUE),
                     .names = "{.col}")) |>
    tidyr::drop_na() |>
    tidyr::pivot_longer(cols = -rlang::sym(cruce),
                        names_to = "variable", values_to = "valor") |>
    mutate(separar = ifelse(stringr::str_detect(variable, glue::glue('_{vartype}$')), vartype, "mean"),
           variable = stringr::str_remove(variable, glue::glue('_{vartype}'))) |>
    tidyr::pivot_wider(names_from = separar, values_from = valor)

  return(res)
}

analizar_cruce_2vbrechas <-  function(encuesta_diseño, var1, var2_filtro, filtro, vartype){

  encuesta_diseño %>%
    group_by(!!rlang::sym(var1), !!rlang::sym(var2_filtro)) |>
    summarise(srvyr::survey_mean(na.rm=T, vartype = vartype)) %>%
    {
      if(is.null(filtro)){
        .
      } else{
        filter(., !!rlang::sym(var2_filtro) %in% filtro)
      }
    } %>% {
      if(vartype == "cv"){
        mutate(., pres=case_when(`_cv` >.15 & `_cv` <.30 ~ "*",
                                 `_cv` >.30 ~ "**",
                                 TRUE ~""))
      } else{
        .
      }
    }
}
