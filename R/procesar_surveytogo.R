#' Analizar frecuencias de una variable
#'
#' Calcula la media de una variable en un diseno muestral construido con la paqueteria `survey`
#' @param diseno Diseno muestral que contiene los pesos por individuo y las variables relacionadas
#' @param pregunta Nombre de la variable a calcular la estimación de proporciones de valores en la base de datos
#' @return Tibble con la media de las estimaciones por valor unico de la variable seleccionada
#' @examples
#' encuestar:::analizar_frecuencias(diseno = encuesta_demo$muestra$diseno, pregunta = "sexo")
#' encuestar:::analizar_frecuencias(diseno = encuesta_demo$muestra$diseno, pregunta = "voto_pr_24")
analizar_frecuencias <- function(diseno, pregunta){
  surveySummary_mean <- survey::svymean(survey::make.formula(pregunta),
                                        design = diseno,
                                        na.rm = TRUE)

  estimacion <-
    surveySummary_mean |>
    tibble::as_tibble(rownames = "respuesta") %>%
    rename(media = 2, ee = 3) %>%
    mutate(respuesta = stringr::str_replace(pattern = rlang::expr_text(ensym(pregunta)),
                                            replacement = "",
                                            string = respuesta),
           pregunta = rlang::expr_text(ensym(pregunta)),
           respuesta = stringr::str_replace_all(respuesta, " \\(No leer\\)", "")) |>
    left_join(surveySummary_mean |>
                stats::confint() %>%
                tibble::as_tibble(rownames = "respuesta") |>
                mutate(respuesta = stringr::str_replace(pattern = rlang::expr_text(ensym(pregunta)),
                                                        replacement = "",
                                                        string = respuesta),
                       pregunta = rlang::expr_text(ensym(pregunta))) |>
                rename(inf = 2, sup = 3), by = c("respuesta", "pregunta")) |>
    mutate(respuesta = forcats::fct_reorder(.f = respuesta,
                                            .x = media,
                                            .fun = max))
  return(estimacion)
}
#' Analizar frecuencias de multiples variables
#'
#' Calcula la media de multiples variables cuyos nombres comparten un patron inicial en comun
#'  de un diseno muestral construido con la paqueteria `survey`
#' @param diseno Diseno muestral que contiene los pesos por individuo y las variables relacionadas
#' @param diccionario Cuestionario de la encuesta en formato de procesamiento requerido
#' @param patron_pregunta Cadena de texto en comun (inicial) entre los nombres de las variables las variables a analizar.
#' @param aspectos Vector de tipo cadena de texto (final) que diferencia las variables a analizar.
#'  Separada de patron_pregunta por un guion bajo.
#' @return Base de datos con las estimaciones de frecuencia para cada categoria respondida por cada aspecto distinto.
#' @examples
#' encuestar:::analizar_frecuencias_aspectos(diseno = encuesta_demo$muestra$diseno, diccionario = encuesta_demo$cuestionario$diccionario, patron_pregunta = "conoce_pm", aspectos = c("astiazaran", "delrio"))
#' encuestar:::analizar_frecuencias_aspectos(diseno = encuesta_demo$muestra$diseno, diccionario = encuesta_demo$cuestionario$diccionario, patron_pregunta = "conoce_pm", aspectos = c("lia", "javier"))
analizar_frecuencias_aspectos <- function(diseno, diccionario, patron_pregunta, aspectos){
  estimaciones <-
    paste(patron_pregunta, aspectos, sep = "_") %>%
    purrr::map_df(.x = .,
                  .f = ~ analizar_frecuencias(diseno = diseno,
                                              pregunta = .)) |>
    rename(aspecto = pregunta) |>
    relocate(aspecto, .after = sup) |>
    left_join(diccionario |>
                select(aspecto = llaves,
                       pregunta), by = "aspecto") |>
    mutate(respuesta = forcats::fct_reorder(.f = respuesta,
                                            .x = media,
                                            .fun = max))
  return(estimaciones)
}
#' Calcula las estimaciones de asociacion partidista de un personaje en particular
#'
#' @param diseno Diseno muestral que contiene los pesos por individuo y las variables relacionadas.
#' @param diccionario Cuestionario de la encuesta en formato de procesamiento requerido
#' @param llave_partido Patrón que comparten las variables asociadas a las preguntas que relacionan un personaje con un partido político
#' @param llave_conocimiento Patrón que comparten las variables relacionadas a preguntas sobre el conocimiento de un personaje
#' @param respuesta_conoce Filtro sobre el cuál se evalúa la asociación de un personaje a un partído político
#' @param candidatos Vector de nombres cortos asociados a uno o más personajes sobre los cuáles se preguntó su asociación partidista
#' @param corte_otro Parámetro 'prop' de la función 'fct_lump' de la paquetería 'forcats' usado para agrupar valores pequeños de partidos políticos
#' @return Lista de donde cada elemento es un [tibble()] asociado al conocimiento y asociacion partidista
#' @examples
#' encuestar:::analizar_candidatoPartido(diseno = encuesta_demo$muestra$diseno, diccionario = encuesta_demo$cuestionario$diccionario, llave_partido = "partido_pm", llave_conocimiento = "conoce_pm", respuesta_conoce = "Sí", candidatos = c("lia", "javier"))
analizar_candidatoPartido <- function(diseno, diccionario, llave_partido, llave_conocimiento, respuesta_conoce, candidatos, corte_otro = 0.05){

  conoce <- paste(llave_conocimiento, candidatos, sep = "_")
  llaves_partido <- paste(llave_partido, candidatos,sep = "_")

  conoce <- analizar_frecuencias_aspectos(diseno = diseno, diccionario = diccionario, patron_pregunta = llave_conocimiento, aspectos = candidatos) |>
    filter(respuesta == respuesta_conoce)

  partido <- analizar_frecuencias_aspectos(diseno = diseno, diccionario = diccionario, patron_pregunta = llave_partido, aspectos = candidatos) |>
    mutate(respuesta = as.character(respuesta),
           respuesta = forcats::fct_lump(respuesta, w = media, prop = corte_otro, other_level = "Otro")) |>
    count(respuesta, aspecto, wt = media, name = "media") %>%
    group_by(aspecto) |>
    mutate(respuesta = forcats::fct_reorder(.f = respuesta, .x = media, .fun = max)) |>
    ungroup() |>
    arrange(aspecto, desc(media)) |>
    group_by(aspecto) %>%
    mutate(sup = cumsum(media),
           inf = lag(sup, default = 0),
           label = (inf +sup)/2)

  bases <- list(conoce = conoce, partido = partido)

  return(list(conoce = conoce, partido = partido))
}
#' Calcular el saldo de opinión por personaje
#'
#' @param diseno Diseno muestral que contiene los pesos por individuo y las variables relacionadas.
#' @param diccionario Cuestionario de la encuesta en formato de procesamiento requerido
#' @param candidatos Cadena de texto que diferencia las variables a analizar. Separada de patron_pregunta por un guion bajo.
#' @param llave_opinion Patrón que comparten las variables asociadas a las preguntas sobre la opinión pública hacia un personaje.
#' @param grupo_positivo Conjunto de valores de la variable opinión tratados como positivos.
#' @param grupo_negativo Conjunto de valores de la variable opinión tratados como negativos
#' @examples
#' encuestar:::analizar_saldoOpinion(diseno = encuesta_demo$muestra$diseno, diccionario = encuesta_demo$cuestionario$diccionario, llave_opinion = "opinion_pm", candidatos = c("astiazaran", "delrio"), grupo_positivo = c("Muy buena", "Buena"), grupo_negativo = c("Muy mala", "Mala"))
analizar_saldoOpinion <- function(diseno, diccionario, llave_opinion, candidatos, grupo_positivo, grupo_negativo){

  llave_op = paste(llave_opinion) # BUG

  bd <- analizar_frecuencias_aspectos(diseno = diseno, diccionario = diccionario, patron_pregunta = llave_op, aspectos = candidatos) |>
    left_join(diccionario |> select(aspecto = llaves, tema), by = "aspecto")

  res <- bd %>%
    mutate(!!rlang::sym(llave_op) := case_when(respuesta %in% grupo_positivo ~"Positiva",
                                               respuesta %in% grupo_negativo ~"Negativa",)) %>%
    filter(!is.na(!!rlang::sym(llave_op))) %>%
    mutate(persona = stringr::str_replace(aspecto, glue::glue("{llave_op}_"), "")) %>%
    count(persona, tema, grupo = !!rlang::sym(llave_op), wt = media, name = "saldo") %>%
    mutate(saldo = if_else(grupo == "Negativa", -saldo, saldo))

  return(res)
}

analizar_frecuencia_region <- function(variable, diseno, diccionario){
  survey::svytable(survey::make.formula(c(variable,"region")), design = diseno) %>%
    as_tibble() %>% group_by(region) %>% mutate(pct = n/sum(n)) %>% mutate(llaves = variable) %>%
    left_join(
      diccionario %>% select(llaves, pregunta)
    )
}

#' Analizar conocimiento de personajes por region
#'
#' @param diseno Diseno muestral que contiene los pesos por individuo y las variables relacionadas.
#' @param diccionario Cuestionario de la encuesta en formato de procesamiento requerido
#' @param patron_llaveConocimiento Patrón que comparten las variables asociadas al conocimiento de un personaje
#' @param filtro_respuestaConocimiento Filtro aplicado a las variables asociadas al conocimiento
#' @param aspectos_llaveConocimiento Cadena de texto que diferencia las variables a analizar. Separada de patron_pregunta por un guion bajo.
#'
#' @return
#' @export
#'
#' @examples
#' bd_analizar_conocimientoRegion <- analizar_conocimientoRegion(patron_llaveConocimiento = "conocimiento", aspectos_llaveConocimiento = candidatos, filtro_respuestaConocimiento = "Sí", diseno = Preguntas$Regiones$diseno, diccionario = cuestionario_demo)
analizar_conocimientoRegion <- function(patron_llaveConocimiento, aspectos_llaveConocimiento, filtro_respuestaConocimiento, diseno, diccionario){
  junto <- paste(patron_llaveConocimiento, aspectos_llaveConocimiento, sep = "_")
  tbl <- junto %>%
    purrr::map_df(~{survey::svytable(survey::make.formula(c(.x, "region")), design = diseno) %>%
        as_tibble() %>%
        group_by(region) %>%
        mutate(pct = n/sum(n)) %>%
        mutate(aspecto = .x) %>%
        filter(!!rlang::sym(.x) == filtro_respuestaConocimiento) %>%
        select(-1)}) %>%
    left_join(diccionario %>% select(aspecto = llaves, tema), by = "aspecto")
  return(tbl)
}
#' Analizar saldo de opinión de personajes en cada región o estrato
#'
#' @param diseno Diseno muestral que contiene los pesos por individuo y las variables relacionadas.
#' @param ns_nc Cadena de texto que identifica los valores relacionados al "No sabe" o "No contesta"
#' @param cat_negativo Vector que contiene los valores relacionados a las opiniones 'negativas'
#' @param cat_positivo Vector que contiene los valores relacionados a las opiniones 'positivas'
#' @param patron_llaveConocimiento Patrón que comparten las variables asociadas a la opinión de un personaje
#' @param aspectos_llaveOpinion Cadena de texto que diferencia las variables a analizar. Separada de patron_pregunta por un guion bajo.
#' @param cat_regular Vector que contiene los valores relacionados a las opiniones 'regulares' o neutras
#' @param diccionario Cuestionario de la encuesta en formato de procesamiento requerido
#'
#' @return
#' @export
#'
#' @examples
#' analizar_saldoRegion(patron_llaveOpinion = "opinion", aspectos_llaveOpinion = candidatos, ns_nc = "Ns/Nc", cat_negativo = c("Muy mala", "Mala"), cat_regular = "Regular", cat_positivo = c("Buena", "Muy buena"), diseno = Preguntas$Regiones$diseno, diccionario = Preguntas$Regiones$diccionario)
analizar_saldoRegion <- function(patron_llaveOpinion, aspectos_llaveOpinion, ns_nc, cat_negativo, cat_regular, cat_positivo, diseno, diccionario){
  if(patron_llaveOpinion ==  "") llaves <- candidatos else llaves <- paste(patron_llaveOpinion, candidatos,sep = "_")

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
        diccionario %>% select(aspecto = llaves, tema), by = "aspecto"
      )
  } else{
    res <- res %>% mutate(tema = names(candidatos[match(aspecto, candidatos)]))
  }
  return(res)
}
#' Calcular valor más frecuente por región de acuerdo al diseño muestral
#'
#' @param diseno Diseno muestral que contiene los pesos por individuo y las variables relacionadas.
#' @param regiones Shape file de las regiones-estrato de la encuesta
#' @param lugar Primer lugar del promedio por región de la variable "variable" usando la función dense_rank
#' @param variable Variable a obtener la estimación estimación por región
#' @param lugar Lugar del top de las estimaciones por región
#'
#' @return
#' @export
#'
#' @examples
#' calcular_ganadorRegion(regiones = shp, variable = "voto_partido", lugar = lugar, diseno = diseno)
calcular_ganadorRegion <- function(diseno, regiones, variable, lugar){
  bd_topEstimaciones <- encuestar:::analizar_frecuenciasRegion(regiones = regiones, variable = variable, diseno = diseno) |>
    janitor::clean_names() |>
    as_tibble() |>
    select(!c(geometry, n)) |>
    select(!contains("se")) |>
    tidyr::pivot_longer(cols = !region, names_to = rlang::expr_text(ensym(variable)), values_to = "estimacion") |>
    group_by(region) %>%
    filter(dense_rank(-estimacion) == lugar)
  tbl <- regiones %>%
    left_join(bd_topEstimaciones, by = "region")
  return(tbl)

}
#' Analizar estimaciones frecuentistas por región
#'
#' @param diseno Diseno muestral que contiene los pesos por individuo y las variables relacionadas.
#' @param regiones Shape file de las regiones-estrato de la encuesta
#' @param variable Variable a obtener la estimación estimación por región
#'
#' @return
#' @export
#'
#' @examples
#' analizar_frecuenciasRegion(regiones = shp, variable = "voto_partido", diseno = diseno)
analizar_frecuenciasRegion <- function(regiones, variable, diseno){
  variable = variable
  formula <- survey::make.formula(rlang::expr_text(ensym(variable)))
  tbl <- regiones %>%
    left_join(
      survey::svyby(formula, ~region, design = diseno, FUN = survey::svymean, na.rm  = T) %>%
        as_tibble(), by = "region")
  return(tbl)
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

#' Analizar metodología de MORENA
#'
#' @param diseno Diseno muestral que contiene los pesos por individuo y las variables relacionadas.
#' @param diccionario Cuestionario de la encuesta en formato de procesamiento requerido
#' @param personajes Vector de nombres cortos asociados a uno o más personajes sobre los cuáles se preguntó la batería de MORENA
#' @param atributos Tibble que contiene los atributos y los puntos que obtiene el ganador de cada atributo
#'
#' @return
#' @export
#'
#' @examples
#' analizar_morena(diseno = diseno, diccionario = diccionario, personajes = c("era", "sasil"), atributos = atributos)
#' analizar_morena(diseno = diseno, diccionario = diccionario, personajes = c("era", "sasil", "jaac"), atributos = atributos)
analizar_morena <- function(diseno, diccionario, personajes, atributos){

  #opinión positiva
  o_p <- encuestar:::analizar_frecuencias_aspectos(diseno = diseno, diccionario = diccionario, patron_pregunta = "opinion", aspectos = personajes) %>%
    filter(respuesta == "Buena") %>%
    mutate(ganador = media == max(media), puntos = if_else(ganador, 2, 0)) %>%
    separate(aspecto, c("atributo", "personaje"), remove = F) %>%
    select(atributo, personaje, media, ganador, puntos, aspecto)

  #atributos
  atr_p <- atributos %>%
    purrr::pmap_df(function(atributo, puntos){
      encuestar:::analizar_frecuencias_aspectos(diseno = diseno, diccionario = diccionario, patron_pregunta = atributo, aspectos = personajes) %>%
        filter(respuesta %in% c("Mucho", "Algo")) %>%
        mutate(media = if_else(respuesta == "Algo", media *.5,media)) %>%
        group_by(aspecto) %>%
        summarise(media = sum(media)) %>%
        mutate(ganador = media == max(media),
               puntos = if_else(ganador, puntos, 0)) %>%
        separate(aspecto, c("atributo", "personaje"), remove = F)
    })

  #buen candidato
  cand <- encuestar:::analizar_frecuencias_aspectos(diseno = diseno, diccionario = diccionario, patron_pregunta = "buencandidato", aspectos = personajes) %>%
    filter(respuesta == "Sí") %>%
    mutate(ganador = media == max(media), puntos = if_else(ganador, 1, 0)) %>%
    separate(aspecto, c("atributo","personaje"),remove = F) %>%
    select(atributo, personaje, media, ganador, puntos, aspecto)

  #votaria
  voto <- encuestar:::analizar_frecuencias_aspectos(diseno = diseno, diccionario = diccionario, patron_pregunta = "votaria", aspectos = personajes) %>%
    filter(respuesta == "Sí votaría") %>%
    mutate(ganador = media == max(media), puntos = if_else(ganador, 2, 0)) %>%
    separate(aspecto, c("atributo","personaje"),remove = F) %>%
    select(atributo, personaje, media, ganador, puntos, aspecto)

  #preferencia
  pref <- encuestar:::analizar_frecuencias(diseno = diseno, pregunta = "candidato_preferencia") %>%
    filter(!respuesta %in% c("Ns/Nc", "Ninguno", "Otro")) %>%
    mutate(ganador = media == max(media),
           puntos = if_else(ganador, 2.75, 0)) %>%
    transmute(atributo = "preferencia", tema = respuesta, media, ganador, puntos)

  #juntar
  atr <- o_p %>%
    bind_rows(atr_p) %>%
    bind_rows(cand) %>%
    bind_rows(voto) %>%
    left_join(diccionario %>% select(llaves, tema), by = c("aspecto" = "llaves")) %>%
    bind_rows(pref)

  return(atr)

}

#' Title
#'
#' @param diseno
#' @param patron_inicial
#'
#' @return
#' @export
#'
#' @examples
analizar_frecuencia_multirespuesta <- function(diseno, patron_inicial){

  aux <- diseno$variables %>%
    tibble::rownames_to_column() %>%
    as_tibble() |>
    mutate(weight = weights(diseno)) %>%
    select(rowname, contains(patron_inicial), weight) |>
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
#' Analizar cruce por puntos
#'
#' @param diseno Diseno muestral que contiene los pesos por individuo y las variables relacionadas.
#' @param variable_principal
#' @param variables_secundarias
#' @param filtro_variables_secundarias
#' @param vartype
#'
#' @return
#' @export
#'
#' @examples
analizar_cruce_aspectos = function(diseno, variable_principal, variables_secundarias, filtro_variables_secundarias, vartype){
  variables_secundarias <- rlang::enquo(variables_secundarias)
  res <-
    srvyr::as_survey_design(diseno) |>
    group_by(!!rlang::sym(variable_principal)) %>%
    summarise(across(!!variables_secundarias, ~ srvyr::survey_mean(.x == !!filtro_variables_secundarias, vartype = vartype, na.rm = TRUE), .names = "{.col}")) |>
    tidyr::drop_na() |>
    tidyr::pivot_longer(cols = -rlang::sym(variable_principal),
                        names_to = "variable",
                        values_to = "valor") |>
    mutate(separar = ifelse(stringr::str_detect(variable, glue::glue('_{vartype}$')), vartype, "mean"),
           variable = stringr::str_remove(variable, glue::glue('_{vartype}'))) |>
    tidyr::pivot_wider(names_from = separar, values_from = valor)
  return(res)
}
#' Analizar cruce entre dos variables
#'
#' @param diseno Diseno muestral que contiene los pesos por individuo y las variables relacionadas.
#' @param vartype
#' @param variable_principal
#' @param variable_secundaria
#'
#' @return
#' @export
#'
#' @examples
analizar_cruce = function(diseno, variable_principal, variable_secundaria, vartype){
  res <-
    srvyr::as_survey_design(diseno) %>%
    group_by(!!rlang::sym(variable_principal), !!rlang::sym(variable_secundaria)) |>
    summarise(srvyr::survey_mean(na.rm = TRUE, vartype = vartype))
  if(vartype == "cv") {
    res <-
      res |>
      mutate(pres = case_when(`_cv` > .15 & `_cv` < .30 ~ "*",
                              `_cv` > .30 ~ "**",
                              TRUE ~ ""))
  }
  res <-
    res |>
    ungroup()
  return(res)
}
#' Analizar sankey
#'
#' @param variables Vector que contiene las llaves de las cuales se va a hacer el cruce
#' @param diseno Diseno muestral que contiene los pesos por individuo y las variables relacionadas.
#'
#' @return
#' @export
#'
#' @examples
analizar_sankey = function(diseno, variables, filtro_var1, filtro_var2){
  if(length(variables) == 2) {
    vec_variables <- c(var1 = variables[[1]], var2 = variables[[2]])
  }

  if(length(variables) == 3) {
    vec_variables <- c(var1 = variables[[1]], var2 = variables[[2]], var3 = variables[[3]])
  }
  res <-
    survey::svytable(survey::make.formula(vec_variables),
                     design = diseno) %>%
    tibble::as_tibble()

  if(!is.null(filtro_var1)) {

    res <-
      res |>
      filter(!(!!rlang::sym(variables[[1]]) %in% filtro_var1))

  }

  if(!is.null(filtro_var2)) {

    res <-
      res |>
      filter(!(!!rlang::sym(variables[[2]]) %in% filtro_var2))

  }

  res |>
    ggsankey::make_long(-n, value = n)
}

#' Title
#'
#' @param var1
#' @param var2
#' @param legenda1
#' @param legenda2
#' @param diseno
#' @param colores
#'
#' @return
#' @export
#'
#' @examples
analisis_correspondencia <- function(var1, var2, legenda1=NULL, legenda2=NULL, diseno, colores =NULL){

  if(is.null(legenda1)) legenda1 <- var1
  if(is.null(legenda2)) legenda2 <- var2
  if(is.null(colores)) colores <- c("#DE6400","#023047")

  formula <- survey::make.formula(c(var1,var2))
  aux <- survey::svytable(formula, design = diseno) %>%
    tibble::as_tibble() %>%
    tidyr::pivot_wider(names_from = var2, values_from = "n") %>%
    tibble::column_to_rownames(var = var1)


  # chisq.test(aux)
  res.ca <- FactoMineR::CA(aux, graph = F)
  eig <- factoextra::get_eigenvalue(res.ca)[, 2]

  res.ca$col$coord %>%
    as_tibble(rownames = "respuesta") %>%
    janitor::clean_names() %>%
    select(respuesta,num_range("dim_",1:2)) %>%
    mutate(variable = legenda2) %>%
    bind_rows(res.ca$row$coord %>%
                as_tibble(rownames = "respuesta") %>%
                janitor::clean_names() %>%
                select(respuesta,num_range("dim_",1:2)) %>%
                mutate(variable = legenda1)) %>%
    ggpubr::ggscatter(x = "dim_1", y = "dim_2", color = "variable", label = "respuesta", repel = T) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = scales::percent(eig[1]/100,accuracy = .1),
         y = scales::percent(eig[2]/100,accuracy = .1),
         color = ""
    ) +
    scale_color_manual(values = colores) +
    lemon::scale_x_symmetric() +
    lemon::scale_y_symmetric() +
    theme(axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major.x = element_line(colour = "#C5C5C5",linetype = "dotted"),
          panel.grid.major.y = element_line(colour = "#C5C5C5",linetype = "dotted"))

}
#' Calcular media movil de una variable
#'
#' @param bd_resultados
#' @param variable
#' @param sin_peso
#' @param valores_interes
#'
#' @return
#' @export
#'
#' @examples
calcular_mediaMovil = function(bd_resultados, variable, sin_peso, valores_interes) {
  bd_resultados %>%
    {
      if(sin_peso) {
        count(x = ., hora = lubridate::floor_date(Date, "hour"), !!rlang::sym(variable))
      } else {
        count(x = ., hora = lubridate::floor_date(Date, "hour"), !!rlang::sym(variable), wt = peso)
      }
    }  %>%
    group_by(hora) |>
    complete(!!rlang::sym(variable) := valores_interes,
             fill = list(n = 0)) |>
    ungroup() |>
    mutate(tot = sum(n), .by = c(hora)) |>
    mutate(n_acum = cumsum(n),
           tot_acum = cumsum(tot), .by = c(!!rlang::sym(variable)),
           !!rlang::sym(paste0("movil_", variable)) := n_acum/tot_acum) |>
    filter(!!rlang::sym(variable) %in% valores_interes) |>
    select(hora, !!rlang::sym(variable), !!rlang::sym(paste0("movil_", variable)))
}

#' Calcular media movil de una variable por region
#'
#' @param bd_resultados
#' @param variable
#' @param sin_peso
#' @param valores_interes
#' @param variable_region
#'
#' @return
#' @export
#'
#' @examples
calcular_mediaMovil_region = function(bd_resultados, variable, sin_peso, valores_interes, variable_region) {
  bd_resultados %>%
    {
      if(sin_peso) {
        count(x = ., hora = lubridate::floor_date(Date, "hour"), !!rlang::sym(variable_region), !!rlang::sym(variable))
      } else {
        count(x = ., hora = lubridate::floor_date(Date, "hour"), !!rlang::sym(variable_region), !!rlang::sym(variable), wt = peso)
      }
    }  %>%
    group_by(hora, !!rlang::sym(variable_region)) |>
    complete(!!rlang::sym(variable) := valores_interes,
             fill = list(n = 0)) |>
    ungroup() |>
    mutate(tot = sum(n), .by = c(hora, !!rlang::sym(variable_region) )) |>
    mutate(n_acum = cumsum(n),
           tot_acum = cumsum(tot), .by = c(!!rlang::sym(variable_region), !!rlang::sym(variable)),
           !!rlang::sym(paste0("movil_", variable)) := n_acum/tot_acum) |>
    filter(!!rlang::sym(variable) %in% valores_interes) |>
    select(!!rlang::sym(variable_region), hora, !!rlang::sym(variable), !!rlang::sym(paste0("movil_", variable)))
}

#' Title
#'
#' @param diseno
#' @param diccionario
#' @param patron_opinion
#' @param patron_conocimiento
#' @param aspectos
#' @param filtro_conocimiento
#' @param orden_opinion
#' @param ns_nc
#'
#' @return
#' @export
#'
#' @examples
calcular_tabla_candidatoOpinion = function(diseno, diccionario, patron_opinion, patron_conocimiento, aspectos, filtro_conocimiento, orden_opinion, ns_nc, salto_respuestas) {

  bd_opinion <-
    encuestar:::analizar_frecuencias_aspectos(diseno = diseno,
                                              diccionario = diccionario,
                                              patron_pregunta = patron_opinion,
                                              aspectos = aspectos) |>
    left_join(diccionario %>% select(aspecto = llaves, tema)) |>
    tidyr::pivot_wider(id_cols = tema,
                       names_from = respuesta,
                       values_from = media) |>
    select(Candidato = tema, all_of(orden_opinion), ns_nc) |>
    rename_with(.fn = ~ stringr::str_wrap(string = .x, width = salto_respuestas), .cols = all_of(orden_opinion))

  if(!is.na(patron_conocimiento)) {
    bd_conocimiento <-
      encuestar::analizar_frecuencias_aspectos(diseno = diseno,
                                               diccionario = diccionario,
                                               patron_pregunta = patron_conocimiento,
                                               aspectos = aspectos) %>%
      filter(eval(rlang::parse_expr(filtro_conocimiento))) %>%
      left_join(diccionario %>% select(aspecto = llaves, tema)) |>
      select(tema, Conocimiento = media)
  }

  bd_opinion %>%
    {
      if(!is.na(patron_conocimiento)) {
        left_join(x = ., y = bd_conocimiento, by = c("Candidato" = "tema")) |>
          arrange(desc(Conocimiento))
      } else {
        arrange(., desc(.data[[names(.)[2]]]))
      }
    } %>%
    mutate(across(.cols = !Candidato, .fns = ~ scales::percent(.x, accuracy = 1.)))

}

#' Title
#'
#' @param diseno
#' @param diccionario
#' @param var1
#' @param var2
#'
#' @return
#' @export
#'
#' @examples
calcular_tabla_votoCruzado = function(diseno, var1, var2, filtro_var2){

  orden_var1 <-
    encuestar:::analizar_frecuencias(diseno = diseno,
                                     pregunta = var1) |>
    arrange(desc(media)) |>
    mutate(respuesta = as.character(respuesta)) |>
    select(!!rlang::sym(var1) := respuesta, media)

  orden_var2 <-
    encuestar:::analizar_frecuencias(diseno = diseno,
                                     pregunta = var2) |>
    arrange(desc(media)) %>%
    {
      if(!is.null(filtro_var2)) {
        filter(.data = ., respuesta %in% filtro_var2)
      } else {
        .
      }
    } %>%
    mutate(respuesta = as.character(respuesta)) |>
    pull(respuesta)

  aux <-
    encuestar:::analizar_cruce(diseno = diseno,
                               variable_principal = var1,
                               variable_secundaria = var2,
                               vartype = "cv") |>
    ungroup() |>
    select(var1, var2, coef) |>
    tidyr::pivot_wider(id_cols = var1,
                       names_from = var2,
                       values_from = coef) |>
    left_join(orden_var1) |>
    arrange(desc(media)) |>
    select(!media) |>
    mutate(across(.cols = !var1, .fns = ~ tidyr::replace_na(data = .x, replace = 0)),
           across(.cols = !var1, .fns = ~ scales::percent(x = .x, accuracy = 1.))) |>
    select(var1, all_of(orden_var2))
  return(aux)
}

