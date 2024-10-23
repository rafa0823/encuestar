#' Clase Muestra
#'
#' @description La clase `Muestra` recibe.
Muestra <-
  R6::R6Class(classname = "Muestra",
              public = list(
                muestra = NULL,
                base=NULL,
                diseno_original = NULL,
                region = NULL,
                calibracion = NULL,
                diseno=NULL,
                calibraciones = NULL,
                initialize =function(muestra, respuestas, nivel, var_n){
                  self$muestra <- muestra

                  self$base <- muestra$muestra %>% purrr::pluck(var_n)
                  self$recalcular_fpc(respuestas = respuestas, nivel, var_n)
                },
                recalcular_fpc = function(respuestas, nivel, var_n){

                  var_pob <- self$muestra$variable_poblacional

                  pob <- self$base %>%
                    tidyr::unnest(data) %>%
                    count(!!rlang::sym(nivel), wt= !!rlang::sym(var_pob), name="poblacion") %>%
                    mutate(!!rlang::sym(var_n) := as.character(!!rlang::sym(nivel))) %>%
                    select(-!!rlang::sym(nivel))

                  respuesta_fpc <- respuestas %>%
                    count(!!rlang::sym(var_n)) %>%
                    left_join(pob, by= var_n) %>%
                    mutate(fpc_0=n/poblacion) %>%
                    select(!!rlang::sym(var_n), fpc_0)

                  muestra <- self$base %>%
                    mutate(data = map(data,~.x %>% distinct(across(contains("fpc"))))) %>%
                    tidyr::unnest(data) %>%
                    select(-fpc_0) %>%
                    mutate(!!rlang::sym(var_n) := as.character(!!rlang::sym(nivel))) %>%
                    inner_join(respuesta_fpc, by= var_n)

                  self$base <- muestra

                },
                extraer_diseno = function(respuestas, marco_muestral, tipo_encuesta, sin_peso, rake){
                  if(sin_peso){
                    self$diseno <- survey::svydesign(
                      ids=~1,
                      data = respuestas
                    )
                  } else{
                    r <- try(
                      survey::svydesign(
                        pps="brewer",
                        ids=crear_formula_nombre(respuestas, "cluster_"),
                        fpc = crear_formula_nombre(respuestas, "fpc_"),
                        strata = crear_formula_nombre(respuestas, "strata_"),
                        data = respuestas
                      )
                      ,T)

                    diseno <- if("try-error" %in% class(r)){
                      message("Se intenta muestreo estratificado por estrato. Faltan unidades a muestrear.")
                      out <- survey::svydesign(
                        pps="brewer",
                        ids = ~1,
                        strata = crear_formula_nombre(respuestas, "strata_"),
                        data = respuestas
                      )
                      out
                    } else{
                      r
                    }

                    if(rake){
                      if(tipo_encuesta == "inegi"){
                        pob <- marco_muestral %>%
                          transmute(
                            P_18A24_F,
                            P_18A24_M,
                            P_25A59_F = P_18YMAS_F - P_18A24_F - P_60YMAS_F,
                            P_25A59_M = P_18YMAS_M - P_18A24_M - P_60YMAS_M,
                            P_60YMAS_F, P_60YMAS_M) %>%
                          summarise(across(everything(), ~sum(.x,na.rm = T))) %>%
                          pivot_longer(everything()) %>% mutate(name = gsub("P_","",name)) %>%
                          separate(name, into = c("rango_edad", "sexo"))
                      }

                      if(tipo_encuesta == "ine"){
                        pob <- marco_muestral %>%
                          select(contains("LN22_")) %>%
                          summarise(across(everything(), ~sum(.x,na.rm = T))) %>%
                          pivot_longer(everything()) %>% mutate(name = gsub("LN22_","",name)) %>%
                          separate(name, into = c("rango_edad", "sexo"))
                      }


                      pobG <- pob %>% count(rango_edad, wt = value, name = "Freq")
                      pobS<- pob %>% count(sexo, wt = value, name = "Freq")
                      self$diseno <- survey::rake(diseno, list(~rango_edad, ~sexo), list(pobG, pobS))
                    } else{
                      self$diseno <- diseno
                    }
                  }

                },
                revisar_sexo = function(){
                  self$diseno$variables %>% count(sexo) %>%
                    mutate(pct =n/sum(n), tipo = "encesta") %>% bind_rows(
                      self$muestra$poblacion$marco_muestral %>%
                        select(contains("LN22_")) %>%
                        summarise(across(everything(), ~sum(.x,na.rm = T))) %>%
                        pivot_longer(everything()) %>% mutate(name = gsub("LN22_","",name)) %>%
                        separate(name, into = c("rango_edad", "sexo")) %>%
                        count(sexo, wt = value) %>%
                        mutate(pct = n/sum(n), tipo = "real")
                    ) %>% ggplot(aes(x = tipo, y = pct, color = sexo)) + geom_point() +
                    ggrepel::geom_text_repel(aes(label = paste0(scales::percent(pct,.01))),force_pull = 5) +
                    geom_line(aes(group = sexo)) +
                    scale_y_continuous(labels = scales::percent) +
                    labs(y = NULL,  x = NULL) +
                    theme_minimal()
                },
                revisar_rango_edad = function(){
                  self$diseno$variables %>% count(rango_edad) %>%
                    mutate(pct = n/sum(n), tipo = "encuesta") %>% bind_rows(
                      self$muestra$poblacion$marco_muestral %>%
                        select(contains("LN22_")) %>%
                        summarise(across(everything(), ~sum(.x,na.rm = T))) %>%
                        pivot_longer(everything()) %>% mutate(name = gsub("LN22_","",name)) %>%
                        separate(name, into = c("rango_edad", "sexo")) %>%
                        count(rango_edad, wt = value )%>%
                        mutate(pct = n/sum(n), tipo = "real")
                    ) %>% ggplot(aes(x = tipo, y = pct, color = rango_edad)) + geom_point() +
                    ggrepel::geom_text_repel(aes(label = paste0(scales::percent(pct,.01))),force_pull = 5) +
                    geom_line(aes(group = rango_edad)) +
                    scale_y_continuous(labels = scales::percent) +
                    labs(y = NULL,  x = NULL) +
                    theme_minimal()
                },
                diseno_region = function(seleccion){
                  self$region <- seleccion

                  if(is.null(self$diseno_original)){
                    self$diseno_original <- self$diseno
                  }

                  self$diseno <- subset(self$diseno_original, region %in% self$region)
                },
                regresar_diseno_original = function(){
                  self$region <- NULL
                  self$calibracion <- NULL
                  self$diseno_original -> self$diseno
                },
                agregar_calibracion = function(vars, poblacion, nombre){
                  calibracion <- survey::calibrate(self$diseno,
                                                   survey::make.formula(vars),
                                                   population = poblacion)

                  self$calibraciones <- self$calibraciones |>
                    append(
                      list(
                        list(
                          diseno = calibracion,
                          variables = vars
                        )
                      ) |>
                        purrr::set_names(nombre))
                },
                revisar_calibracion = function(nombre){
                  aux <- self$calibraciones |> purrr::pluck(nombre)
                  survey::svytotal(survey::make.formula(aux |> pluck("variables")),
                                   design = aux |> pluck("diseno"))
                },
                comparar_calibraciones = function(variables, valor_variables, vartype){
                  list(original = self$diseno) |>
                    append(self$calibraciones |> purrr::map(~.x$diseno)) |>
                    comparar_disenos(variables, valor_variables, vartype)
                },
                elegir_calibracion = function(nombre){

                  self$calibracion <- nombre

                  if(is.null(self$diseno_original)){
                    self$diseno_original <- self$diseno
                  }

                  self$diseno <- self$calibraciones |> purrr:::pluck(nombre, "diseno")
                }
              ))
