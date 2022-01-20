#' Esta es la clase Encuesta
#' @description La clase Encuesta contiene todos los métodos, y atributos que puede tener una encuesta.
#' @field respuestas Base de datos de respuestas
#' @field muestra Base de datos de muestra.
#' @field dicionario Base de datos de diccionario
#' @export
#' @import dplyr ggplot2 tidyr sf purrr stringr
Encuesta <- R6::R6Class("Encuesta",
                        public = list(
                          respuestas = NULL,
                          cuestionario=NULL,
                          muestra = NULL,
                          auditoria_telefonica=NA,
                          diseño = NULL,
                          shp = NULL,
                          mantener = NULL,
                          #' @description
                          #' Create a person
                          #' @param respuestas Name of the person
                          #' @param diccionario Hair colour
                          initialize = function(respuestas = NA,
                                                muestra = NA,
                                                postestratificacion = NA,
                                                auditoria_telefonica = NA,
                                                cuestionario=NA,
                                                shp = NA,
                                                mantener = NA) {
                            self$respuestas <- Respuestas$new(base = respuestas)
                            # Valorar si no es mejor una active binding
                            self$muestra <- Muestra$new(base = muestra)
                            # Valorar active binding
                            self$cuestionario <- Cuestionario$new(documento = cuestionario)
                            # Valorar active bindign
                            self$auditoria_telefonica <- auditoria_telefonica

                            self$shp <- shp
                            self$mantener <- mantener
                            # Procesos ####
                            self$respuestas <- private$limpiar_respuestas()
                            # Cambiar variables a tipo numérica
                            numericas <- self$cuestionario$diccionario %>% filter(tipo_pregunta == "numericas") %>%
                              pull(llaves)

                            self$respuestas$base <- self$respuestas$base %>%
                              mutate(across(all_of(numericas), ~readr::parse_number(.x)))
                            # Recalcular fpc
                            self$muestra <- private$recalcular_fpc()

                            #Información muestral

                            self$respuestas$base <- self$respuestas$base %>%
                              inner_join(self$muestra$base, by=c("CLUSTER")) %>%
                              mutate(rango = as.character(cut(as.integer(PB),c(17,24,59,200),
                                                              c("18A24","25A59","60YMAS"))),
                                     sexo = if_else(P21 == "Mujer", "F", "M"))
                            # En las líneas de arriba cambiar las variables PB y P21

                            self$diseño <- self$muestra$extraer_diseño(self$respuestas$base, postestratificacion = postestratificacion)
                          },
                          exportar_entregable = function(carpeta = "Entregables", agregar = NULL, quitar = NULL){
                            if(!file.exists(carpeta)) dir.create(carpeta)
                            #Exportar bd
                            exportar_bd(self, carpeta, agregar, quitar)
                            # Exportar diccionario
                            self$cuestionario$diccionario %>% unnest(respuestas) %>%
                              readr::write_excel_csv(glue::glue("{carpeta}/diccionario.csv"))

                          }),
                        private=list(
                          limpiar_respuestas=function(){
                            # Limpiar las que no pasan auditoría telefónica
                            self$respuestas$eliminar_auditoria_telefonica(self$auditoria_telefonica)

                            # Limpiar las respuestas que no tienen coordenadas
                            self$respuestas$eliminar_falta_coordenadas()

                            # Eliminar entrevistas cuyo cluster no pertenece a la muestra
                            self$respuestas$eliminar_fuera_muestra(self$respuestas$base, self$muestra$base)

                            # Corregir cluster equivocado
                            self$respuestas$correccion_cluster(self$respuestas$base, self$shp, self$mantener)

                            # Limpiar las que no tienen variables de diseño
                            self$respuestas$eliminar_faltantes_diseño()
                            return(invisible(self$respuestas))
                          },
                          recalcular_fpc = function(){
                            self$muestra$recalcular_fpc(respuestas = self$respuestas$base)

                            return(invisible(self$muestra))
                          }

                        )
)

#' Esta es la clase Respuestas
#' @export
Respuestas <- R6::R6Class("Respuestas",
                          inherit = Encuesta,
                          public = list(
                            base = NULL,
                            n=NULL,
                            m=NULL,
                            #' @description
                            #' Crear respuesta
                            #' @param base Base de datos de respuestas.
                            initialize=function(base = NA) {
                              self$base <- base
                            },
                            eliminar_auditoria_telefonica=function(auditoria_telefonica){
                              if(("SbjNum" %in% names(self$base)) &
                                 ("SbjNum" %in% names(auditoria_telefonica))){
                                # Se eliminan por no pasar la auditoria telefónica
                                n <- nrow(self$base)
                                self$base <- self$base %>%
                                  anti_join(auditoria_telefonica, by="SbjNum")
                                # Mandar mensaje
                                print(
                                  glue::glue("Se eliminaron {n-nrow(self$base)} encuestas por auditoría telefónica")
                                )
                              }
                              else print("Identificador SbjNum no presente en alguna de las bases")
                              return(self$base)
                            },
                            eliminar_falta_coordenadas = function(){
                              if("Longitude" %in% names(self$base) & "Latitude" %in% names(self$base)){
                                n <- nrow(self$base)
                                self$base <- self$base %>% filter(!is.na(Longitude) | !is.na(Latitude))
                                print(
                                  glue::glue("Se eliminaron {n-nrow(self$base)} encuestas por falta de coordenadas")
                                )
                              } else{
                                print("No existe la columna Longitude ni Latitude en la base de respuestas")
                              }
                            },
                            correccion_cluster = function(base, shp, mantener){
                              self$base <- corregir_cluster(base, shp, mantener)
                            },
                            eliminar_fuera_muestra = function(respuestas, muestra){
                              self$base <- respuestas %>%
                                semi_join(muestra %>% mutate(cluster_3 = as.character(cluster_3)),
                                          by = c("CLUSTER" = "cluster_3"))
                              print(glue::glue("Se eliminaron {nrow(respuestas) - nrow(self$base)} entrevistas ya que el cluster no pertenece a la muestra"))
                            },
                            eliminar_faltantes_diseño=function(){
                              n <- nrow(self$base)
                              self$base <- self$base %>%
                                filter(
                                  across(
                                    .cols = c(starts_with("id_"),
                                              starts_with("fpc_"),
                                              starts_with("prob_"),
                                              starts_with("pob_")),
                                    .fns = ~ !is.na(.x)
                                  ))
                              print(
                                glue::glue("Se eliminaron {n-nrow(self$base)} encuestas por tener datos faltantes en las variables de diseño muestral")
                              )
                              return(self$base)
                            }
                          )
)


#'Esta es la clase de muestra
#'@export
Muestra <- R6::R6Class("Muestra",
                       public=list(
                         base=NULL,
                         diseño=NULL,
                         initialize =function(base){
                           self$base=base
                         },
                         recalcular_fpc = function(respuestas){
                           pob <- self$base %>%
                             tidyr::unnest(data) %>%
                             count(cluster_3, wt=POBTOT, name="poblacion") %>%
                             mutate(CLUSTER=as.character(cluster_3)) %>%
                             select(-cluster_3)

                           respuesta_fpc <- respuestas %>%
                             count(CLUSTER) %>%
                             left_join(pob, by=c("CLUSTER")) %>%
                             mutate(fpc_0=n/poblacion) %>%
                             select(CLUSTER, fpc_0)


                           muestra <- self$base %>%
                             mutate(data = map(data,~.x %>% distinct(across(contains("fpc"))))) %>%
                             tidyr::unnest(data) %>%
                             select(-fpc_0) %>%
                             mutate(CLUSTER=as.character(cluster_3)) %>%
                             inner_join(respuesta_fpc, by="CLUSTER")

                           self$base <- muestra
                           return(self)
                         },
                         extraer_diseño=function(respuestas, postestratificacion){
                           warning(glue::glue("Se está haciendo el join entre respuestas y muestra manualmente. Corregir.
                                                Además, se elimina el cluster_0 de respuestas para que corra, esto es temporal, se debe recalcular el fpc"))

                           diseño<- survey::svydesign(
                             pps="brewer",
                             ids=crear_formula_nombre(respuestas, "cluster_"),
                             fpc = crear_formula_nombre(respuestas, "fpc_"),
                             strata = crear_formula_nombre(respuestas, "strata_"),
                             data = respuestas
                           )
                           pobG <- postestratificacion %>% count(rango, wt = value, name = "Freq")
                           pobS<- postestratificacion %>% count(sexo, wt = value, name = "Freq")
                           diseño <- survey::rake(diseño, list(~rango, ~sexo), list(pobG, pobS))

                           return(diseño)
                         }
                       ))

#'Esta es la clase cuestionario
#'@export
#'
Cuestionario <- R6::R6Class("Cuestionario",
                            public=list(
                              documento=NULL,
                              aprobado=NULL,
                              diccionario=NULL,
                              initialize=function(documento){
                                self$documento <- documento
                                self$diccionario <- private$crear_diccionario()

                              },
                              aprobar=function(){
                                self$aprobado <- T
                                return(invisible(self))
                              },
                              checar_pregunta=function(pregunta){
                                pregunta_chr <- rlang::expr_text(ensym(pregunta))
                                bd <- self$diccionario %>%
                                  filter(llaves==pregunta_chr)
                                pertenece <- (nrow(bd)==1)
                                if(pertenece){
                                  return(map(bd,~.x))
                                }
                                else return(NULL)
                              },
                              resumen = function(){

                                res <- resumen_cuestionario(self$diccionario)


                                return(res)

                              }
                              )
                            ,
                            private=list(
                              crear_diccionario=function(){
                                diccionario <-
                                  officer::docx_summary(self$documento) %>%
                                    as_tibble() %>%
                                    filter(!is.na(style_name),style_name %in% c("Morant_Bloque","Morant_Pregunta",
                                                                                "Morant_respuestas_aspectos",
                                                                                "Morant_respuestas_abiertas",
                                                                                "Morant_respuestas_abiertasMultiples",
                                                                                "Morant_respuestas_numericas",
                                                                                "Morant_respuestas_numericasMultiples",
                                                                                "Morant_respuestas_multiples")) %>%
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

                                  self$diccionario <- diccionario %>%
                                    anti_join(diccionario %>% filter(tipo_pregunta == "aspectos"), by = "pregunta") %>%
                                    select(bloque, pregunta, tipo_pregunta, llaves, text) %>%
                                    bind_rows(aspectos) %>%
                                    group_by(bloque, pregunta, tipo_pregunta, llaves, tema) %>%
                                    summarise(respuestas=list(text)) %>%
                                    ungroup() %>% arrange(llaves)

                                return(self$diccionario)

                              })
)

#'Esta es la clase de pregunta
#'@export
Pregunta <- R6::R6Class("Pregunta",
                        public=list(
                          texto_completo=NULL,
                          llave=NULL,
                          aspectos=NULL,
                          opciones=NULL,
                          dependencia=NULL,
                          tipo=NULL,
                          encuesta = NULL,
                          graficadas = NULL,
                          tema = NULL,
                          initialize = function(encuesta, tema = tema_default){
                            self$encuesta <- encuesta
                            self$tema <- tema
                          },
                          graficar = function(llave, tipo, aspectos, tit = NULL){
                            if(tipo == "frecuencia"){
                              llave_aux <- quo_name(enquo(llave))
                              if(!(llave_aux %in% self$graficadas)){
                                if(llave_aux %in% self$encuesta$cuestionario$diccionario$llaves){
                                  self$graficadas <- self$graficadas %>% append(llave_aux)
                                } else{
                                  stop(glue::glue("La llave {llave_aux} no existe en el diccionario"))
                                }
                              } else{
                                warning(glue::glue("La llave {llave_aux} ya fue graficada con anterioridad"))
                              }
                              g <- encuestar::analizar_frecuencias(self$encuesta, {{llave}}) %>%
                                encuestar::graficar_barras_frecuencia(titulo = tit) + self$tema()
                            }
                            if(tipo == "aspectos"){
                              aspectos_aux <- paste(quo_name(enquo(llave)), aspectos, sep = "_")

                              if(!all(aspectos_aux %in% self$graficadas)){
                                if(all(aspectos_aux %in% self$encuesta$cuestionario$diccionario$llaves)){
                                  self$graficadas <- self$graficadas %>% append(aspectos_aux)
                                } else{
                                  stop(glue::glue("Alguna o todas las llaves {paste(aspectos_aux, collapse = ', ')} no existe en el diccionario"))
                                }
                              } else{
                                warning(glue::glue("Las llaves {paste(aspectos_aux, collapse = ', ')} ya fueron graficadas con anterioridad"))
                              }

                              g <- analizar_frecuencias_aspectos(encuesta_qro,{{llave}},aspectos) %>% rename(grupo=respuesta, respuesta=aspecto)
                            }


                            return(g)

                          },
                          faltantes = function(){
                            gant_p_r(self$encuesta$cuestionario$diccionario %>% filter(!llaves %in% self$graficadas))
                          }
                        ))
