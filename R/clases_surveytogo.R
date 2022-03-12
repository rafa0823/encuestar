#' Esta es la clase Encuesta
#' @description La clase Encuesta contiene todos los metodos, y atributos que puede tener una encuesta.
#' @field respuestas Base de datos de respuestas
#' @field muestra Base de datos de muestra.
#' @field dicionario Base de datos de diccionario
#' @export
#' @import dplyr ggplot2 tidyr sf purrr stringr highcharter

Encuesta <- R6::R6Class("Encuesta",
                        public = list(
                          respuestas = NULL,
                          cuestionario=NULL,
                          muestra = NULL,
                          auditoria_telefonica=NA,
                          preguntas = NULL,
                          shp_completo = NULL,
                          shp = NULL,
                          tipo_encuesta = NULL,
                          mantener = NULL,
                          auditoria = NULL,
                          #' @description
                          #' Create a person
                          #' @param respuestas Name of the person
                          #' @param diccionario Hair colour
                          initialize = function(respuestas = NA,
                                                muestra = NA,
                                                auditoria_telefonica = NA,
                                                cuestionario=NA,
                                                shp = NA,
                                                tipo_encuesta = NA,
                                                mantener = NA) {
                            sf_use_s2(F)
                            tipo_encuesta <- match.arg(tipo_encuesta,c("inegi","ine"))
                            self$tipo_encuesta <- tipo_encuesta
                            # Valorar si no es mejor un active binding
                            un <- muestra$niveles %>% filter(nivel == muestra$ultimo_nivel)
                            nivel <- un %>% unite(nivel, tipo, nivel) %>% pull(nivel)
                            var_n <- un %>% pull(variable)

                            # Valorar active binding
                            self$cuestionario <- Cuestionario$new(documento = cuestionario)
                            # Valorar active binding
                            self$auditoria_telefonica <- auditoria_telefonica

                            self$shp_completo <- shp

                            self$shp <- shp$shp %>% purrr::pluck(var_n) %>%
                              inner_join(muestra$muestra %>% purrr::pluck(var_n) %>% unnest(data) %>%
                                           distinct(!!rlang::sym(var_n) := !!rlang::sym(var_n),cluster_3))
                            self$mantener <- mantener

                            # Respuestas
                            self$respuestas <- Respuestas$new(base = respuestas %>% mutate(cluster_0 = SbjNum),
                                                              encuesta = self,
                                                              muestra_completa = muestra,
                                                              nivel = nivel, var_n = var_n
                            )
                            # Muestra (recalcula fpc)
                            self$muestra <- Muestra$new(muestra = muestra, respuestas = self$respuestas$base,
                                                        nivel = nivel, var_n = var_n)
                            # Informacion muestral
                            self$respuestas$vars_diseno(muestra = self$muestra, var_n = var_n, tipo_encuesta = self$tipo_encuesta)
                            # Diseno
                            self$muestra$extraer_diseno(respuestas = self$respuestas$base,
                                                        marco_muestral = self$muestra$muestra$poblacion$marco_muestral,
                                                        tipo_encuesta = self$tipo_encuesta)

                            #Preguntas

                            self$preguntas <- Pregunta$new(encuesta = self)

                            self$auditoria <- Auditoria$new(self, tipo_encuesta = self$tipo_encuesta)

                            return(print(match_dicc_base(self)))
                          },
                          exportar_entregable = function(carpeta = "Entregables", agregar = NULL, quitar = NULL){
                            if(!file.exists(carpeta)) dir.create(carpeta)
                            #Exportar bd
                            exportar_bd(self, carpeta, agregar, quitar)
                            # Exportar diccionario
                            self$cuestionario$diccionario %>% unnest(respuestas) %>%
                              readr::write_excel_csv(glue::glue("{carpeta}/diccionario.csv"))
                          }),
                        private=list()
)

#' Esta es la clase Respuestas
#' @export
Respuestas <- R6::R6Class("Respuestas",
                          inherit = Encuesta,
                          public = list(
                            eliminadas = NULL,
                            base = NULL,
                            n=NULL,
                            m=NULL,
                            #' @description
                            #' Crear respuesta
                            #' @param base Base de datos de respuestas.
                            initialize=function(base,
                                                encuesta,
                                                muestra_completa,
                                                nivel, var_n) {
                              shp <- encuesta$shp
                              mantener <- encuesta$mantener
                              diccionario <- encuesta$cuestionario$diccionario
                              auditoria_telefonica <- encuesta$auditoria_telefonica
                              muestra <- muestra_completa$muestra %>% purrr::pluck(var_n)

                              self$base <- base

                              # Limpiar las que no pasan auditoria telefonica
                              self$eliminar_auditoria_telefonica(auditoria_telefonica)

                              # Limpiar las respuestas que no tienen coordenadas
                              self$eliminar_falta_coordenadas()

                              # Eliminar entrevistas cuyo cluster no pertenece a la muestra
                              self$eliminar_fuera_muestra(self$base, muestra, nivel, var_n)
                              # Corregir cluster equivocado
                              self$correccion_cluster(self$base, shp, mantener, nivel, var_n)
                              # Calcular distancia de la entrevista al cluster correcto
                              self$calcular_distancia(base = self$base,
                                                      encuesta = encuesta,
                                                      muestra = muestra_completa,
                                                      var_n = var_n, nivel = nivel)
                              # Limpiar las que no tienen variables de diseno
                              self$eliminar_faltantes_diseno()

                              # Cambiar variables a tipo numerica
                              numericas <- diccionario %>% filter(tipo_pregunta == "numericas") %>%
                                pull(llaves)

                              self$base <- self$base %>%
                                mutate(across(all_of(numericas), ~readr::parse_number(.x)))

                              self$eliminadas <- anti_join(base, self$base, by = "SbjNum")
                            },
                            eliminar_auditoria_telefonica=function(auditoria_telefonica){
                              if(("SbjNum" %in% names(self$base)) &
                                 ("SbjNum" %in% names(auditoria_telefonica))){
                                if(is.character(auditoria_telefonica$SbjNum)) auditoria_telefonica <- auditoria_telefonica %>% mutate(SbjNum = readr::parse_double(SbjNum))
                                # Se eliminan por no pasar la auditoria telefonica
                                n <- nrow(self$base)
                                self$base <- self$base %>%
                                  anti_join(auditoria_telefonica, by="SbjNum")
                                # Mandar mensaje
                                print(
                                  glue::glue("Se eliminaron {n-nrow(self$base)} encuestas por auditoria telefonica")
                                )
                              }
                              else cat("Identificador SbjNum no presente en alguna de las bases para eliminar por auditoria telefonica")
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
                            correccion_cluster = function(base, shp, mantener, nivel, var_n){
                              self$base <- corregir_cluster(base, shp, mantener, nivel, var_n)
                            },
                            eliminar_fuera_muestra = function(respuestas, muestra, nivel, var_n){
                              self$base <- respuestas %>%
                                semi_join(muestra %>% mutate(!!rlang::sym(nivel) := as.character(!!rlang::sym(nivel))),
                                          by = set_names(nivel,var_n))
                              print(glue::glue("Se eliminaron {nrow(respuestas) - nrow(self$base)} entrevistas ya que el cluster no pertenece a la muestra"))
                            },
                            calcular_distancia = function(base, encuesta, muestra, var_n, nivel){
                              aux_sf <- base %>% st_as_sf(coords = c("Longitude", "Latitude"),crs = 4326)

                              pol <- dist_poligonos(aux_sf, shp = encuesta$shp, var_n, nivel)
                              puntos <- dist_puntos(aux_sf, encuesta, muestra, var_n, nivel)

                              respuestas <- pol %>%
                                anti_join(
                                  puntos %>% as_tibble, by = "SbjNum"
                                ) %>% bind_rows(
                                  puntos
                                )
                              self$base <- respuestas %>% left_join(base %>% select(SbjNum, Longitude, Latitude), by = "SbjNum")
                            },
                            eliminar_faltantes_diseno=function(){
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
                                glue::glue("Se eliminaron {n-nrow(self$base)} encuestas por tener datos faltantes en las variables de diseno muestral")
                              )
                              return(self$base)
                            },
                            vars_diseno = function(muestra, var_n, tipo_encuesta){
                              self$base <- self$base %>%
                                inner_join(muestra$base)

                              if(tipo_encuesta == "inegi"){
                                self$base <- self$base %>%
                                  mutate(rango_edad = as.character(cut(as.integer(edad),c(17, 24, 59, 200),
                                                                       c("18A24","25A59","60YMAS"))),
                                         sexo = if_else(sexo == "Mujer", "F", "M"))
                              }

                              if(tipo_encuesta == "ine"){
                                self$base <- self$base %>%
                                  mutate(rango_edad = cut(as.numeric(edad), c(17,24,39,59,Inf),
                                                          labels = c("18A24","25A39","40A60","60YMAS")),
                                         sexo = if_else(sexo == "Mujer", "F", "M"))
                              }

                              if(sum(grepl("region", muestra$muestra$niveles$variable)) > 0){
                                var_reg <- muestra$muestra$niveles %>% filter(variable == "region") %>%
                                  unite("var_reg", c(tipo, nivel)) %>% pull(var_reg)
                                self$base <- self$base %>%
                                  inner_join(muestra$muestra$poblacion$marco_muestral %>%
                                               distinct(across(all_of(var_reg)), region), by = var_reg)
                              }
                            }
                          )
)


#'Esta es la clase de muestra
#'@export
Muestra <- R6::R6Class("Muestra",
                       public=list(
                         muestra = NULL,
                         base=NULL,
                         diseno=NULL,
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
                         extraer_diseno = function(respuestas, marco_muestral, tipo_encuesta){
                           r <- try(
                             survey::svydesign(
                               pps="brewer",
                               ids=crear_formula_nombre(respuestas, "cluster_"),
                               fpc = crear_formula_nombre(respuestas, "fpc_"),
                               strata = crear_formula_nombre(respuestas, "strata_"),
                               data = respuestas
                             )
                             ,T)

                           diseno <- if(class(r) == "try-error"){
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
                                self$documento <- documento %>% officer::docx_summary() %>% as_tibble
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
                                diccionario <- diccionario_cuestionario(self$documento)
                                return(diccionario)

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
                              g <- analizar_frecuencias_aspectos(self$encuesta,{{llave}},aspectos)
                            }


                            return(g)

                          },
                          faltantes = function(){
                            gant_p_r(self$encuesta$cuestionario$diccionario %>% filter(!llaves %in% self$graficadas))
                          }
                        ))

#' Title
#'
#' @param encuesta
#' @param dir
#'
#' @return
#' @export
#'
#' @examples
Auditoria <- R6::R6Class("Auditoria",
                         public = list(
                           dir = NULL,
                           initialize = function(encuesta, tipo_encuesta, dir = "auditoria"){
                             if(!file.exists(dir)){
                               dir.create(dir)
                               dir.create(glue::glue("{dir}/data"))
                             }
                             readr::write_rds(encuesta$muestra$muestra, glue::glue("{dir}/data/diseno.rda"))
                             readr::write_rds(encuesta$shp_completo, glue::glue("{dir}/data/shp.rda"))
                             readr::write_excel_csv(encuesta$respuestas$base, glue::glue("{dir}/data/bd.csv"))
                             sf_use_s2(T)
                             mapa_base <- encuesta$shp_completo$shp$MUNICIPIO %>%
                               left_join(encuesta$muestra$muestra$poblacion$marco_muestral %>% distinct(MUNICIPIO,strata_1)) %>%
                               group_by(strata_1) %>% summarise(n()) %>%
                               sf::st_buffer(dist = 0)
                             readr::write_rds(mapa_base, glue::glue("{dir}/data/mapa_base.rda"))
                             enc_shp <- encuesta$respuestas$base %>%
                               sf::st_as_sf(coords = c("Longitude","Latitude"), crs = "+init=epsg:4326")
                             readr::write_rds(enc_shp, glue::glue("{dir}/data/enc_shp.rda"))
                             readr::write_excel_csv(encuesta$respuestas$eliminadas, glue::glue("{dir}/data/eliminadas.csv"))
                             if(tipo_encuesta == "inegi"){
                               file.copy(
                                 from = system.file("app_inegi/app.R", package = "encuestar",
                                                    mustWork = TRUE),
                                 to = dir

                               )
                             }

                             if(tipo_encuesta == "ine"){
                               file.copy(
                                 from = system.file("app_ine/app.R", package = "encuestar",
                                                    mustWork = TRUE),
                                 to = dir

                               )
                             }

                             self$dir <- dir
                           },
                           run_app = function(){
                             shiny::shinyAppDir(
                               self$dir
                             )
                           },
                           subir_app = function(){
                             rsconnect::deployApp(self$dir)
                           }
                         ))
