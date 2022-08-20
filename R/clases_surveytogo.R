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
                          patron = NA,
                          auditar = NA,
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
                                                mantener = NA,
                                                patron = NA,
                                                auditar = NA) {
                            sf_use_s2(F)
                            tipo_encuesta <- match.arg(tipo_encuesta,c("inegi","ine"))
                            self$tipo_encuesta <- tipo_encuesta
                            self$patron <- patron
                            self$auditar <- auditar
                            # Valorar si no es mejor un active binding
                            un <- muestra$niveles %>% filter(nivel == muestra$ultimo_nivel)
                            nivel <- un %>% unite(nivel, tipo, nivel) %>% pull(nivel)
                            var_n <- un %>% pull(variable)

                            # Valorar active binding
                            self$cuestionario <- Cuestionario$new(documento = cuestionario, patron)
                            # Valorar active binding
                            self$auditoria_telefonica <- auditoria_telefonica %>% distinct(SbjNum, .keep_all = T)

                            self$shp_completo <- shp

                            self$shp <- shp$shp %>% purrr::pluck(var_n) %>%
                              inner_join(muestra$muestra %>% purrr::pluck(var_n) %>% unnest(data) %>%
                                           distinct(!!rlang::sym(var_n) := !!rlang::sym(var_n),cluster_3))
                            self$mantener <- mantener
                            # Respuestas
                            self$respuestas <- Respuestas$new(base = respuestas %>% mutate(cluster_0 = SbjNum),
                                                              encuesta = self,
                                                              muestra_completa = muestra,
                                                              patron = patron,
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
                          error_muestral_maximo = function(quitar_patron = NULL){
                            aux <- self$cuestionario$diccionario %>% filter(tipo_pregunta == "multiples")
                            if(!is.null(quitar_patron)) {
                              quitar_patron <- paste(quitar_patron, collapse = "|")
                              aux <- aux %>% filter(!grepl(quitar_patron, x = llaves))
                            }

                            aux <- aux %>% mutate(n = map_int(respuestas,~length(.x)))
                            aux <- aux %>%
                              pull(llaves) %>% map_df(~{
                                # nas <- self$respuestas$base %>% summarise(any(is.na(c_across(.x)))) %>% pull(1)
                                survey::svymean(survey::make.formula(.x), design = self$muestra$diseno, na.rm = T) %>%
                                  tibble::as_tibble(rownames = "respuesta") %>%
                                  # rename(SE = 3) %>%
                                  mutate(pregunta = .x,
                                         # tiene_na = !!nas,
                                         # respuesta = stringr::str_replace(string = respuesta,
                                         #                                  pattern = as.character(.x),
                                         #                                  replacement = "")
                                  ) %>% select(respuesta, mean, SE)
                              }) %>% mutate(SE = qnorm(.95)*SE)

                            labels <- aux %>% summarise(inf = quantile(SE,.25,na.rm = T),
                                                        mid = quantile(SE,.5,na.rm = T),
                                                        sup = quantile(SE,.75,na.rm = T),
                                                        max = max(SE,na.rm = T)
                            ) %>% mutate(iqr_min = inf-1.5*(sup-inf),
                                         iqr_max = sup + 1.5*(sup-inf)) %>%
                              pivot_longer(everything(), names_to = "stat", values_to = "valor")

                            a <- aux %>%
                              ggplot() +
                              geom_boxplot(aes(x = 0, y = SE)) +
                              geom_label(data = labels, aes(x = 0, y = valor, label = scales::percent(valor)),
                                         hjust = 0, vjust = 0, nudge_x = .01) + labs(x = NULL) +
                              scale_y_continuous(labels = scales::percent_format(1))

                            b <- aux %>% filter(SE >= labels %>% filter(stat == "sup") %>% pull(valor)) %>%
                              ggplot() + geom_col(aes(y = reorder(respuesta, SE), x = SE)) +
                              geom_vline(xintercept = labels %>% filter(stat == "sup") %>% pull(valor))+
                              labs(y = NULL)+
                              scale_x_continuous(labels = scales::percent_format(1))

                            a + b
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
                            cluster_corregido = NULL,
                            base = NULL,
                            n=NULL,
                            m=NULL,
                            #' @description
                            #' Crear respuesta
                            #' @param base Base de datos de respuestas.
                            initialize=function(base,
                                                encuesta,
                                                muestra_completa,
                                                patron,
                                                nivel, var_n) {
                              shp <- encuesta$shp
                              mantener <- encuesta$mantener
                              diccionario <- encuesta$cuestionario$diccionario

                              if(!identical(names(encuesta$auditoria_telefonica), c("SbjNum", "razon"))) stop("Los nombres de las columnas de la base de datos de auditoría telefónica deben ser: 'Sbjnum, razon'")
                              auditoria_telefonica <- encuesta$auditoria_telefonica
                              muestra <- muestra_completa$muestra %>% purrr::pluck(var_n)

                              self$base <- base

                              # Parar si nombres de respuestas no coinciden con diccionario
                              self$nombres(self$base, diccionario)

                              #Quitar patrones a respuestas

                              self$q_patron(self$base, diccionario, patron)

                              # Parar si opciones de respuesta no coinciden con diccionario
                              self$categorias(self$base, diccionario)

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
                              # self$eliminar_faltantes_diseno() # no entiendo por qué

                              # Cambiar variables a tipo numerica
                              numericas <- diccionario %>% filter(tipo_pregunta == "numericas") %>%
                                pull(llaves)

                              self$base <- self$base %>%
                                mutate(across(all_of(numericas), ~readr::parse_number(.x)))

                              # self$eliminadas <- anti_join(base, self$base, by = "SbjNum")
                            },
                            nombres = function(bd, diccionario){
                              faltantes <- is.na(match(diccionario$llaves, names(bd)))
                              if(!all(!faltantes)){
                                stop(glue::glue("Las siguientes variables no se encuentran en la base de datos: {paste(diccionario$llaves[faltantes], collapse = ', ')}"))
                              }
                            },
                            q_patron = function(bd, dicc, patron){
                              if(!is.na(patron)){
                                aux <- bd %>%
                                  mutate(across(c(all_of(dicc$llaves),where(is.character)),
                                                ~stringr::str_squish(gsub(x = .x,pattern = patron, ""))))
                                self$base <- aux
                              }
                            },
                            categorias = function(bd, diccionario){

                              discrepancia <- diccionario %>% filter(tipo_pregunta == "multiples", !grepl("_otro", llaves)) %>% pull(llaves) %>%
                                map_df(~{

                                  res <- bd %>% count(across(all_of(.x))) %>% na.omit %>% pull(1)
                                  m <- match(
                                    res,
                                    diccionario %>% filter(llaves == .x) %>% unnest(respuestas) %>% pull(respuestas)
                                  )

                                  tibble(
                                    llave = .x,
                                    faltantes = res[is.na(m)]
                                  )

                                })

                              if(nrow(discrepancia)>0){
                                print(discrepancia)
                                warning("Revisar la base impresa arriba pues hay respuestas que no se contemplaron en el diccionario.")
                              }

                            },
                            eliminar_auditoria_telefonica=function(auditoria_telefonica){
                              if(("SbjNum" %in% names(self$base)) &
                                 ("SbjNum" %in% names(auditoria_telefonica))){
                                if(is.character(auditoria_telefonica$SbjNum)) auditoria_telefonica <- auditoria_telefonica %>% mutate(SbjNum = readr::parse_double(SbjNum))
                                # Se eliminan por no pasar la auditoria telefonica
                                n <- nrow(self$base)

                                self$eliminadas <- self$base %>% inner_join(auditoria_telefonica, by = "SbjNum")

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
                                self$eliminadas <- self$eliminadas %>% bind_rows(
                                  self$base %>% filter(is.na(Longitude) | is.na(Latitude)) %>% mutate(razon = "Sin coordenadas")
                                )
                                print(
                                  glue::glue("Se eliminaron {n-nrow(self$base)} encuestas por falta de coordenadas")
                                )
                              } else{
                                print("No existe la columna Longitude ni Latitude en la base de respuestas")
                              }
                            },
                            correccion_cluster = function(base, shp, mantener, nivel, var_n){
                              aux <- corregir_cluster(base, shp, mantener, nivel, var_n)

                              self$cluster_corregido <- self$base %>% select(all_of(c("SbjNum", "Srvyr", "Date", "Longitude", "Latitude", var_n))) %>%
                                anti_join(aux, by = c("SbjNum", var_n)) %>%
                                left_join(
                                  aux %>% select(all_of(c("SbjNum", var_n))), by = "SbjNum"
                                ) %>% rename(anterior = 6, nueva = 7)

                              self$base <- aux
                            },
                            eliminar_fuera_muestra = function(respuestas, muestra, nivel, var_n){
                              self$base <- respuestas %>%
                                semi_join(muestra %>% mutate(!!rlang::sym(nivel) := as.character(!!rlang::sym(nivel))),
                                          by = set_names(nivel,var_n))

                              self$eliminadas <- self$eliminadas %>% bind_rows(
                                respuestas %>%
                                  anti_join(muestra %>% mutate(!!rlang::sym(nivel) := as.character(!!rlang::sym(nivel))),
                                             by = set_names(nivel,var_n)) %>%
                                  mutate(razon = "Cluster no existente")
                              )
                              print(glue::glue("Se eliminaron {nrow(respuestas) - nrow(self$base)} entrevistas ya que el cluster no pertenece a la muestra"))
                            },
                            calcular_distancia = function(base, encuesta, muestra, var_n, nivel){
                              aux_sf <- base %>% st_as_sf(coords = c("Longitude", "Latitude"),crs = 4326)

                              pol <- dist_poligonos(aux_sf, shp = encuesta$shp, var_n, nivel)
                              puntos <- dist_puntos(aux_sf, encuesta, muestra, var_n, nivel)

                              if(nrow(puntos) > 0){
                                respuestas <- pol %>%
                                  anti_join(
                                    puntos %>% as_tibble, by = "SbjNum"
                                  ) %>% bind_rows(
                                    puntos
                                  )
                              } else{
                                respuestas <- pol
                              }

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
                              vars_join <- c(var_n,
                                             names(muestra$base)[is.na(match(names(muestra$base), names(self$base)))]
                              )

                              self$base <- self$base %>%
                                inner_join(muestra$base %>% select(all_of(vars_join)))

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
                              initialize=function(documento, patron){
                                self$documento <- documento %>% officer::docx_summary() %>% as_tibble
                                self$diccionario <- private$crear_diccionario(patron)
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
                              crear_diccionario=function(patron){
                                diccionario <- diccionario_cuestionario(self$documento, patron)
                                return(diccionario)

                              })
)

#'Esta es la clase de pregunta
#'@export
Pregunta <- R6::R6Class("Pregunta",
                        public=list(
                          regiones = NULL,
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
                            self$regiones_shp()

                          },
                          graficar = function(llave, tipo, aspectos = NULL, filtro = NULL,
                                              llave_partido, llave_conocimiento,
                                              llave_opinion,
                                              llave_xq,
                                              parametros = list(tit = "")){
                            tipo <- match.arg(tipo, choices = c("frecuencia", "promedio", "texto_barras", "texto_nube",
                                                                "candidato_opinion", "candidato_saldo", "candidato_partido"))
                            if(tipo == "frecuencia"){
                              if(is.null(aspectos)){
                                tipo_p <- self$encuesta$cuestionario$diccionario %>%
                                  filter(llaves == quo_name(enquo(llave))) %>% pull(tipo_pregunta)

                                if("numericas" == tipo_p){
                                  v_params <- c("color", "maximo")

                                  if(sum(is.na(match(v_params, names(parametros)))) > 0) stop(glue::glue("Especifique los parametros {paste(v_params[is.na(match(v_params, names(parametros)))], collapse= ', ')}"))
                                  g <- encuestar::analizar_frecuencias(self$encuesta, {{llave}}) %>%
                                    graficar_gauge_promedio(color = parametros$color, maximo = parametros$maximo,
                                                            familia = self$tema()$text$family)
                                } else{
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
                                  v_params <- c("tit", "salto")

                                  if(sum(is.na(match(v_params, names(parametros)))) > 0) stop(glue::glue("Especifique los parametros {paste(v_params[is.na(match(v_params, names(parametros)))], collapse= ', ')}"))

                                  g <- encuestar::analizar_frecuencias(self$encuesta, {{llave}}) %>%
                                    encuestar::graficar_barras_frecuencia(titulo = parametros$tit,
                                                                          salto = parametros$salt) + self$tema()
                                }
                              } else{
                                if(quo_name(enquo(llave)) != "NULL") {
                                  aspectos_aux <- paste(quo_name(enquo(llave)), aspectos, sep = "_")
                                } else {
                                  aspectos_aux <- aspectos
                                }

                                v_params <- c("tipo_numerica")
                                if(sum(is.na(match(v_params, names(parametros)))) > 0) stop(glue::glue("Especifique los parametros {paste(v_params[is.na(match(v_params, names(parametros)))], collapse= ', ')}"))

                                tipo_p <- self$encuesta$cuestionario$diccionario %>%
                                  filter(llaves %in% aspectos_aux) %>% pull(tipo_pregunta)
                                if("numericas" %in% tipo_p){
                                  g <- analizar_frecuencias_aspectos(self$encuesta, {{llave}}, aspectos) %>%
                                    left_join(
                                      self$encuesta$preguntas$encuesta$cuestionario$diccionario %>% select(aspecto = llaves, tema)
                                    )
                                  if(parametros$tipo_numerica == "intervalos"){
                                    g <- g %>% graficar_intervalo_numerica() + self$tema()
                                  }
                                  if(parametros$tipo_numerica == "barras"){
                                    g <- g %>% graficar_barras_numerica() + self$tema()
                                  }


                                } else{
                                  if(!all(aspectos_aux %in% self$graficadas)){
                                    if(all(aspectos_aux %in% self$encuesta$cuestionario$diccionario$llaves)){
                                      self$graficadas <- self$graficadas %>% append(aspectos_aux)
                                    } else{
                                      stop(glue::glue("Alguna o todas las llaves {paste(aspectos_aux, collapse = ', ')} no existe en el diccionario"))
                                    }
                                  } else{
                                    warning(glue::glue("Las llaves {paste(aspectos_aux, collapse = ', ')} ya fueron graficadas con anterioridad"))
                                  }

                                  g <- analizar_frecuencias_aspectos(self$encuesta, {{llave}}, aspectos)

                                  if(!is.null(filtro)) {
                                    v_params <- c("tit")
                                    if(sum(is.na(match(v_params, names(parametros)))) > 0) stop(glue::glue("Especifique los parametros {paste(v_params[is.na(match(v_params, names(parametros)))], collapse= ', ')}"))
                                    g <- g %>% filter(eval(rlang::parse_expr(filtro)))%>%
                                      mutate(tema = names(aspectos[match(gsub(pattern = glue::glue("{quo_name(enquo(llave))}_"),
                                                                              replacement = "",x = aspecto),aspectos)])) %>%
                                      select(-respuesta) %>%
                                      rename(respuesta = tema) %>%
                                      encuestar::graficar_barras_frecuencia(titulo = parametros$tit) + self$tema()
                                  } else{
                                    v_params <- c("tit", "nota", "grupo_positivo", "grupo_negativo", "ns_nc", "colores", "orden")
                                    if(sum(is.na(match(v_params, names(parametros)))) > 0) stop(glue::glue("Especifique los parametros {paste(v_params[is.na(match(v_params, names(parametros)))], collapse= ', ')}"))
                                    if(quo_name(enquo(llave)) != "NULL"){
                                      g <- g %>% left_join(
                                        self$encuesta$preguntas$encuesta$cuestionario$diccionario %>% select(aspecto = llaves, tema)
                                      )
                                    }else{
                                      g <- g %>% mutate(tema = names(aspectos[match(aspecto,aspectos)]))
                                    }
                                    g <- g %>%
                                      graficar_aspectos_frecuencias(
                                        titulo = parametros$tit,
                                        nota = parametros$nota,
                                        grupo_positivo = parametros$grupo_positivo,
                                        grupo_negativo = parametros$grupo_negativo,
                                        ns_nc = parametros$ns_nc,
                                        colores =  parametros$colores,
                                        familia = self$tema()$text$family
                                      ) + self$tema()
                                  }
                                }
                              }

                            }

                            if(stringr::str_detect(pattern = "candidato", tipo)){
                              if(stringr::str_detect(pattern = "opinion", tipo)){

                                v_params <- c("ns_nc", "regular", "grupo_positivo", "grupo_negativo", "colores")

                                if(sum(is.na(match(v_params, names(parametros)))) > 0) stop(glue::glue("Especifique los parametros {paste(v_params[is.na(match(v_params, names(parametros)))], collapse= ', ')}"))

                                if(quo_name(enquo(llave)) != "NULL") {
                                  aspectos_aux <- paste(quo_name(enquo(llave)), aspectos, sep = "_")
                                } else {
                                  aspectos_aux <- aspectos
                                }

                                if(!all(aspectos_aux %in% self$graficadas)){
                                  if(all(aspectos_aux %in% self$encuesta$cuestionario$diccionario$llaves)){
                                    self$graficadas <- self$graficadas %>% append(aspectos_aux)
                                  } else{
                                    stop(glue::glue("Alguna o todas las llaves {paste(aspectos_aux, collapse = ', ')} no existe en el diccionario"))
                                  }
                                } else{
                                  warning(glue::glue("Las llaves {paste(aspectos_aux, collapse = ', ')} ya fueron graficadas con anterioridad"))
                                }

                                g <- analizar_frecuencias_aspectos(self$encuesta, {{llave}}, aspectos) %>%
                                  left_join(
                                    self$encuesta$preguntas$encuesta$cuestionario$diccionario %>% select(aspecto = llaves, tema)
                                  ) %>%
                                  graficar_candidato_opinion(ns_nc = parametros$ns_nc,
                                                             regular = parametros$regular,
                                                             grupo_positivo= parametros$grupo_positivo,
                                                             grupo_negativo = parametros$grupo_negativo,
                                                             colores = parametros$colores,
                                                             tema = self$tema)

                              }
                              if(stringr::str_detect(pattern = "saldo", tipo)){

                                v_params <- c("grupo_positivo", "grupo_negativo", "tipo_combinacion","n_palabras")

                                if(sum(is.na(match(v_params, names(parametros)))) > 0) stop(glue::glue("Especifique los parametros {paste(v_params[is.na(match(v_params, names(parametros)))], collapse= ', ')}"))

                                if(quo_name(enquo(llave)) != "NULL") {
                                  aspectos_aux <- paste(llave_opinion, aspectos, sep = "_") %>%
                                    append(paste(llave_xq, aspectos, sep = "_"))
                                } else {
                                  aspectos_aux <- aspectos
                                }

                                if(!all(aspectos_aux %in% self$graficadas)){
                                  if(all(aspectos_aux %in% self$encuesta$cuestionario$diccionario$llaves)){
                                    self$graficadas <- self$graficadas %>% append(aspectos_aux)
                                  } else{
                                    stop(glue::glue("Alguna o todas las llaves {paste(aspectos_aux, collapse = ', ')} no existe en el diccionario"))
                                  }
                                } else{
                                  warning(glue::glue("Las llaves {paste(aspectos_aux, collapse = ', ')} ya fueron graficadas con anterioridad"))
                                }

                                bd <- analizar_frecuencias_aspectos(self$encuesta, {{llave_opinion}}, aspectos) %>%
                                  left_join(
                                    self$encuesta$preguntas$encuesta$cuestionario$diccionario %>% select(aspecto = llaves, tema)
                                  ) %>%
                                  organizar_opinion_saldo(llave_opinion, parametros$grupo_positivo, parametros$grupo_negativo)

                                texto <- ordenar_opinion_xq(self$encuesta$respuestas$base,
                                                            llave_opinion, llave_xq, aspectos,
                                                            parametros$grupo_positivo, parametros$grupo_negativo) %>%
                                  pclave_combinaciones_saldo(parametros$tipo_combinacion, parametros$n_palabras)

                                g <- left_join(bd, texto) %>%
                                  graficar_candidato_saldo(grupo_positivo = parametros$grupo_positivo,
                                                           grupo_negativo = parametros$grupo_negativo,
                                                           familia = self$tema()$text$family ) +
                                  self$tema()
                              }
                              if(stringr::str_detect(pattern = "partido", tipo)){

                                v_params <- c("corte_otro", "cliente", "tipo_conoce", "colores_candidato","colores_partido", "respuesta_conoce")

                                if(sum(is.na(match(v_params, names(parametros)))) > 0) stop(glue::glue("Especifique los parametros {paste(v_params[is.na(match(v_params, names(parametros)))], collapse= ', ')}"))

                                if(quo_name(enquo(llave)) != "NULL") {
                                  aspectos_aux <- paste(llave_partido, aspectos, sep = "_") %>%
                                    append(paste(llave_conocimiento, aspectos, sep = "_"))
                                } else {
                                  aspectos_aux <- aspectos
                                }

                                if(!all(aspectos_aux %in% self$graficadas)){
                                  if(all(aspectos_aux %in% self$encuesta$cuestionario$diccionario$llaves)){
                                    self$graficadas <- self$graficadas %>% append(aspectos_aux)
                                  } else{
                                    stop(glue::glue("Alguna o todas las llaves {paste(aspectos_aux, collapse = ', ')} no existe en el diccionario"))
                                  }
                                } else{
                                  warning(glue::glue("Las llaves {paste(aspectos_aux, collapse = ', ')} ya fueron graficadas con anterioridad"))
                                }
                                g <- analizar_candidato_partido(diseno = self$encuesta$muestra$diseno,
                                                                llave_partido = llave_partido,
                                                                llave_conocimiento = llave_conocimiento,
                                                                respuesta_conoce = parametros$respuesta_conoce,
                                                                candidatos = aspectos,
                                                                corte_otro = parametros$corte_otro) %>%
                                  map(
                                    ~.x %>%
                                      left_join(
                                        self$encuesta$preguntas$encuesta$cuestionario$diccionario %>% select(aspecto = llaves, tema)
                                      )
                                  ) %>%
                                  graficar_candidato_partido(cliente = parametros$cliente,
                                                             tipo_conoce = parametros$tipo_conoce,
                                                             colores_candidato = parametros$colores_candidato,
                                                             colores_partido = parametros$colores_partido,
                                                             tema = self$tema)
                              }
                            }

                            if(stringr::str_detect(pattern = "texto", tipo)){
                              if(stringr::str_detect(pattern = "nube", tipo)){

                                v_params <- c("n", "color1", "color2", "color3", "ancho", "alto")

                                if(sum(is.na(match(v_params, names(parametros)))) > 0) stop(glue::glue("Especifique los parametros {paste(v_params[is.na(match(v_params, names(parametros)))], collapse= ', ')}"))

                                g <- graficar_nube_frecuencias(bd = self$encuesta$respuestas$base, llave,
                                                               n = parametros$n,
                                                               color1 = parametros$color1, color2 = parametros$color2, color3 = parametros$color3,
                                                               familia = self$tema()$text$family,
                                                               ancho = parametros$ancho, alto = parametros$alto)
                              }

                              if(stringr::str_detect(pattern = "barras", tipo)){
                                v_params <- c("n", "nota","tit")

                                if(sum(is.na(match(v_params, names(parametros)))) > 0) stop(glue::glue("Especifique los parametros {paste(v_params[is.na(match(v_params, names(parametros)))], collapse= ', ')}"))

                                g <- graficar_barras_palabras(bd = self$encuesta$respuestas$base,
                                                              pregunta = llave, n = parametros$n,
                                                              nota = parametros$nota,
                                                              tit = parametros$tit) + self$tema()
                              }
                            }

                            return(g)

                          },
                          regiones_shp = function(){
                            sf_use_s2(T)
                            self$regiones <- self$encuesta$shp_completo$shp$MUNICIPIO %>%
                              left_join(
                                self$encuesta$muestra$muestra$poblacion$marco_muestral %>% distinct(region, MUNICIPIO), by = "MUNICIPIO"
                              ) %>% group_by(region) %>% summarise(n()) %>%
                              sf::st_buffer(dist = 0)
                            sf_use_s2(F)
                          },
                          correspondencia = function(var1, var2, legenda1 = NULL, legenda2 = NULL, colores = NULL){
                            analisis_correspondencia(var1, var2, legenda1, legenda2, diseno = self$encuesta$muestra$diseno, colores)
                          },
                          mapa_ganador = function(var, lugar = 1){
                            analizar_ganador_region(regiones = self$regiones, {{var}},
                                                    lugar = lugar,
                                                    diseno = self$encuesta$muestra$diseno) %>%
                              graficar_mapa_region({{var}})
                          },
                          mapa_numerico = function(var){
                            analizar_promedio_region(regiones = self$regiones, var = {{var}},
                                                     diseno = self$encuesta$muestra$diseno) %>%
                              graficar_mapa_region({{var}})
                          },
                          conocimiento_region = function(llave_conocimiento, candidatos, respuesta){
                            analizar_conocimiento_region(llave_conocimiento, candidatos, respuesta,
                                                         self$encuesta$muestra$diseno,
                                                         self$encuesta$preguntas$encuesta$cuestionario$diccionario) %>%
                              graficar_conocimiento_region()

                          },
                          saldo_region = function(llave_opinion = "", candidatos, ns_nc, cat_negativo, cat_regular, cat_positivo){
                            analizar_saldo_region(llave_opinion, candidatos, ns_nc, cat_negativo, cat_regular, cat_positivo,
                                                  diseno = self$encuesta$muestra$diseno,
                                                  diccionario = self$encuesta$preguntas$encuesta$cuestionario$diccionario) %>%
                              graficar_saldo_region()

                          },
                          pclave_region = function(var){
                            analizar_pclave_region(bd = self$encuesta$respuestas$base, var)
                          },
                          sankey = function(var1, var2){
                            aux <- survey::svytable(survey::make.formula(c(var1,var2)),
                                                    design = self$encuesta$muestra$diseno) %>%
                              tibble::as_tibble() %>% ggsankey::make_long(-n, value = n)


                            ggplot(aux, aes(x = x,
                                            value = value,
                                            next_x = next_x,
                                            node = node,
                                            next_node = next_node,
                                            fill = factor(node))) +
                              ggsankey::geom_sankey() + self$tema()
                          },
                          prcomp = function(variables){
                            pc <- survey::svyprcomp(survey::make.formula(variables),
                                                    design= self$encuesta$muestra$diseno,
                                                    scale=TRUE,scores=TRUE)
                            fviz_pca_biplot(pc, geom.ind = "point", labelsize = 2, repel = T)
                          },
                          blackbox_1d = function(vars, stimuli){
                            self$encuesta$respuestas$base %>% analizar_blackbox_1d(vars,stimuli) %>%
                              graficar_blackbox_1d()
                          },
                          morena = function(personajes, atributos, labels){
                            analizar_morena(self$encuesta$preguntas, personajes, atributos) %>%
                              graficar_morena(atributos)
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
                             readr::write_rds(encuesta$preguntas, glue::glue("{dir}/data/clase_pregunta.rda"))
                             # readr::write_rds(encuesta$muestra$muestra, glue::glue("{dir}/data/diseno.rda"))
                             # readr::write_rds(encuesta$shp_completo, glue::glue("{dir}/data/shp.rda"))
                             # readr::write_excel_csv(encuesta$respuestas$base, glue::glue("{dir}/data/bd.csv"))
                             sf_use_s2(T)
                             mapa_base <- encuesta$shp_completo$shp$MUNICIPIO %>%
                               left_join(encuesta$muestra$muestra$poblacion$marco_muestral %>% distinct(MUNICIPIO,strata_1)) %>%
                               group_by(strata_1) %>% summarise(n()) %>%
                               sf::st_buffer(dist = 0)
                             readr::write_rds(mapa_base, glue::glue("{dir}/data/mapa_base.rda"))

                             enc_shp <- encuesta$respuestas$base %>%
                               sf::st_as_sf(coords = c("Longitude","Latitude"), crs = "+init=epsg:4326")
                             readr::write_rds(enc_shp, glue::glue("{dir}/data/enc_shp.rda"))
                             # readr::write_excel_csv(encuesta$respuestas$eliminadas, glue::glue("{dir}/data/eliminadas.csv"))
                             if(tipo_encuesta == "inegi"){
                               file.copy(overwrite = T,
                                         from = system.file("app_inegi/app.R", package = "encuestar",
                                                            mustWork = TRUE),
                                         to = dir

                               )
                             }

                             if(tipo_encuesta == "ine"){
                               file.copy(overwrite = T,
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
                           subir_app = function(...){
                             rsconnect::deployApp(self$dir,...)
                           }
                         ))
