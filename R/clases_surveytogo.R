#' Esta es la clase Encuesta
#' @description La clase Encuesta contiene todos los métodos, y atributos que puede tener una encuesta.
#' @field respuestas Base de datos de respuestas
#' @field muestra Base de datos de muestra.
#' @field dicionario Base de datos de diccionario
#' @export
Encuesta <- R6::R6Class("Encuesta",
                        public = list(
                          respuestas = NULL,
                          cuestionario=NULL,
                          muestra = NULL,
                          auditoria_telefonica=NA,
                          diseño = NULL,
                          #' @description
                          #' Create a person
                          #' @param respuestas Name of the person
                          #' @param diccionario Hair colour
                          initialize = function(respuestas = NA,
                                                muestra = NA,
                                                auditoria_telefonica = NA,
                                                cuestionario=NA) {
                            self$respuestas <- Respuestas$new(base = respuestas)
                            # Valorar si no es mejor una active binding
                            self$muestra <- Muestra$new(base = muestra)
                            # Valorar active binding
                            self$cuestionario <- Cuestionario$new(documento = cuestionario)
                            # Valorar active bindign
                            self$auditoria_telefonica <- auditoria_telefonica
                            # Procesos ####
                            self$respuestas <- private$limpiar_respuestas()
                            self$diseño <- self$muestra$extraer_diseño(self$respuestas$base)
                          }),
                        private=list(
                          limpiar_respuestas=function(){
                            # Limpiar las que no pasan auditoría telefónica
                            self$respuestas$eliminar_auditoria_telefonica(self$auditoria_telefonica)
                            # Limpiar las que no tienen variables de diseño
                            self$respuestas$eliminar_faltantes_diseño()
                            return(invisible(self$respuestas))
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
                         extraer_diseño=function(respuestas){
                           diseño=survey::svydesign(
                             ids=crear_formula_nombre(self$base, "id_"),
                             # fpc = crear_formula_nombre(self$base, "fpc_"),
                             data = respuestas
                           )
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
                              })
                            ,
                            private=list(
                              crear_diccionario=function(){
                                self$diccionario <- officer::docx_summary(self$documento) %>%
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
                                  separate(style_name, c("a", "b", "c"), sep = "_") %>%
                                  rename("tipo_pregunta"="c") %>%
                                  group_by(bloque, pregunta, tipo_pregunta) %>%
                                  summarise(respuestas=list(text)) %>%
                                  ungroup() %>%
                                  mutate(llaves=stringr::str_extract(pregunta, "(?<=\\{).+?(?=\\})"),
                                         llaves=stringr::str_squish(llaves),
                                         pregunta=stringr::str_remove(pregunta, "\\{.+\\}"),
                                         pregunta=stringr::str_squish(pregunta))
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
                          tipo=NULL
                        ))
