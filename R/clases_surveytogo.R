#' Esta es la clase Encuesta
#' @description La clase Encuesta contiene todos los métodos, y atributos que puede tener una encuesta.
#' @field respuestas Base de datos de respuestas
#' @field muestra Base de datos de muestra.
#' @field dicionario Base de datos de diccionario
#' @export
Encuesta <- R6::R6Class("Encuesta",
                        public = list(
                          respuestas = NULL,
                          diccionario=NULL,
                          muestra = NULL,
                          auditoria_telefonica=NA,
                          diseño = NULL,
                          #' @description
                          #' Create a person
                          #' @param respuestas Name of the person
                          #' @param diccionario Hair colour
                          initialize = function(respuestas = NA,
                                                diccionario = NA,
                                                muestra = NA,
                                                auditoria_telefonica = NA) {
                            self$respuestas <- Respuestas$new(base = respuestas)
                            # Valorar si no es mejor una active binding
                            self$muestra <- Muestra$new(base = muestra)
                            # Valorar active binding
                            self$diccionario <- diccionario
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
                          initialize=function(){

                          },
                          crear_diccionario=function(){

                          },
                          aprobar=function(){
                            self$aprobado <- T
                            return(invisible(self))
                          }
                        ))

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
