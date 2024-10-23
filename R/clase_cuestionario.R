#'Esta es la clase cuestionario
#'
Cuestionario <- R6::R6Class("Cuestionario",
                            public = list(
                              documento = NULL,
                              aprobado = NULL,
                              diccionario = NULL,
                              initialize = function(documento, patron){
                                if("data.frame" %in% class(documento)){
                                  llaves_repetidas = comprobar_unicidadLlaves(diccionario = documento)
                                  if(0 < llaves_repetidas) {
                                    stop(glue::glue("Hay ", llaves_repetidas, " llaves repetidas en el diccionario"))
                                  }
                                  self$diccionario <- documento
                                } else{
                                  self$documento <- documento %>% officer::docx_summary() %>% as_tibble
                                  self$diccionario <- private$crear_diccionario(patron)
                                }
                              },
                              aprobar = function(){
                                self$aprobado <- T
                                return(invisible(self))
                              },
                              checar_pregunta = function(pregunta){
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
                            private = list(
                              crear_diccionario = function(patron){
                                diccionario <- diccionario_cuestionario(self$documento, patron)
                                return(diccionario)
                              })
)
