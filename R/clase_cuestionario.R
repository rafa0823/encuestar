#' Clase Cuestionario
#'
#' @description La clase `Cuestionario` tiene como objetivo almacenar, verificar y estandarizar, de
#'  acuerdo a las necesidades de la paquetería, el diccionario usado en procesamiento y producción
#'  de resultados de la encuesta.
#'
#' @field documento Actualmente contiene el diccionario usado en procesamiento y producción de
#'  resultados
#' @field aprobado Campo en desuso.
#' @field diccionario Anteriormente el diccionario era producido al procesar el campo  `documento`.
#'  Este proceso ya no es necesario.
Cuestionario <-
  R6::R6Class(classname = "Cuestionario",
              public = list(
                documento = NULL,
                aprobado = NULL,
                diccionario = NULL,
                #' @description Recibe y establece el diccionario de procesamiento
                #'
                #' @param documento [tibble()] qque contiene el diccionario (codebook) del cuestionario
                #'  aplicado en la encuesta.
                #' @param patron Valor tipo caracter que se omitirán de las preguntas.
                #'  Por ejmplo "Rotar respuestas".
                initialize = function(documento,
                                      patron){
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
                #' @description En desuso.
                aprobar = function(){
                  self$aprobado <- T
                  return(invisible(self))
                },
                #' @description En desuso.
                #' @param pregunta En desuso
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
                #' @description Visualiza el diccionario para mostrar extensión de cada bloque y sus
                #'  posibles respuestas a cada pregunta
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
                }
              )
  )
