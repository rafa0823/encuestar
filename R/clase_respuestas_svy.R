#' Clase Respuestas_svy
#'
#' @description La clase `Resultados` es el nodo principal entre el cálculo de las estimaciones y la
#'  interfaz de usuario. Después de la clase `Encuesta`, la clase `Resultados` es la segunda más
#'  importante dentro de la paquetería. Áunque tiene clases de jerarquía menor, estas clumplen el papel
#'  de clasificadoras de métodos. De esta forma, la clase `Resultados` escencialmente realiza dos pasos
#'  procesat y visualizar.
#'
#' @field encuesta Clase de jerarquía superior. Campo heredado por la clase `Encuesta`.
#' @field diseno Diseno muestral actual. Aunque el diseno esté contenido en la clase `Encuesta`,
#'  el modo de procesamiento de encuesta que no se levantó en campo requiere de alimentar la clase
#'  `Resultados` con un diseno muestral independiente a la clase `Muestra`.
#' @field diccionario Diccionario de procesamiento. Aunque el diccionario esté contendio en la
#'  clase `Cuestionario` que a su vez está dentro de la clase `Encuesta` el modo de procesamiento de
#'  encuesta que no se levantói en campo requiere de alimentar la clase con un diccionario que no
#'  haya sido procesado por la clase `Cuestionario`.
#' @field Descriptiva Clase de jerarquía menor. Contiene los métodos que visualizan resultados de
#'  forma más simple como gráficas de barras de resultados de una sola variable.
#' @field Regiones Clase de jerarquía menor, Contiene los métodos que visualizan resultados basados
#'  en la estratificación de la encuesta.
#' @field Modelo Clase de jerarquía menor. Contiene métodos de análisis estadísitico como PCA.
#' @field Cruce Clase de jerarquía menor. Contiene métodos enfocados en presentar resultados
#'  cruzando dos o más variables.
#' @field Especial Clase de jerarquía menor. Contiene métodos para visualizar resultados construidos
#'  de forma personalizada.
#' @field Tendencias Clase de jerarquía menor. Contiene los metodos de estimacions usando la media móvil
#' @field tema Objeto [theme] de la paquetería [ggplot2] que contiene el formato e identidad gráfica
#'  de Morant Consultores
#' @field graficadas En desuso. Contiene un [ tibble] que indica qué variables ya han sido procesadas
#'  durante la producción.
#'
#' @export
Respuestas_svy <-
  R6::R6Class(
    classname = "Respuestas_svy",
    public = list(
      bd_respuestas = NULL,
      base = NULL,
      eliminadas = NULL,
      sin_coordenadas = NULL,
      no_efectivas = NULL,
      eliminadas_regla = NULL,
      #' @description Toma los datos de la base procesada y los va separando para su lectura.
      #' @param bd_respuestas Base de datos que carga los datos previamente procesados
      initialize = function(bd_respuestas){

        # Se carga la base
        self$base <- bd_respuestas

        #Se separan las eliminadas por auditoria
        self$obtener_eliminadas_auditorias()

        #Se separan las eliminadas por no ser efectivas
        self$obtener_eliminadas_no_efectivas()

        #Se separan las eliminadas por no tener coordenadas
        self$obtener_eliminadas_sin_coordenadas()

      },
      #' @description Visualiza las variables que aún no han sido producidas
      #'  por la paqutería durante la producción.
      obtener_eliminadas_auditorias = function(){
        self$eliminadas <- self$base |> filter(eliminada_auditoria == 1)
        self$base <- self$base |> filter(eliminada_auditoria == 0)
      },
      obtener_eliminadas_no_efectivas = function(){
        self$no_efectivas <- self$base |> filter(eliminada_proceso == 1 & razon == "No efectivas")
        self$base <- self$base |> filter(!(eliminada_proceso == 1 & razon == "No efectivas") )
      },
      obtener_eliminadas_sin_coordenadas = function(){
        self$sin_coordenadas <- self$base |> filter(eliminada_proceso == 1 & razon == "Falta de coordenadas")
        self$base <- self$base |> filter(!(eliminada_proceso == 1 & razon == "Falta de coordenadas") )
      },
      #' @description Agrega las variables relacionadas al diseno muestral
      #' @param muestra Contiene el diseño muestral completo de la encuesta.
      #' @param var_n var_n Valor tipo caracter que indica el nombre de la variable asociado al último
      #'  nivel de la etapa de muestreo.
      #' @param tipo_encuesta Campo de la clase Encuesta que determina el tipo de encuesta levantada.
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
                                    labels = c("18A24","25A39","40A59","60YMAS")),
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
