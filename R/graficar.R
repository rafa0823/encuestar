

#' Para graficar frecuencias simples
#'
#' @param bd Debe provenir de la función de analizar_frecuencias
#' @param titulo Es un parámetro obligatorio para el título de la gráfica
#' @param tamano_letra Tamaño de letra de los elementos del plot
#' @param tamano_titulo Tamaño de letra solamente del título
#' @param color_titulo Color de la letra del título
#' @param familia Familia tipográfica de los elementos del plot
#' @param color_etiqueta Color de la letra de las etiquetas de datos
#' @param nota Si de desea añadir una nota al pie del plot
#' @param colores Vector de colores del plot
#'
#' @return
#' @export
#'
#' @examples
graficar_barras_frecuencia <- function(bd,titulo, tamano_letra = 18,
                                       tamano_titulo = 20,
                                       color_titulo = "black",
                                       familia = "Poppins",
                                       color_etiqueta = "#3B3838",
                                       nota = "",
                                       colores){
  transparencia <- .8
  ancho_barras <- .45


  g <-  bd %>% ggplot(aes(x = forcats::fct_reorder(stringr::str_wrap(respuesta,40), media),
                          y  = media, fill =respuesta))+
    ggchicklet::geom_chicklet(radius = grid::unit(3, "pt"),
                              alpha= transparencia, width =ancho_barras )+
    labs(title = titulo, x = NULL, y = NULL, caption = nota)+
    coord_flip()+
    scale_y_continuous(labels=scales::percent_format(accuracy = 1))+
    scale_fill_manual(values = colores)+
    geom_text(aes(label=scales::percent(media, accuracy = 1)),
              position=position_dodge(width=0.9),
              vjust=0.5)
  return(g)

}


#' Para graficar frecuencias de barras agrupadas
#'
#' @param bd Debe provenir de la función PENDIENTE
#' @param titulo Es un parámetro obligatorio para el título de la gráfica
#' @param tamano_letra Tamaño de letra de los elementos del plot
#' @param tamano_titulo Tamaño de letra solamente del título
#' @param color_titulo Color de la letra del título
#' @param familia Familia tipográfica de los elementos del plot
#' @param color_etiqueta Color de la letra de las etiquetas de datos
#' @param nota Si de desea añadir una nota al pie del plot
#' @param colores Vector de colores del plot
#'
#' @return
#' @export
#'
#' @examples
graficar_barras_grupos <- function(bd, titulo, tamano_letra = 18,
                                   tamano_titulo = 20,
                                   color_titulo = "black",
                                   familia = "Poppins",
                                   color_etiqueta = "#3B3838",
                                   nota = "",
                                   colores){

  transparencia <- .8
  ancho_barras <- .45
  # verde <- "#7FA389"
  # rojo <- "#D67278"
  # gris <- "gray"
  # amarillo <- "#F0A96C"
  # rojo_f <- "#D14B4B"
  # verde_f <- "#518265"

  # c("Muy mala" = rojo_f,
  #   "Mala" = rojo,
  #   "Regular" = amarillo,
  #   "Buena" = verde,
  #   "Muy buena" = verde_f,
  #   "Ns/Nc" = gris)

  bd %>%
    ggplot(aes(x  = factor(respuesta, media), fill = grupo, y =media,
               group = grupo, levels = c("Muy mala",
                                         "Mala",
                                         "Regular",
                                         "Buena",
                                         "Muy buena",
                                         "Ns/Nc"))) +
    geom_chicklet(stat = "identity", width = .5)+
    geom_text(aes(label = scales::percent({{n}},accuracy = 1)), family = familia,
              position = position_stack(.5,reverse = T), vjust = .5,
              color = color_etiqueta) +
    scale_fill_manual(values = colores)+
    scale_y_continuous(labels=scales::percent_format(accuracy = 1))+
    coord_flip()+
    labs(title = titulo, x = NULL, y = "", fill = NULL)+
    theme_minimal()+
    theme(text = element_text(family = familia, size = tamano_letra),
          plot.title = element_text(size = tamano_titulo,
                                    colour = color_titulo,
                                    hjust = 0),
          legend.position = "bottom",
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank())
}





