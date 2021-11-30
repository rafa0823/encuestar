

#' Title
#'
#' @param bd debe provenir de la función de analizar_frecuencias
#' @param titulo Es un parámetro obligatorio para
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
graficar_barras_frecuencia <- function(bd,
                                       titulo,
                                       fill=NULL,
                                       nota = "",
                                       colores){
  g <-  bd %>% ggplot(aes(x = forcats::fct_reorder(stringr::str_wrap(respuesta,40),
                                                   media),
                          y  = media,
                          fill="1"))+
    ggchicklet::geom_chicklet(radius = grid::unit(3, "pt"),
                              alpha= .8,
                              width =.45)+
    labs(title = titulo,
         x = NULL,
         y = NULL,
         caption = nota)+
    coord_flip()+
    scale_y_continuous(labels=scales::percent_format(accuracy = 1))+
    ggfittext::geom_bar_text(aes(label=scales::percent(media, accuracy = 1)),contrast = T)
  return(g)

}
