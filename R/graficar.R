

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
