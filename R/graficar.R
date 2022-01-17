

#' Para graficar frecuencias simples
#'
#' @param bd Debe provenir de la función de analizar_frecuencias
#' @param titulo Es un parámetro obligatorio para el título de la gráfica
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
                          fill=respuesta))+
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


#' Para graficar frecuencias de barras agrupadas
#'
#' @param bd Debe provenir de la función PENDIENTE
#' @param titulo Es un parámetro obligatorio para el título de la gráfica
#' @param familia Familia tipográfica de los elementos del plot
#' @param color_etiqueta Color de la letra de las etiquetas de datos
#' @param nota Si de desea añadir una nota al pie del plot
#' @param colores Vector de colores del plot
#' @param orden Vector en el orden que se desean los grupos de las barras
#'
#' @return
#' @export
#'
#' @examples
graficar_barras_grupos <- function(bd, titulo,
                                   familia = "Poppins",
                                   color_etiqueta = "#3B3838",
                                   nota = "",
                                   orden = "",
                                   colores){

  transparencia <- .8
  ancho_barras <- .45


  bd %>%
    ggplot(aes(x  =forcats::fct_reorder(respuesta, media), fill = grupo, y =media,
               group =factor(grupo,
                             levels = orden)
    )) +
    geom_chicklet(stat = "identity", width = ancho_barras, alpha = transparencia)+
    geom_text(aes(label = scales::percent(media,accuracy = 1)), family = familia,
              position = position_stack(.5,reverse = T), vjust = .5,
              color = color_etiqueta) +
    scale_fill_manual(values = colores)+
    scale_y_continuous(labels=scales::percent_format(accuracy = 1))+
    coord_flip()+
    labs(title = titulo, x = NULL, y = "", fill = NULL)
}

#' Title
#'
#'  @param bd Debe provenir de la función PENDIENTE
#' @param titulo Es un parámetro obligatorio para el título de la gráfica
#' @param grupo1 vector: es el grupo que saldrá del lado derecho del plot
#' @param grupo2 vector: es el grupo que saldrá del lado izquierdo del plot
#' @param color1 Color de las barras del grupo 1
#' @param color2 Color de las barras del grupo 2
#'
#' @return
#' @export
#'
#' @examples
graficar_frecuencia_opuestos <- function(bd,titulo, grupo1, grupo2,
                                         color1= "#006466", color2= "#4d194d",
                                         contraste_etiqueta = T,
                                         color_etiqueta = "#FFFFFF"){
  transparencia <- .8
  ancho_barras <- .45

  aux <-bd  %>%
    mutate( media2 = case_when(respuesta %in% grupo1 ~media*-1, T~media),
            color = case_when(respuesta %in% grupo1 ~ color1, T~color2))

  aux %>%
    ggplot(aes(x = forcats::fct_reorder(respuesta, media2),
               y = media2, fill = color, color = NULL))+
    ggchicklet::geom_chicklet(stat = "identity",
                              width = ancho_barras, alpha = transparencia)+
    coord_flip()+
    scale_fill_identity()+
    labs(title = titulo, x = "", y = "", fill = "")+
    lemon::scale_y_symmetric(labels = function(x){
      scales::percent(abs(x), accuracy = 1)
    }  )+
    ggfittext::geom_bar_text(aes(label=media %>%  scales::percent(accuracy = 1)),
                             show.legend = F, color = color_etiqueta,
                             contrast = contraste_etiqueta, grow = T)+
    theme(legend.position = "none")
}

graficar_stack_frecuencias <- function(bd,   titulo= NULL,
                                       fill=NULL,
                                       nota = "",
                                       grupo_positivo = c("Aprueba mucho",
                                                          "Aprueba poco"),
                                       grupo_negativo = c("Desaprueba mucho",
                                                          "Desaprueba poco"),
                                       ns_nc = "Ns/Nc",
                                       colores =  c("Desaprueba mucho" = "#DE6400",
                                                    "Desaprueba poco" = "#FB8500",
                                                    "Aprueba poco" = "#126782",
                                                    "Aprueba mucho" = "#023047",
                                                    "Ns/Nc" = "gray")){

  transparencia <- .8
  ancho_barras <- .45
  familia <- "Poppins"
  color_etiqueta <- "#3B3838"



  aux  <-  bd %>%
    mutate(etiqueta = media,
           media = case_when(respuesta %in%grupo_negativo~media*-1,
                             respuesta %in% grupo_positivo~media,
                             respuesta == ns_nc~media+1))


  aux %>%  filter(respuesta != ns_nc) %>%
    ggplot(aes(x  =forcats::fct_reorder(aspecto, media), fill = respuesta, y =media,
               group =factor(respuesta, levels = orden) )) +
    geom_chicklet(stat = "identity", width = ancho_barras, alpha = transparencia)+
    geom_text(aes(label = scales::percent(media,accuracy = 1)), family = familia,
              position = position_stack(.5,reverse = T), vjust = .5,
              color = color_etiqueta) +
    scale_fill_manual(values = colores)+
    scale_y_continuous(labels=scales::percent_format(accuracy = 1))+
    coord_flip()+
    labs(title = titulo, x = NULL, y = "", fill = NULL)+
    geom_segment(data=aux %>%  filter(respuesta== ns_nc),
                 mapping=aes(x = aspecto, y = 1,
                             xend=aspecto, yend= media), size = 10,
                 color = "gray")+
    geom_text(data=aux %>%  filter(respuesta== ns_nc),
              aes(label = scales::percent(etiqueta,accuracy = 1)),
              hjust = 'outside', nudge_x = 0, nudge_y = .025)+
    tema()

}

