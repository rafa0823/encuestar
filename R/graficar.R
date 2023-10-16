if(getRversion() >= "2.15.1")  utils::globalVariables(c("grupo"))

#' Gráfica de barras horizontales ordenadas
#'
#' @param bd Base de datos con una variable categórica (respuesta) y una numérica (media).
#' @param salto Número entero, se aplica un stringr::str_wrap a la variable categórica.
#' @param porcentajes_fuera Si es T, las labels de los porcentajes aparecen fuera (o sobre) las barras.
#' @param desplazar_porcentajes Si porcentajes_fuera es T, este parametro ajusta las etiquetas de texto.
#'
#' @return
#' @export
#'
#' @examples
#' graficar_barras(bd, salto = 13)
#' graficar_barras(bd, porcentajes_fuera = T, desplazar_porcentajes = 0.1)

graficar_barras <- function(bd,
                            salto = 20,
                            porcentajes_fuera = F,
                            desplazar_porcentajes = 0){

  g <-  bd %>%
    ggplot(aes(x = forcats::fct_reorder(stringr::str_wrap(respuesta, salto), media),
               y  = media,
               fill=respuesta)) +
    ggchicklet::geom_chicklet(radius = grid::unit(3, "pt"), alpha= .8, width = .45)

  if (porcentajes_fuera == F) {

    g <- g +
      ggfittext::geom_bar_text(aes(label=scales::percent(media, accuracy = 1)), contrast = T)

  }

  if (porcentajes_fuera == T) {

    g <- g +
      geom_text(aes(label=scales::percent(media, accuracy = 1)), nudge_y = desplazar_porcentajes)

  }

  g <- g +
    coord_flip() +
    labs(x = NULL, y = NULL) +
    scale_y_continuous(labels=scales::percent_format(accuracy = 1)) +
    theme(legend.position = "none")

  return(g)

}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("media2","color"))

#' Para graficar frecuencias de barras agrupadas
#'
#' @param bd  Base de datos (ya procesada)
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
    ggchicklet::geom_chicklet(stat = "identity", width = ancho_barras, alpha = transparencia)+
    geom_text(aes(label = scales::percent(media,accuracy = 1)), family = familia,
              position = position_stack(.5,reverse = T), vjust = .5,
              color = color_etiqueta) +
    scale_fill_manual(values = colores)+
    scale_y_continuous(labels=scales::percent_format(accuracy = 1))+
    coord_flip()+
    labs(title = titulo, x = NULL, y = "", fill = NULL)
}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("orden", "etiqueta"))

#' Title
#'
#' @param bd  Base de datos (ya procesada)
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

if(getRversion() >= "2.15.1")  utils::globalVariables(c("saldo","color_saldo","pct","strata_1"))

#' Title
#'
#' @param bd  Base de datos (ya procesada)
#' @param titulo Título de la gráfica
#' @param fill Variable de color
#' @param nota Nota opcional
#' @param grupo_positivo Grupo cuyas barras serán positivas
#' @param grupo_negativo Grupo que su porcentaje será negativo
#' @param ns_nc Nombre de las respuestas que corresponden a "No sabe", default: "Ns/Nc"
#' @param colores Vector de colores con el nombre de las variables
#' @param orden Orden de la variable de color
#' @param familia font family
#'
#' @return
#' @export
#'
#' @examples

graficar_aspectos_frecuencias <- function(bd,
                                          titulo= NULL,
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
                                                       "Ns/Nc" = "gray"),
                                          familia){

  transparencia <- .8
  ancho_barras <- .45
  color_etiqueta <- "#3B3838"

  orden <- c(grupo_negativo, grupo_positivo)

  aux  <-  bd %>%
    mutate(etiqueta = media,
           media = case_when(respuesta %in%grupo_negativo~media*-1,
                             respuesta %in% grupo_positivo~media,
                             respuesta == ns_nc~media+1.01),
           media2 = case_when(respuesta == ns_nc~0, T~media)) %>%
    group_by(tema) %>%  mutate(saldo=sum(media2, na.rm = T),
                               color_saldo = case_when(saldo>0~"#fb8500", saldo<0~"#126782",
                                                       saldo ==0 ~"gray")) %>%  ungroup()


  aux %>%  filter(respuesta != ns_nc) %>%
    ggplot(aes(x  =forcats::fct_reorder(tema, saldo), fill = respuesta, y =media,
               group =factor(respuesta, levels = orden) )) +
    ggchicklet::geom_chicklet(stat = "identity", width = ancho_barras, alpha = transparencia)+
    geom_text(aes(label = scales::percent(media,accuracy = 1)), family = familia,
              color = "white", fontface = "bold",
              position = position_stack(.5,reverse = T), vjust = .5,
              color = color_etiqueta) +
    scale_fill_manual(values = colores)+
    scale_y_continuous(labels=scales::percent_format(accuracy = 1))+
    coord_flip()+
    labs(title = titulo, x = NULL, y = "", fill = NULL)+
    geom_segment(data=aux %>%  filter(respuesta== ns_nc),
                 mapping=aes(x = tema, y = 1,
                             xend=tema, yend= media), size = 10,
                 color = "gray")+
    # geom_segment(aes(y =0, yend = 1), size = 1)+
    geom_text(data=aux %>%  filter(respuesta== ns_nc),
              aes(label = scales::percent(etiqueta,accuracy = 1)),
              hjust = 'outside', nudge_x = 0, nudge_y = .025, family = familia)+
    geom_text(data =  aux %>% filter(respuesta == ns_nc),
              aes(color = color_saldo, y=saldo,
                  label =paste("Saldo: ",scales::percent(saldo, accuracy = 1))),
              family = familia, size = 3.5, hjust = .5,nudge_x = .3,  nudge_y =-0.08, show.legend = F)+
    # geom_point(data =  aux %>% filter(respuesta == ns_nc), shape = 18, size = 3,
    #            aes(x = tema, y=saldo, color = color_saldo), show.legend = F, position = position_nudge(x =.3))+
    # geom_text(aes(label = scales::percent(media,accuracy = 1)), family = familia,
    #           position = position_stack(.7,reverse = T),
    #           color = color_etiqueta) +
    geom_hline(yintercept = 0, color = "#FFFFFF", size= .6)+
    geom_hline(yintercept = 0, color = "gray", size= .6)+
    geom_hline(yintercept = 1, color = "#FFFFFF", size = 1.2)+
    geom_hline(yintercept = 1, color = "#323232", linetype = "dotted", size = .7)

}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("familia"))

#' Graficar gauge
#'
#' @param bd  Base de datos con un único row. La variable a considerar se llama "media"
#' @param color_principal Color principal de la barra
#' @param color_secundario Color secundario de la barra
#' @param escala Máximo y mínimo de los valores que puede tomar "media"
#' @param size_text_pct Tamaño del texto dentro del gauge
#'
#' @return
#' @export
#'
#' @examples
#' graficar_gauge_promedio(bd = bd_procesada, color_principal = "red", escala = c(0, 10), size_text_pct = 8)
#' graficar_gauge_promedio(bd = bd_procesada, color_principal = "pink", color_secundario = "brown", escala = c(1, 7), size_text_pct = 14)

graficar_gauge <- function(bd, color_principal, color_secundario = "gray80", escala, size_text_pct){

  g <- bd %>%
    ggplot() +
    geom_rect(aes(xmin = 2, xmax = 3, ymin = 0, ymax = media),
              fill = color_principal,  color = "white", alpha= .95) +
    geom_rect(aes(xmin = 2, xmax = 3, ymin = media, ymax = escala[2]),
              fill = color_secundario, color = "white")

  if(escala[2] == 1) {

    g <- g + geom_text(aes(x = 0, y = media, label = scales::percent(x = media, accuracy = 1.)),
                       size = size_text_pct, family = "Poppins", nudge_y = 0.25)

  }
  else {

    g <- g + geom_text(aes(x = 0, y = media, label = scales::comma(x = media, accuracy = 1.1)),
                       size = size_text_pct, family = "Poppins", nudge_y = 0.25)

  }

  g <- g +
    scale_fill_manual(values = c("#1DCDBC", "#38C6F4")) +
    scale_x_continuous(limits = c(0, NA)) +
    scale_y_continuous(limits = c(0, escala[2])) +
    xlab("") +
    ylab("") +
    coord_polar(theta = "y") +
    theme_void() +
    theme(legend.position = "bottom", axis.text = element_blank(),
          text = element_text(size = 15, family = "Poppins"))

  return(g)

}

sustituir <- function(bd, patron, reemplazo = ""){
  bd %>% mutate(respuesta = gsub(pattern = patron, replacement = reemplazo,
                                 x = respuesta, fixed = T))
}

#' Title
#'
#' @param bd  Base de datos (ya procesada)
#'
#' @return
#' @export
#'
#' @examples
graficar_barras_numerica<- function(bd){
  bd %>%
    ggplot(aes(y = media, x = stats::reorder(str_wrap(tema,40),media))) +
    ggchicklet::geom_chicklet(radius = grid::unit(3, "pt"),
                              alpha= .95, fill = "#850D2D",
                              width =.45)+ coord_flip()+
    labs(title = NULL,
         x = NULL,
         y = "Promedio")+
    ggfittext::geom_bar_text(aes(label= round(media,digits = 1)),
                             contrast = T)
}

#' Graficar intervalos numérica
#'
#' @param bd Base de datos con una variable categórica (respuesta) y una numérica (media).
#' @param point_size Tamaño del punto que indica el promedio de la estimación.
#' @param text_point_size Tamaño del texto que acompaña el valor de la estimación
#'
#' @return
#' @export
#'
#' @examples
#' graficar_intervalo_numerica(bd = bd, point_size = 2, text_point_size = 14)
graficar_intervalo_numerica <- function(bd, escala = c(0, 1), point_size = 1, text_point_size = 8){

  bd %>%
    ggplot(aes(y = media, x = stats::reorder(str_wrap(tema,40), media))) +
    geom_pointrange(aes(ymin = inf, ymax = sup), color = "#850D2D", size = point_size) +
    coord_flip() +
    labs(title = NULL,
         x = NULL,
         y = "Promedio")+
    geom_text(aes(label = round(media,digits = 2)), nudge_x = .3, size = text_point_size) +
    scale_y_continuous(limits = c(escala[1], escala[2]))

}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("palabras","pregunta","titulo","nota"))

#' Title
#'
#' @param bd  Base de datos (ya procesada)
#' @param pregunta columna de caracter a procesar
#' @param n Número de palabras a mostrar
#'
#' @return
#' @export
#'
#' @examples

graficar_barras_palabras <- function(bd, pregunta, nota, tit, n = 10){
  bd %>% tidytext::unnest_tokens(palabras, pregunta) %>%
    count(palabras,sort = T) %>%
    anti_join(tibble(palabras = c(stopwords::stopwords("es"),"ns","nc", "no", "leer", "Ns/Nc"))) %>%
    head(n) %>%
    ggplot(aes(x = forcats::fct_reorder(stringr::str_to_title(palabras), n), y = n))+
    ggchicklet::geom_chicklet(radius = grid::unit(3, "pt"),
                              alpha= .8,
                              width =.45,
                              fill = "#871938")+
    theme_minimal()+
    labs(title = tit,
         x = NULL,
         y = NULL,
         caption = nota)+
    coord_flip()+
    ggfittext::geom_bar_text(aes(label=n),contrast = T)
}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("colores"))

#' Title
#'
#' @param bd  Base de datos (ya procesada)
#' @param pregunta
#' @param n
#' @param color1
#' @param color2
#' @param color3
#' @param familia
#' @param ancho
#' @param alto
#'
#' @return
#' @export
#'
#' @examples

graficar_nube_frecuencias <- function(bd,pregunta, n = 100,
                                      color1 = "#5B0A1C", color2 = "#850D2D", color3 = "#961B41",
                                      familia = "Poppins",
                                      ancho = 600*5, alto =  600*5/1.41){
  plot <- bd %>% tidytext::unnest_tokens(palabras,{{pregunta}}) %>% count(palabras,sort = T) %>%
    anti_join(tibble(palabras = c(stopwords::stopwords("es"),"ns","nc"))) %>%
    mutate(colores = case_when(
      n<=quantile(n,probs=.75)~ color3,
      n>quantile(n,probs=.75) & n<=quantile(n,probs=.90)~color2,
      n>quantile(n,probs=.90)~ color1,
      T~NA_character_),
    ) %>%
    slice(1:n) %>%
    hchart(hcaes(x= palabras, weight =log(n),
                 color=colores), type= "wordcloud") %>%
    hc_chart(style=list(fontFamily = familia))



  # htmlwidgets::saveWidget(widget = plot, file = paste0("plot",".html"))
  # webshot::webshot(url = paste0("plot",".html"),
  #         file = "Entregables/plot.png",vwidth = ancho, vheight = alto,
  #         delay=3) # delay will ensure that the whole plot appears in the image
  # grDevices::dev.off()
  return(plot)

}



#' Title
#'
#' @param base_size
#' @param base_family
#' @param fondo
#'
#' @return
#' @export
#'
#' @examples
tema_default <- function(base_size = 15, base_family = "Poppins", fondo="#FFFFFF") {
  (ggthemes::theme_foundation(base_size = base_size,
                              base_family = base_family)
   + theme(
     line = element_line(colour = "#4C5B61"),
     rect = element_rect(fill = fondo,
                         linetype = 0,
                         colour = NA),
     text = element_text(color = "#2C423F"),
     axis.title = element_blank(),
     axis.text = element_text(),
     axis.ticks = element_blank(),
     axis.line.x = element_line(colour = "#E1356D"),
     legend.background = element_rect(),
     legend.position = "bottom",
     legend.direction = "horizontal",
     legend.box = "vertical",
     panel.grid = element_line(colour = NULL),
     panel.grid.major.y = element_blank(),
     panel.grid.major.x =element_line(colour = "#C5C5C5",
                                      linetype = "dotted"),
     panel.grid.minor = element_blank(),
     plot.title = element_text(hjust = 0,
                               size = rel(1.1),
                               # face = "bold",
                               colour = "#4C5B61"),
     plot.subtitle = element_text(hjust = 0,
                                  size = rel(1),
                                  face = "bold",
                                  colour = "#C5C5C5"),
     plot.margin = unit(c(1, 1, 1, 1), "lines"),
     strip.text=element_text(colour ="#2C423F")))
}


#' Title
#'

#' @param bd  Base de datos (ya procesada)
#' @param tipo
#' @param titulo
#' @param nota
#' @param familia
#'
#' @return
#' @export
#'
#' @examples
graficar_estratos_frecuencia <- function(bd,
                                         tipo,
                                         titulo = NULL,
                                         nota = "", familia = "Poppins"){
  if(tipo == "barras"){
    #Agrupada por estratos
    plot <-bd %>%
      ggplot(aes(x =forcats::fct_reorder(respuesta,pct), y = pct,
                 fill = respuesta)) +
      ggchicklet::geom_chicklet(width =.5, alpha = .85)+
      coord_flip()+
      scale_y_continuous(labels=scales::percent_format(accuracy = 1))+
      theme_minimal()+
      theme(panel.grid.minor = element_blank(),
            text = element_text(family = familia),
            legend.position = "none")+
      ggfittext::geom_bar_text(aes(label=scales::percent(pct, accuracy = 1)),contrast = T,family = familia )+
      labs(title = titulo, x = "Estrato", y = NULL, fill = NULL)+
      facet_wrap(~strata_1)
  }
  if(tipo == "lineas") {
    plot <-bd %>% mutate(etiqueta = round(pct*100)) %>%
      ggplot(aes(x = strata_1, y = pct, group = respuesta,
                 color = respuesta, label = paste0(etiqueta, "%"))) +
      geom_line(size = 3.5, show.legend = F)+
      geom_point(size = 5)+
      scale_y_continuous(labels=scales::percent_format(accuracy = 1))+
      geom_text(hjust=-.1, vjust=-.8, show.legend = F, check_overlap = T)+
      theme_minimal()+
      theme(panel.grid.minor = element_blank(),
            text = element_text(family = familia),
            legend.position = "bottom")+
      labs(title = titulo, x = "Estrato", y = NULL, color = NULL)

  }


  return(plot)
}

graficar_estratos_likert <- function(bd, titulo = NULL,
                                     grupo_positivo = c("Aprueba mucho",
                                                        "Aprueba poco"),
                                     grupo_negativo = c("Desaprueba mucho",
                                                        "Desaprueba poco"),
                                     nombre_positivo = "Aprueba",
                                     nombre_negativo = "Desaprueba",
                                     color_positivo =  "#023047",
                                     color_negativo = "#DE6400",
                                     etiqueta_x = "Aspecto",
                                     familia = "Poppins"){
  bd %>%  mutate(grupo = case_when(respuesta %in% grupo_positivo~ nombre_positivo,
                                   respuesta %in% grupo_negativo~ nombre_negativo),
                 pct = case_when(grupo == nombre_positivo ~pct,
                                 grupo == nombre_negativo~pct*-1)) %>%
    filter(!is.na(grupo)) %>%
    group_by(strata_1, grupo) %>%  summarise(saldo =sum(pct)) %>%
    ggplot(aes(x = forcats::fct_reorder(strata_1, saldo), y = saldo, fill = grupo))+
    ggchicklet::geom_chicklet(width =.5, alpha = .85)+
    coord_flip()+
    geom_text(aes(label = scales::percent(saldo,accuracy = 1)),
              position = position_stack(.8,reverse = T))+
    scale_fill_manual(values = purrr::set_names(c(color_negativo, color_positivo), c(nombre_negativo, nombre_positivo)))+
    theme_minimal()+
    scale_y_continuous(labels=scales::percent_format(accuracy = 1))+
    theme(panel.grid.minor = element_blank(),
          text = element_text(family = familia),
          panel.grid.major.y = element_blank(),
          legend.position = "bottom")+
    labs(titulo = titulo, x = "Estrato", y = "Saldo", fill = NULL)

}



#' Title
#'
#' @param bd Base de datos (ya procesada)
#' @param titulo
#' @param nota
#' @param familia
#'
#' @return
#' @export
#'
#' @examples
graficar_estratos_grupos <- function(bd, titulo = "", nota = "", familia = "Poppins"
){

  bd %>% ggplot(aes(x = forcats::fct_reorder(grupo,pct), y = pct, fill = grupo) )+
    ggchicklet::geom_chicklet(alpha = .8, width = .7)+
    coord_flip()+
    facet_wrap(~strata_1)+
    coord_flip()+
    scale_y_continuous(labels=scales::percent_format(accuracy = 1))+
    ggfittext::geom_bar_text(aes(label=scales::percent(pct, accuracy = 1)),contrast = T,family = familia )+
    theme_minimal()+
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          text = element_text(family = familia),
          legend.position = "none")+
    labs(title = titulo, x = NULL, y = NULL, fill = NULL)+
    facet_wrap(~strata_1)
}

graficar_estratos_aspectos <- function(bd, titulo = NULL,
                                       grupo_positivo = c("Aprueba mucho",
                                                          "Aprueba poco"),
                                       grupo_negativo = c("Desaprueba mucho",
                                                          "Desaprueba poco"),
                                       nombre_positivo = "Aprueba",
                                       nombre_negativo = "Desaprueba",
                                       color_positivo =  "#023047",
                                       color_negativo = "#DE6400",
                                       etiqueta_x = "Aspecto",
                                       familia = "Poppins"){
  bd %>% mutate(valor = case_when(respuesta %in% grupo_positivo~ nombre_positivo,
                                  respuesta %in% grupo_negativo~ nombre_negativo),
                pct2 = case_when(valor == nombre_positivo~pct, valor == nombre_negativo~pct*-1)) %>%
    filter(!is.na(valor)) %>%
    group_by(strata_1, grupo, valor) %>%  summarise(saldo =sum(pct2)) %>%
    ggplot(aes(x =forcats::fct_reorder(grupo, saldo),  y= saldo, fill = valor))+
    geom_chicklet(width =.4, alpha =.9)+
    coord_flip()+
    geom_text(aes(label = scales::percent(saldo,accuracy = 1)),
              position = position_stack(.8,reverse = T))+
    facet_wrap(~strata_1)+
    scale_fill_manual(values = purrr::set_names(c(color_negativo, color_positivo), c(nombre_negativo, nombre_positivo)))+
    theme_minimal()+
    scale_y_continuous(labels=scales::percent_format(accuracy = 1)) %+replace%
    theme(panel.grid.minor = element_blank(),
          text = element_text(family = familia),
          panel.grid.major.y = element_blank(),
          legend.position = "bottom")+
    labs(title = titulo, fill = NULL, x = etiqueta_x)
}


#' Graficar candidato opinión
#'
#' @param bd Base de datos con estructura producida por analizar_frecuencias_aspectos.
#' @param ns_nc Valor de la variable 'respuesta' asociado a "No sabe" o "No contesta" en la pregunta relativa.
#' @param regular Valor de la variable 'respuesta' asociado a "Regular" en la pregunta relativa.
#' @param grupo_positivo Conjunto de valores de la  variable 'respuesta' considerados como positivos.
#' @param grupo_negativo Conjunto de valores de la  variable 'respuesta' considerados como negativos
#' @param colores Vector ordenado de colores asociados al grupo negativo, regular y positivo.
#' @param burbuja Base de datos con estructura producida por analizar_frecuencias_aspectos filtrada por un valor de interés. Disponible en el enviroment.
#' @param color_burbuja Color de los puntos asociados a la base de datos 'burbuja'
#' @param caption_opinion Caption de la gráfica que visualiza los datos de bd
#' @param caption_nsnc Caption de la gráfica que visualiza los datos de bd separando el valor asociado a "No sabe" o "No contesta" en la pregunta relativa.
#' @param caption_burbuja Caption de la gráfica que visualiza los datos de burbuja
#' @param size_caption_opinion Tamaño del texto del caption_opinion
#' @param size_caption_nsnc Tamaño del texto del caption_nsnc
#' @param size_caption_burbuja Tamaño del texto del caption_burbuja
#' @param size_text_cat Tamaño del texto de las categorías diferentes categorías de la variable 'tema' de la base de datos 'bd'
#' @param orden_resp Vector ordenado de los posibles valores de la variable 'respuesta'
#' @param salto Parámetro usado por la función str_wrap de la paquetería stringr aplicado a la variable 'tema' de la base 'bd'
#' @param tema Tema de la gráfica asociado a la paquetería 'encuestar'
#'
#' @return
#' @export
#'
#' @examples
#' graficar_candidato_opinion(bd, ns_nc = "Ns/Nc", regular = "Regular", grupo_positivo = "Buena", grupo_negativo = "Mala", colores = c("red", "yellow", "green", "gray70"), burbuja = burbuja, color_burbuja = "blue", caption_opinion = "", caption_nsnc = "Ns/Nc", caption_burbuja = "Nivel de conocimiento", size_caption_opinion = 12, size_caption_nsnc = 12, size_caption_burbuja = 12, size_caption_cat = 12, orden_resp = c("Mala", "Regular", "Buena"), tema = self$tema)
graficar_candidato_opinion <- function(bd, ns_nc, regular,
                                       grupo_positivo,
                                       grupo_negativo,
                                       colores,
                                       burbuja,
                                       color_burbuja,
                                       caption_opinion,
                                       caption_nsnc,
                                       caption_burbuja,
                                       size_caption_opinion,
                                       size_caption_nsnc,
                                       size_caption_burbuja,
                                       size_text_cat,
                                       orden_resp,
                                       salto = 200,
                                       tema){

  if(!is.null(ns_nc)){
    bd <- bd %>% group_by(tema) %>% complete(respuesta = ns_nc, fill = list(media = 0)) %>% ungroup
  }

  aux <- bd %>% mutate(Regular = if_else(respuesta == regular, "regular1", as.character(respuesta))) %>%
    bind_rows(bd %>% filter(respuesta == regular) %>% mutate(Regular = "regular2", media = -media)) %>%
    mutate(etiqueta = if_else(Regular != "regular2", scales::percent(media,1), ""),
           media = if_else(respuesta %in% grupo_negativo,-1*media,media),
           media = if_else(respuesta == regular, media/2, media)) %>%
    # mutate(Regular =factor(Regular, levels = c(grupo_negativo, "regular1", "regular2",grupo_positivo))) %>%
    group_by(tema) %>%
    mutate(saldo = sum(as.numeric(!(respuesta %in% c(regular, ns_nc)))*media))

  orden <- aux %>% arrange(saldo) %>% pull(tema) %>% unique %>% na.omit

  if(!all(is.na(burbuja))){
    burbuja <- burbuja %>% mutate(escala = media/max(media), tema = forcats::fct_reorder(tema, media))
    orden <- burbuja$tema %>% levels
    a.1 <-  burbuja %>%
      ggplot(aes(y = tema,
                 x = factor(1))) + geom_point(aes(size = escala), color = color_burbuja, shape = 16) +
      geom_text(aes(label = scales::percent(media,1)),hjust = -.5) +
      tema() +
      labs(caption = caption_burbuja) +
      theme(legend.position = "none", panel.grid.major.x = element_blank(),
            axis.text = element_blank(), axis.line.x = element_blank(),
            plot.caption = element_text(hjust = 0.5, size = size_caption_burbuja))
  }

  a <- aux %>%
    {if(!is.null(ns_nc)) filter(., respuesta!= ns_nc) else .}  %>%
    mutate(respuesta = factor(respuesta, levels = orden_resp)) |>
    ggplot(aes(x  = factor(tema, orden), fill = respuesta,
               group = factor(Regular, levels = c( "regular2", grupo_negativo, "regular1", grupo_positivo)), y =media)) +
    ggchicklet::geom_chicklet(width =.6, alpha =.9)+
    scale_fill_manual(values = colores)+
    ggfittext::geom_fit_text(aes(label = etiqueta), family = tema()$text$family,
                             min.size = 8,
                             position = position_stack(.5,reverse = T), vjust = .5, contrast = T, show.legend = F) +
    coord_flip()+
    labs(fill= NULL , y= NULL, x = NULL, caption = caption_opinion) +
    theme_minimal() +
    geom_hline(yintercept = 0, color = "#FFFFFF", size= .6)+
    geom_hline(yintercept = 0, color = "gray", size= .6)+
    lemon::scale_y_symmetric(labels = function(x) scales::percent(abs(x), accuracy = 1)) +
    theme(legend.position = "bottom") %+replace% tema() +
    theme(axis.text.y = element_text(size = size_text_cat),
          plot.caption = element_text(hjust = 0.5, size = size_caption_nsnc), legend.key.size = unit(1, units = "cm")) +
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = salto))

  if(!is.null(ns_nc)){
    b <- aux %>%  filter(respuesta == ns_nc) %>%
      ggplot(aes(x = factor(tema, orden), y = media))+
      ggchicklet::geom_chicklet(width =.6, alpha =.9, fill = colores[4])+
      coord_flip()+
      ggfittext::geom_bar_text(aes(label = etiqueta), family = tema()$text$family,
                               hjust = -.1)+
      labs(y = NULL, x = NULL, caption = caption_nsnc)+
      scale_y_continuous(n.breaks = 2) +
      tema() +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            axis.text.x = element_blank(), axis.line.x = element_blank(),
            plot.caption = element_text(hjust = 0.5, size = size_caption_nsnc))

    if(!all(is.na(burbuja))){
      final <-a + a.1 + b + plot_layout(widths = c(.7,.15,.15), ncol= 3)
    } else{
      final <-a + b + plot_layout(widths = c(.8, .2))
    }

  } else{
    if(!all(is.na(burbuja))){
      final <- a + a.1 + plot_layout(widths = c(.8,.2))
    } else{
      final <- a
    }
  }

  return(final)
}


#' Graficar el partido político con el que asocian a uno o varios personajes
#'
#' @param bases Lista. 'bases$conoce' contiene la estimación del conocimiento sobre algún personaje. 'bases$partido' tiene la información sobre la asociación a un partido político por personaje
#' @param cliente Vector de códigos cortos que asocian a un personaje con una variable. Por ejemplo, 'personajeC' resaltará entre el conjunto 'personajeA', 'personajeB', 'personajeC'.
#' @param tipo_conoce Tipo de gráfica a mostrar en la estimación de conocimiento. De forma predeterminada son barras, el otro valor son 'intervalos'
#' @param colores_candidato Vector que asigna un color a un candidato de acuerdo al nombre largo (tema)
#' @param solo_respondidos Logical. Omite los partidos en los cuales el personaje no tiene ninguna asociación
#' @param colores_partido Vector que asigna un color a cada partido de acuerdo al nombre largo (tema)
#' @param tema Tema de la gráfica asociado a la paquetería 'encuestar'
#'
#' @return
#' @export
#'
#' @examples
#' graficar_candidato_partido(bases, clientes = c("era", "sasil"), tipo_conoce = "intervalos", colores_candidato = colores_candidato, colores_partido = colores_partido, tema = self$tema)
graficar_candidatoPartido <- function(bases, cliente, tipo_conoce, colores_candidato, solo_respondidos = T, colores_partido, tema){

  bases$conoce <- bases$conoce %>%
    mutate(tema = forcats::fct_reorder(tema, media, min))
  if(tipo_conoce == "intervalos"){
    a <- bases$conoce %>% ggplot(aes(tema, media, ymin = inf, ymax = sup, color = tema)) +
      geom_pointrange(show.legend = F) +
      geom_text(aes(label = scales::percent(media,1)), vjust = 0, nudge_x = .3, show.legend = F) +
      scale_color_manual(values = colores_candidato) +
      labs(title = "Conocimiento", y = NULL,x = NULL ) +
      coord_flip() +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0,max(bases$conoce$media), by = .1)) +
      tema()
  } else{
    a <- bases$conoce %>% ggplot(aes(x = tema, y = media, fill = tema)) +
      # geom_col(show.legend = F) +
      ggchicklet::geom_chicklet(width = .6, alpha =.5, show.legend = F)+
      ggfittext::geom_bar_text(aes(label = scales::percent(media,1))) +
      scale_fill_manual(values = colores_candidato) +
      labs(title = "Conocimiento", y = NULL,x = NULL ) +
      coord_flip() +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0,max(bases$conoce$media), by = .1)) +
      tema()
  }


  bases$partido <- bases$partido %>%
    mutate(tema = factor(tema,levels(bases$conoce$tema)))

  opciones <- bases$partido %>% pull(respuesta) %>% unique %>% as.character()
  orden <- match(opciones, names(colores_partido))

  if(sum(is.na(orden))>0){
    stop(glue::glue("Por favor indique colores para: {paste(opciones[is.na(orden)], collapse = ', ')}"))
  }

  if(solo_respondidos){

    colores_partido_filter <- colores_partido[!is.na(match(names(colores_partido), opciones))]

    if(length(colores_partido_filter) < length(colores_partido)){
      warning(glue::glue("Las siguientes respuestas se omitirán: {paste(names(colores_partido)[is.na(match(names(colores_partido), opciones))], collapse = ', ')}"))
    }
    colores_partido <- colores_partido_filter
  }

  b <- bases$partido %>%
    ggplot() +
    geom_rect(aes(xmin = inf, xmax = sup,
                  y = tema,
                  ymin = as.numeric(tema) - .3,
                  ymax = as.numeric(tema) + .3,
                  fill = respuesta)) +
    geom_text(data = bases$partido %>% filter(tema %in% cliente),
              aes(x = label, y = as.numeric(tema), label = scales::percent(media,accuracy = 1)),
              color = "white", fontface = "bold") +
    geom_text(data = bases$partido %>% filter(respuesta %in% c("MORENA", "Ns/Nc")),
              aes(x = label, y = as.numeric(tema), label = scales::percent(media,accuracy = 1)),
              color = "white", fontface = "bold") +
    scale_fill_manual(values = colores_partido) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
    # geom_text(aes(x = 0, y = as.numeric(tema), label = tema), hjust = 0) +
    labs( y = "", title = "Identificación partidista", x= NULL) +
    tema() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank())

  a + b + patchwork::plot_layout(widths = c(.2,.8))

}

#' Graficar saldo de opinión por personaje
#'
#' @param bd Base de datos resultado de la función 'calcular_saldoOpinion'.
#' @param grupo_positivo Valores posibles dentro de la variable 'grupo' que los identifica de manera arbitraria como positivos.
#' @param grupo_negativo Valores posibles dentro de la variable 'grupo' que los identifica de manera arbitraria como negativos.
#' @param color_positivo Color asociado al grupo positivo.
#' @param color_negativo Color asociado al grupo negativo.
#'
#' @return
#' @export
#'
#' @examples
#' graficar_candidato_saldo(bd, grupo_positivo = "Buena", grupo_negativo = "Mala")
#' graficar_candidato_saldo(bd, grupo_positivo = c("Buena", "Muy buena"), grupo_negativo = c("Mala", "Muy mala"), color_positivo = "orange", color_negativo = "brown")
graficar_candidatoSaldo <- function(bd, grupo_positivo = c("Buena", "Muy buena"), grupo_negativo = c("Mala", "Muy mala"), color_positivo = "green", color_negativo = "red"){

  g <- bd %>%
    ggplot(aes(x = forcats::fct_reorder(tema, saldo), y = saldo, fill = grupo)) +
    ggchicklet::geom_chicklet(width =.6, alpha =.9) +
    ggfittext::geom_bar_text(aes(label = scales::percent(saldo, accuracy = 1)), contrast = T, family = "Poppins") +
    coord_flip() +
    scale_fill_manual(values = c("Negativa" = color_negativo,
                                 "Positiva" = color_positivo)) +
    lemon::scale_y_symmetric(labels = scales::percent_format(accuracy = 1))+
    theme_minimal()+
    theme(legend.position = "bottom",
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank())+
    labs(x = NULL, fill = NULL)

  # if("p_calve" %in% names(bd)) g <- g + geom_text(aes(label = p_calve),
  #                                                 hjust=ifelse(test = bd$grupo == "Positiva",  yes = -.2, no = 1.2), size=3.5, colour="#505050")
  #
  # g +
  #   lemon::scale_y_symmetric(labels = scales::percent_format(accuracy = 1))+
  #   theme_minimal()+
  #   theme(legend.position = "bottom",
  #         panel.grid.minor.y = element_blank(),
  #         panel.grid.major.y = element_blank())+
  #   labs(y = "Saldo", x = NULL, fill = NULL)

  return(g)

}

analisis_correspondencia <- function(var1, var2, legenda1=NULL, legenda2=NULL, diseno, colores =NULL){

  if(is.null(legenda1)) legenda1 <- var1
  if(is.null(legenda2)) legenda2 <- var2
  if(is.null(colores)) colores <- c("#DE6400","#023047")

  formula <- survey::make.formula(c(var1,var2))
  aux <- survey::svytable(formula, design = diseno) %>%
    tibble::as_tibble() %>%
    tidyr::pivot_wider(names_from = var2, values_from = "n") %>%
    tibble::column_to_rownames(var = var1)


  # chisq.test(aux)
  res.ca <- FactoMineR::CA(aux, graph = F)
  eig <- factoextra::get_eigenvalue(res.ca)[, 2]

  res.ca$col$coord %>%
    as_tibble(rownames = "respuesta") %>%
    janitor::clean_names() %>%
    select(respuesta,num_range("dim_",1:2)) %>%
    mutate(variable = legenda2) %>%
    bind_rows(res.ca$row$coord %>%
                as_tibble(rownames = "respuesta") %>%
                janitor::clean_names() %>%
                select(respuesta,num_range("dim_",1:2)) %>%
                mutate(variable = legenda1)) %>%
    ggpubr::ggscatter(x = "dim_1", y = "dim_2", color = "variable", label = "respuesta", repel = T) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = scales::percent(eig[1]/100,accuracy = .1),
         y = scales::percent(eig[2]/100,accuracy = .1),
         color = ""
    ) +
    scale_color_manual(values = colores) +
    lemon::scale_x_symmetric() +
    lemon::scale_y_symmetric() +
    theme(axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major.x = element_line(colour = "#C5C5C5",linetype = "dotted"),
          panel.grid.major.y = element_line(colour = "#C5C5C5",linetype = "dotted"))

}

#' Title
#'
#' @param bd
#'
#' @return
#' @export
#'
#' @examples
graficar_conocimiento_region <- function(bd, orden_horizontal){

  orden_horizontal <- orden_horizontal %>% stringr::str_wrap(5)

  bd %>%
    ggplot(aes(x = factor(region %>% stringr::str_wrap(5), levels = orden_horizontal),
               y = forcats::fct_reorder(tema %>%  str_wrap(60), pct),
               fill = pct)) +
    geom_tile()+
    labs(y = NULL, x= NULL, fill = "Porcentaje")+
    theme_minimal()+
    theme(panel.grid = element_blank(),
          axis.ticks = element_blank(),
          # axis.text.x = element_text(angle = 45),
          text = element_text(family = "Poppins", size=14))+
    scale_x_discrete(position = "top") +
    scale_fill_gradient(high = "#05578A", low= "#EBF7FE",
                        labels = scales::percent_format(accuracy = 1) )+
    # scale_fill_continuous(labels = scales::percent_format(accuracy = 1) )+
    ggfittext::geom_fit_text( grow = F,reflow = F,contrast = T,
                              aes(label =pct %>%  scales::percent(accuracy = 1)))
}

#' Title
#'
#' @param bd
#'
#' @return
#' @export
#'
#' @examples
graficar_saldo_region <- function(bd, orden_horizontal){

  orden_horizontal <- orden_horizontal %>% stringr::str_wrap(5)

  bd %>%
    ggplot(aes(x = factor(region %>% stringr::str_wrap(5), levels = orden_horizontal),
               y =forcats::fct_reorder(tema %>% stringr::str_wrap(60),saldo),
               fill = saldo)) +
    geom_tile() +
    scale_fill_gradient2(low = "orange", mid = "white", high = "blue")+
    labs(y = NULL, x= NULL, fill = "Saldo")+
    theme_minimal()+
    theme(panel.grid = element_blank(),
          axis.ticks = element_blank(),
          # axis.text.x = element_text(angle = 45),
          text = element_text(family = "Poppins", size=14))+
    scale_x_discrete(position = "top") +
    scale_fill_gradient2(high ="#046B9F", low= "#DE6400", mid = "white",
                         labels = scales::percent_format(accuracy = 1) ) +
    # scale_fill_continuous(labels = scales::percent_format(accuracy = 1) )+
    ggfittext::geom_fit_text( grow = F,reflow = F,contrast = T,
                              aes(label =saldo %>%  scales::percent(accuracy = 1)))
}


#' Title
#'
#' @param bd
#' @param var
#'
#' @return
#' @export
#'
#' @examples
graficar_mapa_region <- function(bd, var){
  bd %>% ggplot() + geom_sf(aes(fill = {{var}}), size = .3, alpha = .8, color = "white") +
    theme_minimal() +
    theme(axis.line = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          legend.position = "bottom",
          axis.text = element_blank(),
          plot.title = element_text(hjust = 0,
                                    size = rel(1.1),
                                    # face = "bold",
                                    colour = "#4C5B61"),
          text = element_text(family = "Poppins", size=14))
}

#' Title
#'
#' @param lst
#'
#' @return
#' @export
#'
#' @examples
graficar_blackbox_1d <- function(lst){
  print(lst$stimuli)
  print(lst$slf)

  lst$individuals %>%# mutate(c1 =c1*-1) %>%
    ggplot(aes(x = c1)) +
    geom_density(color = "#871938") +
    facet_wrap(~stimuli) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
    labs(subtitle = glue::glue("Explica el {scales::percent(lst$fits$percent/100)} de la varianza total"))+
    theme_minimal()+
    theme(   # legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.text = element_text(color  = "#A0B12F"),
      panel.grid.major.y = element_line(linetype = "dotted"),
      # axis.text = element_blank(),
      text = element_text(family = "Poppins", size=14))
}

#' Graficar metodología de MORENA con un geom_tile
#'
#' @param atr Base de datos producto de la función 'analizar_morena'.
#' @param cliente Vector de códigos cortos relacionados a los personajes a los cuales se les aplicó la batería de preguntas de MORENA.
#' @param atributos Vector de códigos de cortos que identifican los diferentes atributos contenidos en la metodología de MORENA.
#'
#' @return
#' @export
#'
#' @examples
#' graficar_morena(atr, personajes = c("era", "sasil"), atributos = c("honesto", "opinion"))
#' graficar_morena(atr, personajes = c("era", "sasil", "jaac"), atributos = atributos)
graficar_morena <- function(atr, personajes, atributos){

  orden <- atr %>%
    distinct(tema, atributo, puntos, .keep_all = T) %>%
    select(-aspecto,-ganador,-puntos,-personaje) %>%
    pivot_wider(names_from = atributo, values_from = media) %>%
    left_join(atr %>% count(tema, wt = puntos)) %>%
    arrange(n, preferencia, votaria) %>%
    pull(tema)

  g <- atr %>%
    ggplot(aes(x = atributo, y = factor(tema, orden), fill = media, label = scales::percent(media, 1.))) +
    geom_tile() +
    ggfittext::geom_fit_text(contrast = T, family = "Poppins") +
    geom_label(data = atr %>% filter(puntos!=0), aes(label = puntos),
               color = "black", vjust = 0, nudge_y = -.5, fill = "white", family = "Poppins") +
    geom_text(data = atr %>% count(tema, wt = puntos),
              aes(label = n, x  ="Puntaje", y = tema),
              inherit.aes = F, family = "Poppins") +
    scale_fill_continuous(labels = scales::percent) +
    labs(x = NULL, y = NULL, fill = "Porcentaje") +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_x_discrete(position = "top",
                     limits = c("opinion", atributos$atributo, "buencandidato", "votaria", "preferencia", "Puntaje"),
                     labels = c("Opinión\npositiva", "Honestidad", "Respeta \nderecho\nmujeres",
                                "Cercano\na la\ngente", "Conoce el\nEstado", "Cumple", "Buen\ncandidato",
                                "Disposición\na\nvotar", "Preferencia\ncomo\ncandidato/a", "Puntaje\nfinal")) +
    scale_fill_gradient(low = "white", high = "#A6032F", labels = scales::percent) +
    theme(plot.caption = element_text(family = "Poppins", size = 14),
          axis.text.x = element_text(family = "Poppins", size = 10),
          axis.text.y = element_text(family = "Poppins", size = 14), panel.grid = element_blank())

  return(g)

}

#' Graficar cruce de una variable vs varias variables
#'
#' @param bd Base de datos producto de la función 'analizar_crucePuntos'
#' @param cruce Variable principal por la cual se hace el cruce
#' @param vartype
#'
#' @return
#' @export
#'
#' @examples
#' graficar_cruce_puntos(bd, cruce = "rurub", vartype = "cv")
graficar_crucePuntos = function(bd, cruce, vartype){
  g <- bd |>
    ggplot(aes(x=reorder(variable,mean), xend=variable,
               color=!!rlang::sym(cruce))) +
    geom_vline(aes(xintercept = variable), linetype = "dashed",
               color = "gray60", size=.5) +
    geom_point(aes(y=mean),
               shape=19,  size=6) +
    geom_linerange(aes(ymin = mean-!!rlang::sym(vartype), ymax = mean+!!rlang::sym(vartype)),
                   linetype="solid", color="black", linewidth=.5) +
    scale_y_continuous(labels=scales::percent) +
    coord_flip()

  return(g)
}

#' Graficar cruce de una variable vs otra con opción a filtro
#'
#' @param bd Base de datos resultado de la función 'analizar_cruceBrechas'
#' @param var1 Variable principal por la cual se hace el cruce
#' @param var2_filtro Variable secundaria para hacer análisis con la primaria
#' @param vartype
#' @param line_rich Argumento de la función 'geom_textline'
#' @param line_linewidth Argumento de la función 'geom_textline'
#' @param line_hjust Argumento de la función 'geom_textline'
#' @param line_vjust Argumento de la función 'geom_textline'
#'
#' @return
#' @export
#'
#' @examples
#' graficar_cruce_brechasDuales(bd, var1 = "AMAI_factor", var2_filtro = "candidato_preferencia")
graficar_cruce_brechasDuales = function(bd, var1, var2_filtro, vartype = "cv", line_rich = F, line_linewidth = 2, line_hjust = 0.5, line_vjust = -0.5){
  g <- bd |>
    ggplot(aes(x=!!rlang::sym(var1),
               y=coef)) +
    geomtextpath::geom_textline(aes(color=!!rlang::sym(var2_filtro),
                                    group=!!rlang::sym(var2_filtro),
                                    label=!!rlang::sym(var2_filtro)),
                                linewidth = line_linewidth, hjust = line_hjust,
                                vjust = line_vjust, rich = line_rich,
                                size=6, family = "Poppins") +
    scale_y_continuous(labels=scales::percent) +
    labs(color = NULL)

  if(vartype == "cv"){
    g <- g +
      geom_text(aes(label=pres),
                color="black", size=6, hjust=-.1)
  }

  return(g)
}

#' Graficar cruce de una variable vs múltiples variables
#'
#' @param bd Base de datos producto de la función 'analizar_crucePuntos'
#' @param cruce Variable principal por la cual se hace el cruce
#' @param vartype
#' @param line_rich Argumento de la función 'geom_textline'
#' @param line_linewidth Argumento de la función 'geom_textline'
#' @param line_hjust Argumento de la función 'geom_textline'
#' @param line_vjust Argumento de la función 'geom_textline'
#'
#' @return
#' @export
#'
#' @examples
graficar_cruce_brechasMultiples <- function(bd, cruce, vartype, line_rich, line_linewidth, line_hjust, line_vjust){
  g <- bd |>
    ggplot(aes(x=!!rlang::sym(cruce),
               y=mean)) +
    geomtextpath::geom_textline(aes(color=(variable),
                                    group=(variable),
                                    label=(variable)),
                                linewidth=line_linewidth, hjust = line_hjust,
                                vjust = line_vjust, rich = line_rich,
                                size = 6, family = "Poppins") +
    scale_y_continuous(labels=scales::percent) +
    labs(color = NULL)

  if(vartype == "cv"){
    g <- g +
      geom_text(aes(label=pres),
                color="black", size=6, hjust=-.1)
  }

  return(g)

}

#' Graficar cruce de una variable vs múltiples variables unando gráficas de barras
#'
#' @param bd Base de datos producto de la función 'analizar_crucePuntos'
#' @param cruce Variable principal por la cual se hace el cruce
#' @param vartype
#' @param color Color usado en el parámetro 'fill' de la función 'aes' de ggplot2
#' @param filter Filtro aplicable al parámetro 'cruce'
#'
#' @return
#' @export
#'
#' @examples
graficar_cruce_barrasMultiples = function(bd, cruce, vartype, color, filter){

  if(!is.null(filter)) {

    bd <- bd |>
      filter(!(!!rlang::sym(cruce) %in% filter))

  }

  g <- bd |>
    ggplot(aes(x=reorder(variable, mean),
               y=mean)) +
    ggchicklet::geom_chicklet(width = 0.6,alpha=0.9,
                              fill=color) +
    scale_y_continuous(labels = scales::percent) +
    coord_flip() +
    facet_wrap(rlang::as_label(rlang::sym(cruce)))

  if(vartype == "cv"){
    g <- g +
      ggfittext::geom_bar_text(aes(label = paste0(scales::percent(mean, accuracy=1), pres)),
                               color="white",
                               family = "Poppins")


  }else{
    g <- g +
      ggfittext::geom_bar_text(aes(label = scales::percent(mean, accuracy=1)),
                               color="white",
                               family = "Poppins")

  }
  return(g)

}

#' Graficar cruce de una variable vs otra con opción a filtro
#'
#' @param bd Base de datos producto de la función 'analizar_cruceBrechas'
#' @param cruce Variable principal por la cual se hace el cruce
#' @param variable Variable secundaria para hacer análisis con la primaria
#' @param vartype
#' @param filter Filtro aplicable al parámetro 'cruce'
#'
#' @return
#' @export
#'
#' @examples
graficar_cruce_bloques <-  function(bd, cruce, variable, vartype, filter){

  if(!is.null(filter)) {

    bd <- bd |>
      filter(!(!!rlang::sym(cruce) %in% filter))

  }

  g <- bd |>
    ggplot(aes(area=coef,
               fill=!!rlang::sym(variable))) +
    treemapify::geom_treemap(alpha=0.7) +
    facet_wrap(rlang::as_label(rlang::sym(cruce)))

  if(vartype == "cv"){

    g <- g +
      treemapify::geom_treemap_text(aes(label = paste0(!!ensym(variable), ", ", scales::percent(coef,accuracy = 1), pres)),
                        place = "centre", grow = TRUE, reflow = TRUE, show.legend = F,
                        color="white",
                        family = "Poppins")
  }else{
    g <- g +
      treemapify::geom_treemap_text(aes(label=paste0(!!ensym(variable), ", ", scales::percent(coef,accuracy = 1))),
                        place = "centre", grow = TRUE, reflow = TRUE, show.legend = F,
                        color="white",
                        family = "Poppins")

  }

  return(g)
}

#' Graficar sankey
#'
#' @param bd Base de datos procesada con la función analizar_sankey
#' @param size_text_cat Tamaño del texto mostrado en cada nodo el sankey
#'
#' @return
#' @export
#'
#' @examples
#' graficar_sankey(bd = bd_estimacion, size_text_cat = 8)
graficar_sankey = function(bd, size_text_cat){

  g <- bd |>
    ggsankey::make_long(-n, value = n) |>
    ggplot(aes(x = x,
               value = value,
               next_x = next_x,
               node = node,
               next_node = next_node,
               fill = factor(node))) +
    ggsankey::geom_sankey() +
    ggsankey::geom_sankey_label(data = . %>% filter(x == names(bd)[1]),
                                aes(label = node, color = node),
                                hjust = 1.0, fill = "white", size = size_text_cat) +
    ggsankey::geom_sankey_label(data = . %>% filter(x == names(bd)[2]),
                                aes(label = node, color = node),
                                hjust = -0.2, fill = "white", size = size_text_cat) +
    labs(fill = "") +
    guides(color = "none") +
    theme(legend.position = "bottom")

  return(g)

}

#' Graficar nube de palabras clave
#'
#' @param bd Base de datos procesada con la estructura "palabras", "n", y "colores"
#'
#' @return
#' @export
#'
#' @examples
#' graficar_nubePalabras_hc(bd = tibble(palabras = c("hola"), n = c(2), colores = c("#619CFF")))
graficar_nubePalabras_hc = function(bd){

  hc <- bd |>
    highcharter::hchart(highcharter::hcaes(x = palabras, weight = log(n), acolor = colores),
                        type= "wordcloud") %>%
    highcharter::hc_chart(style = list(fontFamily = "Poppins"))

  return(hc)

}
