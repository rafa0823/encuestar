if(getRversion() >= "2.15.1")  utils::globalVariables(c("grupo"))

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
                                       nota = "", color = "#B0C429"){
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
    ggfittext::geom_bar_text(aes(label=scales::percent(media, accuracy = 1)),contrast = T)+
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
#'  @param bd  Base de datos (ya procesada)
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
                                                       "Ns/Nc" = "gray"),
                                          orden = c("Desaprueba poco","Desaprueba mucho",

                                                    "Aprueba poco",
                                                    "Aprueba mucho"),
                                          familia = "Poppins"){

  transparencia <- .8
  ancho_barras <- .45
  color_etiqueta <- "#3B3838"



  aux  <-  bd %>%
    mutate(etiqueta = media,
           media = case_when(respuesta %in%grupo_negativo~media*-1,
                             respuesta %in% grupo_positivo~media,
                             respuesta == ns_nc~media+1.01),
           media2 = case_when(respuesta == ns_nc~0, T~media)) %>%
    group_by(tema) %>%  mutate(saldo=sum(media2),
                               color_saldo = case_when(saldo>0~"#fb8500", saldo<0~"#126782",
                                                       saldo ==0 ~"gray")) %>%  ungroup()


  aux %>%  filter(respuesta != ns_nc) %>%
    ggplot(aes(x  =forcats::fct_reorder(tema, saldo), fill = respuesta, y =media,
               group =factor(respuesta, levels = orden) )) +
    ggchicklet::geom_chicklet(stat = "identity", width = ancho_barras, alpha = transparencia)+
    geom_text(aes(label = scales::percent(media,accuracy = 1)), family = familia,
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
    geom_point(data =  aux %>% filter(respuesta == ns_nc), shape = 18, size = 3,
               aes(x = tema, y=saldo, color = color_saldo), show.legend = F, position = position_nudge(x =.3))+
    geom_text(aes(label = scales::percent(media,accuracy = 1)), family = familia,
              position = position_stack(.7,reverse = T),
              color = color_etiqueta) +
    geom_hline(yintercept = 0, color = "#FFFFFF", size= .6)+
    geom_hline(yintercept = 0, color = "gray", size= .6)+
    geom_hline(yintercept = 1, color = "#FFFFFF", size = 1.2)+
    geom_hline(yintercept = 1, color = "#323232", linetype = "dotted", size = .7)

}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("familia"))

#' Title
#'
#' @param bd  Base de datos (ya procesada)
#' @param color Color de la barra
#' @param maximo Número máximo en la escala
#'
#' @return
#' @export
#'
#' @examples

graficar_gauge_promedio <- function(bd, color = "#850D2D", maximo = 10){
  bd %>%
    ggplot() +
    geom_rect(aes(xmin = 2, xmax = 3, ymin = 0, ymax =media), fill = color,
              color = 'white') +
    geom_text(aes(x = 0, y = media,
                  label = paste(media %>%  round(1), "\n Promedio")),
              size = 10, family = familia, nudge_y = 0.25) +
    scale_fill_manual(values = c('#1DCDBC', '#38C6F4')) +
    scale_x_continuous(limits = c(0, NA)) +
    scale_y_continuous(limits = c(0, maximo)) +
    xlab('') + ylab('') +
    coord_polar(theta = 'y') +
    theme_void() +
    theme(legend.position = 'bottom', axis.text = element_blank())
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
  ggplot(aes(y = media, x = stats::reorder(str_wrap(aspecto,40),media))) +
  ggchicklet::geom_chicklet(radius = grid::unit(3, "pt"),
                alpha= .95, fill = "#850D2D",
                width =.45)+ coord_flip()+
  labs(title = NULL,
       x = NULL,
       y = "Promedio")+
  ggfittext::geom_bar_text(aes(label= round(media,digits = 1)),
                           contrast = T)+
  theme(panel.grid.major.x =element_line(colour = "#C5C5C5",
                                         linetype = "dotted"),
        panel.grid.major.y = element_blank(),
        axis.line.y = element_line(colour = "#E1356D"),
        axis.line.x = element_blank())
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

graficar_barras_palabras <- function(bd, pregunta, n = 10){
  bd %>% tidytext::unnest_tokens(palabras, pregunta) %>%
    count(palabras,sort = T) %>%
    anti_join(tibble(palabras = c(stopwords::stopwords("es"),"ns","nc"))) %>%
    slice(1:n) %>%
    ggplot(aes(x = forcats::fct_reorder(palabras, n), y = n))+
    ggchicklet::geom_chicklet(radius = grid::unit(3, "pt"),
                              alpha= .8,
                              width =.45)+
    labs(title = titulo,
         x = NULL,
         y = NULL,
         caption = nota)+
    coord_flip()+
    scale_y_continuous(labels=scales::percent_format(accuracy = 1))+
    ggfittext::geom_bar_text(aes(label=scales::percent(n, accuracy = 1)),contrast = T)
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
  pregunta <- bd %>% tidytext::unnest_tokens(palabras, pregunta) %>% count(palabras,sort = T) %>%
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


  htmlwidgets::saveWidget(widget = pregunta, file = paste0(pregunta,".html"))
  webshot::webshot(url = paste0(pregunta,".html"),
          file = "plot.png",vwidth = ancho, vheight = alto,
          delay=3) # delay will ensure that the whole plot appears in the image
  grDevices::dev.off()

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
                               size = rel(1.5),
                               face = "bold",
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
    scale_y_continuous(labels=scales::percent_format(accuracy = 1))+
    theme(panel.grid.minor = element_blank(),
          text = element_text(family = familia),
          panel.grid.major.y = element_blank(),
          legend.position = "bottom")+
    labs(title = titulo, fill = NULL, x = etiqueta_x)
}
