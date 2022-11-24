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
                                       salto =20,
                                       nota = "",
                                       tema){

  g <-  bd %>% ggplot(aes(x = forcats::fct_reorder(stringr::str_wrap(respuesta, salto),
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
    ggfittext::geom_bar_text(aes(label=scales::percent(media, accuracy = 1)),contrast = T, family = tema()$text$family)+
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

graficar_gauge_promedio <- function(bd, color = "#850D2D", maximo = 10, familia, texto = "\n Promedio"){
  bd %>% ggplot() + geom_rect(aes(xmin = 2, xmax = 3, ymin = 0,
                                  ymax = media), fill = color, color = "white") +
    geom_rect(aes(xmin = 2, xmax = 3, ymin = media,
                  ymax = maximo), fill = "grey90", color = "white") +
    geom_text(aes(x = 0,
                  y = media, label = paste(media %>% round(1), texto)),
              size = 10, family = familia, nudge_y = 0.25) + scale_fill_manual(values = c("#1DCDBC",
                                                                                          "#38C6F4")) +
    scale_x_continuous(limits = c(0, NA)) +
    scale_y_continuous(limits = c(0, maximo)) + xlab("") +
    ylab("") + coord_polar(theta = "y") + theme_void() +
    theme(legend.position = "bottom", axis.text = element_blank(),
          text = element_text(size = 15, family = familia))
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

graficar_intervalo_numerica<- function(bd, tema){
  bd %>%
    ggplot(aes(y = media, x = stats::reorder(str_wrap(tema,40),media))) +
    geom_pointrange(aes(ymin = inf, ymax = sup), color = "#850D2D") +
    coord_flip()+
    labs(title = NULL,
         x = NULL,
         y = "Promedio")+
    geom_text(aes(label = round(media,digits = 2)), nudge_x = .3, family = tema()$text$family)

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


graficar_candidato_opinion <- function(bd, ns_nc, regular,grupo_positivo,
                                       grupo_negativo,
                                       colores,
                                       burbuja,
                                       color_burbuja,
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
                 x = factor(1))) + geom_point(aes(alpha = escala, size = escala), color = color_burbuja) +
      geom_text(aes(label = scales::percent(media,1)),hjust = -.5) +
      tema() +
      theme(legend.position = "none", panel.grid.major.x = element_blank(),
            axis.text = element_blank(),axis.line.x = element_blank())
  }

  a <- aux %>%
    {if(!is.null(ns_nc)) filter(., respuesta!= ns_nc) else .}  %>%
    ggplot(aes(x  = factor(tema, orden), fill = respuesta,
               group = factor(Regular, levels = c( "regular2", grupo_negativo, "regular1", grupo_positivo)), y =media)) +
    ggchicklet::geom_chicklet(stat = "identity", width =.6, alpha =.9)+
    scale_fill_manual(values = colores)+
    ggfittext::geom_fit_text(aes(label = etiqueta), family = tema()$text$family,
                             min.size = 8,
                             position = position_stack(.5,reverse = T), vjust = .5, contrast = T, show.legend = F) +
    coord_flip()+
    labs(fill= NULL , y= NULL, x = NULL)+theme_minimal()+
    geom_hline(yintercept = 0, color = "#FFFFFF", size= .6)+
    geom_hline(yintercept = 0, color = "gray", size= .6)+
    lemon::scale_y_symmetric(labels=scales::percent_format(accuracy = 1))+
    theme(legend.position = "bottom") %+replace% tema() +
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = salto))

  if(!is.null(ns_nc)){
    b<- aux %>%  filter(respuesta == ns_nc) %>%
      ggplot(aes(x = factor(tema, orden), y = media))+
      ggchicklet::geom_chicklet(width =.6, alpha =.9, fill = "gray")+
      coord_flip()+
      ggfittext::geom_bar_text(aes(label = etiqueta), family = tema()$text$family,
                               hjust = -.1)+
      labs(y = NULL, x = NULL)+
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      tema() +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

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


#' Title
#'
#' @param bases
#'
#' @return
#' @import patchwork
#' @export
#'
#' @examples
#'
graficar_candidato_partido <- function(bases, cliente, tipo_conoce, colores_candidato, solo_respondidos, colores_partido,tema){

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
    scale_fill_manual(values = colores_partido) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
    # geom_text(aes(x = 0, y = as.numeric(tema), label = tema), hjust = 0) +
    labs( y = "", title = "Identificación partidista", x= NULL) +
    tema() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank())

  a+ b+ plot_layout(widths = c(.2,.8))

}

graficar_candidato_saldo <- function(bd, grupo_positivo = c("Buena", "Muy buena"),
                                     grupo_negativo = c("Mala", "Muy mala"), familia){

  g <- bd %>% ggplot(aes(x  =forcats::fct_reorder(tema, saldo), fill = grupo,
                         y =saldo
  )) +
    ggchicklet::geom_chicklet(stat = "identity", width =.6, alpha =.9)+
    coord_flip()+
    scale_fill_manual(values = c("Negativa" = "#FB8500",
                                 "Positiva" = "#126782"))+
    geom_text(aes(label = scales::percent(saldo,accuracy = 1)), family = familia,
              position = position_stack(.5,reverse = T), vjust = .5)

  if("p_calve" %in% names(bd)) g <- g + geom_text(aes(label = p_calve),
                                                  hjust=ifelse(test = bd$grupo == "Positiva",  yes = -.2, no = 1.2), size=3.5, colour="#505050")

  g +
    lemon::scale_y_symmetric(labels = scales::percent_format(accuracy = 1))+
    theme_minimal()+
    theme(legend.position = "bottom",
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank())+
    labs(y = "Saldo", x = NULL, fill = NULL)

}

analisis_correspondencia <- function(var1, var2, legenda1=NULL, legenda2=NULL, diseno, colores =NULL){
  if(is.null(legenda1)) legenda1 <- var1
  if(is.null(legenda2)) legenda2 <- var2
  if(is.null(colores)) colores <- c("#DE6400","#023047")

  formula <- survey::make.formula(c(var1,var2))
  aux <- survey::svytable(formula, design = diseno) %>% tibble::as_tibble() %>%
    tidyr::pivot_wider(names_from = var2, values_from = "n") %>% tibble::column_to_rownames(var = var1)


  # chisq.test(aux)
  res.ca <- FactoMineR::CA(aux, graph = F)
  eig <- factoextra::get_eigenvalue(res.ca)[, 2]

  res.ca$col$coord %>% as_tibble(rownames = "respuesta") %>% janitor::clean_names() %>%
    select(respuesta,num_range("dim_",1:2)) %>% mutate(variable = legenda2) %>%
    bind_rows(
      res.ca$row$coord %>% as_tibble(rownames = "respuesta") %>%
        janitor::clean_names() %>%
        select(respuesta,num_range("dim_",1:2)) %>% mutate(variable = legenda1)
    ) %>% ggpubr::ggscatter(x = "dim_1", y = "dim_2", color = "variable",
                            label = "respuesta", repel = T) +
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
graficar_conocimiento_region <- function(bd){
  bd %>% ggplot(aes(x = region%>%  str_wrap(5), y =forcats::fct_reorder(tema %>%  str_wrap(20), pct), fill = pct)) +
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
graficar_saldo_region <- function(bd){
  bd %>%
    ggplot(aes(x = region%>% stringr::str_wrap(6), y =forcats::fct_reorder(tema %>% stringr::str_wrap(20),saldo), fill = saldo)) + geom_tile() +
    scale_fill_gradient2(low = "orange", mid = "white", high = "blue")+
    labs(y = NULL, x= NULL, fill = "Saldo")+
    theme_minimal()+
    theme(panel.grid = element_blank(),
          axis.ticks = element_blank(),
          # axis.text.x = element_text(angle = 45),
          text = element_text(family = "Poppins", size=14))+
    scale_x_discrete(position = "top") +
    scale_fill_gradient2(high ="#046B9F", low= "#DE6400", mid = "white",
                         labels = scales::percent_format(accuracy = 1) )
  # scale_fill_continuous(labels = scales::percent_format(accuracy = 1) )+
  # ggfittext::geom_fit_text( grow = F,reflow = F,contrast = T,
  #                           aes(label =saldo %>%  scales::percent(accuracy = 1)))
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

graficar_morena <- function(atr, atributos, p, thm){

  orden <- atr %>% distinct(tema, atributo, puntos, .keep_all = T) %>% select(-aspecto,-ganador,-puntos,-personaje) %>%
    pivot_wider(names_from = atributo, values_from = media) %>%
    left_join(
      atr %>% count(tema, wt = puntos)
    ) %>%
    arrange(n, preferencia, votaria) %>% pull(tema)

  atr %>% ggplot(aes(x = atributo,y = factor(tema, orden), fill = media,
                     label = scales::percent(media, p)
  )) + geom_tile() +
    ggfittext::geom_fit_text(contrast = T, family = thm()$text$family) +
    geom_label(data = atr %>% filter(puntos!=0), aes(label = puntos),
               color = "black", vjust = 0, nudge_y = -.5, fill = "white", family = thm()$text$family) +
    theme(legend.position = "bottom") +
    geom_text(data = atr %>% count(tema, wt = puntos),
              aes(label = n, x  ="Puntaje", y = tema), inherit.aes = F, family = thm()$text$family) +
    scale_fill_continuous(labels = scales::percent) +
    labs(x = NULL, y = NULL, fill = "Porcentaje")
}
