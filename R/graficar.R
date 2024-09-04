if(getRversion() >= "2.15.1")  utils::globalVariables(c("grupo"))

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
                                  colour = "#C5C5C5",
                                  family = "Poppins"),
     plot.margin = unit(c(1, 1, 1, 1), "lines"),
     strip.text=element_text(colour ="#2C423F")))
}
#' Title
#'
#' @param size_axis_text_x
#' @param size_axis_text_y
#'
#' @return
#' @export
#'
#' @examples
tema_transparente = function(){
  theme(legend.background = element_rect(color = "transparent", fill = "transparent"),
        panel.background = element_rect(color = "transparent", fill = "transparent"),
        plot.background = element_rect(color = "transparent", fill = "transparent"),
        strip.background = element_rect(color = "transparent", fill = "transparent"))
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
tema_morant = function(base_size = 15, base_family = "Poppins", fondo = "#FFFFFF") {
  (ggthemes::theme_foundation(base_size = base_size,
                              base_family = base_family) +
     theme(
       line = element_line(colour = "#4C5B61"),
       rect = element_rect(fill = fondo, linetype = 0, colour = NA),
       text = element_text(color = "#2C423F"),
       axis.title = element_blank(),
       axis.text = element_text(),
       axis.text.x = element_text(size = 14),
       axis.text.y = element_text(size = 16),
       axis.ticks = element_blank(),
       axis.line.x = element_line(colour = "#E1356D"),
       legend.position = "none",
       legend.direction = "horizontal",
       legend.box = "vertical",
       legend.text = element_text(size = 14),
       panel.grid = element_line(colour = NULL),
       panel.grid.major.y = element_blank(),
       panel.grid.major.x = element_line(colour = "#C5C5C5", linetype = "dotted"),
       panel.grid.minor = element_blank(),
       plot.title = element_text(hjust = 0, size = rel(1.1), colour = "#4C5B61"),
       plot.subtitle = element_text(hjust = 0, size = rel(1), face = "bold", colour = "#C5C5C5", family = base_family),
       plot.caption = element_text(size = 14),
       plot.margin = unit(c(1, 1, 1, 1), "lines"),
       strip.text = element_text(colour ="#2C423F")
     ) +
     tema_transparente()
  )
}
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
                            desplazar_porcentajes = 0,
                            orden_respuestas = NA){

  g <-
    bd %>%
    {
      if(length(orden_respuestas) == 1) {
        ggplot(data = .,
               aes(x = forcats::fct_reorder(stringr::str_wrap(respuesta, salto), media),
                   y  = media,
                   fill = respuesta))
      } else {
        ggplot(data = .,
               aes(x = factor(stringr::str_wrap(respuesta, salto), levels = stringr::str_wrap(orden_respuestas, salto)),
                   y  = media,
                   fill = respuesta))
      }
    } +
    ggchicklet::geom_chicklet(radius = grid::unit(3, "pt"), color = "transparent", alpha = 0.8, width = 0.45)
  if (porcentajes_fuera == F) {
    g <-
      g +
      ggfittext::geom_bar_text(aes(label = scales::percent(media, accuracy = 1)), contrast = T)
  }
  if (porcentajes_fuera == T) {
    g <-
      g +
      geom_text(aes(label = scales::percent(media, accuracy = 1)), nudge_y = desplazar_porcentajes)
  }
  g <-
    g +
    coord_flip() +
    labs(x = NULL, y = NULL) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  return(g)
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
  g <-
    bd %>%
    ggplot() +
    geom_rect(aes(xmin = 2, xmax = 3, ymin = 0, ymax = media),
              fill = color_principal,  color = "white", alpha= .95) +
    geom_rect(aes(xmin = 2, xmax = 3, ymin = media, ymax = escala[2]),
              fill = color_secundario, color = "white")
  if(escala[2] == 1) {
    g <-
      g +
      geom_text(aes(x = 0, y = 0.5, label = scales::percent(x = media, accuracy = 1.)),
                size = size_text_pct, family = "Poppins", nudge_y = 0.25)
  }
  else {
    g <-
      g +
      geom_text(aes(x = 0, y = 0.5, label = scales::comma(x = media, accuracy = 1.1)),
                size = size_text_pct, family = "Poppins", nudge_y = 0.25)
  }
  g <-
    g +
    scale_fill_manual(values = c("#1DCDBC", "#38C6F4")) +
    scale_x_continuous(limits = c(0, NA)) +
    scale_y_continuous(limits = c(0, escala[2])) +
    xlab("") +
    ylab("") +
    coord_polar(theta = "y") +
    theme_void() +
    theme(legend.position = "none",
          axis.text = element_blank(),
          text = element_text(size = 15, family = "Poppins"))
  return(g)
}
sustituir <- function(bd, patron, reemplazo = ""){
  bd %>% mutate(respuesta = gsub(pattern = patron, replacement = reemplazo,
                                 x = respuesta, fixed = T))
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
#' Title
#'
#' @param bd
#' @param orden_x
#' @param orden_y
#' @param color
#' @param salto_x
#' @param salto_y
#' @param caption
#' @param size_text_x
#' @param size_text_y
#' @param size_text_caption
#'
#' @return
#' @export
#'
#' @examples
graficar_heatmap = function(bd, orden_x, orden_y, color = "blue", caption, salto_x, salto_y, size_text_x = 14, size_text_y = 16, size_text_caption = 14){
  g <-
  bd |>
  ggplot(aes(x = factor(respuesta, levels = orden_x),
             y = factor(candidato, levels = orden_y),
             fill = media,
             label = scales::percent(media, 1.))) +
    geom_tile(color="white") +
    ggfittext::geom_fit_text(contrast = T, family = "Poppins") +
    scale_fill_gradient(low = "white",
                        high = color,
                        labels = scales::percent, na.value = "gray70") +
    scale_x_discrete(position = "top",
                     labels =  function(x) stringr::str_wrap(string = x, width = salto_x)) +
    scale_y_discrete(labels =  function(x) stringr::str_wrap(string = x, width = salto_y)) +
    labs(x = "",
         y = "",
         caption = caption) +
    theme_minimal() +
    tema_transparente() +
    theme(legend.position = "none",
          text = element_text(family = "Poppins"),
          panel.grid = element_blank(),
          axis.text.x = element_text(size = size_text_x),
          axis.text.y = element_text(size = size_text_y),
          plot.caption = element_text(size = size_text_caption))
  return(g)
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
#' @import patchwork
#' @export
#'
#' @examples
#' graficar_candidato_opinion(bd, ns_nc = "Ns/Nc", regular = "Regular", grupo_positivo = "Buena", grupo_negativo = "Mala", colores = c("red", "yellow", "green", "gray70"), burbuja = burbuja, color_burbuja = "blue", caption_opinion = "", caption_nsnc = "Ns/Nc", caption_burbuja = "Nivel de conocimiento", size_caption_opinion = 12, size_caption_nsnc = 12, size_caption_burbuja = 12, size_caption_cat = 12, orden_resp = c("Mala", "Regular", "Buena"), tema = self$tema)
graficar_candidato_opinion <- function(bd, ns_nc, regular,
                                       grupo_positivo,
                                       grupo_negativo,
                                       colores,
                                       color_nsnc,
                                       burbuja,
                                       color_burbuja,
                                       caption_opinion,
                                       caption_nsnc,
                                       caption_burbuja,
                                       size_caption_opinion,
                                       size_caption_nsnc,
                                       size_caption_burbuja,
                                       size_text_cat,
                                       size_pct,
                                       orden_resp,
                                       salto = 200,
                                       tema,
                                       mostrar_nsnc = T,
                                       salto_respuestas,
                                       orden_cat = NULL,
                                       patron_inicial = NULL){

  if(!is.null(ns_nc)){
    bd <- bd %>% group_by(tema) %>% complete(respuesta = ns_nc, fill = list(media = 0)) %>% ungroup
  }

  aux <- bd %>% mutate(Regular = if_else(respuesta == regular, "regular1", as.character(respuesta))) %>%
    bind_rows(bd %>% filter(respuesta == regular) %>% mutate(Regular = "regular2", media = -media)) %>%
    mutate(etiqueta = if_else(Regular != "regular2", scales::percent(media,1), ""),
           media = if_else(respuesta %in% grupo_negativo,-1*media,media),
           media = if_else(respuesta == regular, media/2, media)) %>%
    group_by(tema) %>%
    mutate(saldo = sum(as.numeric(!(respuesta %in% c(regular, ns_nc)))*media))

  orden <- aux %>% arrange(saldo) %>% pull(tema) %>% unique %>% na.omit

  if(!is.null(orden_cat)) {

    orden <- aux %>%
      ungroup() |>
      mutate(aspecto = gsub(pattern = paste0(patron_inicial, "_"),
                            replacement = "",
                            x = aspecto)) |>
      distinct(aspecto, tema) |>
      mutate(aspecto = factor(aspecto, levels = orden_cat, ordered = TRUE)) |>
      arrange(desc(aspecto)) |>
      pull() |>
      as.factor()

  }

  if(!all(is.na(burbuja))){
    burbuja <- burbuja %>% mutate(escala = media/max(media), tema = forcats::fct_reorder(tema, media))
    orden <- burbuja$tema %>% levels
    g_burbuja <-
      burbuja %>%
      ggplot(aes(y = tema,
                 x = factor(1))) +
      geom_point(aes(size = escala), color = color_burbuja, shape = 16) +
      geom_text(aes(label = scales::percent(media,1)), hjust = -.5) +
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = salto)) +
      tema +
      labs(caption = caption_burbuja) +
      theme(legend.position = "none",
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.line.x = element_blank(),
            plot.caption = element_text(hjust = 0.5, size = size_caption_burbuja))
  }

  g_opinion <-
    aux %>%
    {if(!is.null(ns_nc)) filter(., respuesta!= ns_nc) else .}  %>%
    mutate(respuesta = factor(respuesta, levels = orden_resp)) |>
    graficar_barras_saldo(orden = orden,
                          grupo_positivo = grupo_positivo,
                          grupo_negativo = grupo_negativo,
                          Regular = regular,
                          colores = colores,
                          salto_respuestas = salto_respuestas,
                          salto_tema = salto,
                          caption_opinion = caption_opinion,
                          size_text_cat = size_text_cat,
                          size_pct = size_pct,
                          size_caption_opinion = size_caption_opinion)

  if(!is.null(ns_nc)){
    b <- aux %>%
      filter(respuesta == ns_nc) %>%
      ggplot(aes(x = factor(tema, orden),
                 y = media))+
      ggchicklet::geom_chicklet(width =.6, alpha =.9, fill = color_nsnc, color = "transparent") +
      ggfittext::geom_bar_text(aes(label = etiqueta), color = "#2C423F",
                               hjust = -.1) +
      coord_flip() +
      labs(y = NULL, x = NULL, caption = caption_nsnc) +
      scale_y_continuous(n.breaks = 2) +
      tema +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            axis.text.x = element_blank(), axis.line.x = element_blank(),
            plot.caption = element_text(hjust = 0.5, size = size_caption_nsnc))

    if(!all(is.na(burbuja))){
      if(mostrar_nsnc) {
        final <- g_opinion + g_burbuja + b + plot_layout(widths = c(.7, .15, .15), ncol= 3)
      } else {
        final <- g_opinion + g_burbuja + plot_layout(widths = c(.7, .15, .15), ncol = 3)
      }
    } else{
      if(mostrar_nsnc) {
        final <- g_opinion + b + plot_layout(widths = c(.8, .2))
      } else {
        final <- g_opinion
      }

    }

  } else{
    if(!all(is.na(burbuja))){
      final <- g_opinion + g_burbuja + plot_layout(widths = c(.8,.2))
    } else{
      final <- g_opinion
    }
  }
  return(final &
           theme(plot.background = element_rect(color = "transparent", fill = "transparent"),
                 panel.background = element_rect(color = "transparent", fill = "transparent"),
                 legend.background = element_rect(color = "transparent", fill = "transparent")))
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

  a + b + patchwork::plot_layout(widths = c(.2,.8)) &
    tema_transparente()

}
#' Title
#'
#' @param bd
#' @param orden
#' @param grupo_positivo
#' @param grupo_negativo
#' @param colores
#' @param salto_respuestas
#' @param salto_tema
#' @param caption_opinion
#' @param size_text_cat
#' @param size_caption_opinion
#' @param tema
#'
#' @return
#' @export
#'
#' @examples
graficar_barras_saldo = function(bd, orden, grupo_positivo, grupo_negativo, Regular, colores, salto_respuestas, salto_tema, caption_opinion, size_text_cat, size_pct, size_caption_opinion, tema = encuestar:::tema_morant()){

  if(!is.na(Regular)) {
    group_levels <- c("regular2", grupo_negativo, "regular1", grupo_positivo)
  } else {
    group_levels <- c(grupo_negativo, grupo_positivo)
  }

  g <-
    bd |>
    ggplot(aes(x  = factor(tema, orden),
               y = media,
               fill = respuesta,
               group = factor(Regular, levels = group_levels))) +
    ggchicklet::geom_chicklet(color = "transparent", width =.6, alpha =.9) +
    ggfittext::geom_fit_text(aes(label = etiqueta),
                             size = size_pct,
                             position = position_stack(.5, reverse = T),
                             vjust = .5,
                             contrast = T,
                             show.legend = F) +
    geom_hline(yintercept = 0, color = "#FFFFFF", size = .6) +
    geom_hline(yintercept = 0, color = "gray", size = .6) +
    coord_flip() +
    scale_fill_manual(values = colores,
                      labels = function(x) stringr::str_wrap(string = x, width = salto_respuestas)) +
    labs(x = NULL,
         y = NULL,
         fill = NULL,
         caption = caption_opinion) +
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = salto_tema)) +
    lemon::scale_y_symmetric(labels = function(x) scales::percent(abs(x), accuracy = 1)) +
    theme_minimal() +
    tema +
    theme(legend.position = "bottom") +
    theme(axis.text.y = element_text(size = size_text_cat),
          plot.caption = element_text(hjust = 0.5, size = size_caption_opinion),
          legend.key.size = unit(1, units = "cm"))
  return(g)
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
    ggplot(aes(x = forcats::fct_reorder(tema, saldo),
               y = saldo,
               fill = grupo)) +
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
#' Title
#'
#' @param bd
#' @param orden_var_x
#' @param salto_x
#' @param salto_legend
#' @param limits
#' @param text_nudge_y
#' @param size_text
#' @param colores_var_y
#' @param size_text_x
#' @param size_text_y
#' @param size_text_legend
#'
#' @return
#' @export
#'
#' @examples
graficar_lineas = function(bd, orden_var_x, colores_var_y, salto_x, salto_legend, limits = c(0, 0.75), text_nudge_y = 0.01, size_text = 8,
                           size_text_x = 16, size_text_y = 14, size_text_legend = 14){
  g <-
    bd |>
    ggplot(aes(x = factor(var_x, levels = orden_var_x),
               y = media,
               group = var_y,
               color = var_y)) +
    geom_line(show.legend = FALSE,
              linewidth = 1) +
    geom_point(size = 3) +
    ggrepel::geom_text_repel(aes(label = scales::percent(x = media, accuracy = 1.0)),
                             size = size_text,
                             nudge_y = text_nudge_y,
                             show.legend = FALSE) +
    scale_x_discrete(labels = function(x) stringr::str_wrap(string = x, width = salto_x)) +
    scale_y_continuous(labels = scales::percent,
                         limits = limits) +
    scale_color_manual(values = colores_var_y,
                       labels = function(x) stringr::str_wrap(string = x, width = salto_legend)) +
    labs(color = "") +
    tema_morant() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(size = size_text_x),
          axis.text.y = element_text(size = size_text_y),
          legend.text = element_text(size = size_text_legend))
  return(g)
}
#' Graficar conocimiento de personajes por región o estrato
#'
#' @param bd Base de datos resultado de la función 'analizar_conocimientoRegion'
#' @param ordenRegiones Vector que indica el orden de las columnas del geom_tile
#' @param salto_labelRegiones Parámetro 'width' usado por stringr::str_wrap en las etiquetas superiores
#'
#' @return
#' @export
#'
#' @examples
#' graficar_conocimientoRegion(bd_analizar_conocimientoRegion, ordenRegiones = c("reg_02", "reg_01, "reg_03))
graficar_conocimientoRegion <- function(bd, ordenRegiones, salto_labelRegiones = 5){
  if(is.null(ordenRegiones)) {
    ordenRegiones <- bd |>
      ungroup() |>
      distinct(region) |>
      mutate(region = stringr::str_wrap(string = region, width = salto_labelRegiones)) |>
      pull() |>
      stringr::str_wrap(width = salto_labelRegiones)
  }
  g <- bd %>%
    ggplot(aes(x = factor(region |> stringr::str_wrap(width = salto_labelRegiones), levels = ordenRegiones),
               y = forcats::fct_reorder(tema %>%  str_wrap(60), pct),
               fill = pct)) +
    geom_tile() +
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
  return(g)
}

#' Graficar saldo de opinión asociado a personajes por región o estrato
#'
#' @param bd Base de datos resultado de la función 'analizar_saldoRegion'
#' @param ordenRegiones Vector que indica el orden de las columnas del geom_tile
#' @param salto_labelRegiones Parámetro 'width' usado por stringr::str_wrap en las etiquetas superiores
#'
#' @return
#' @export
#'
#' @examples
#' graficar_conocimientoRegion(bd_analizar_conocimientoRegion, ordenRegiones = c("reg_02", "reg_01, "reg_03))
graficar_saldoRegion <- function(bd, ordenRegiones, salto_labelRegiones = 5){

  if(is.null(ordenRegiones)) {
    ordenRegiones <- bd |>
      ungroup() |>
      distinct(region) |>
      mutate(region = stringr::str_wrap(string = region, width = salto_labelRegiones)) |>
      pull() |>
      stringr::str_wrap(width = salto_labelRegiones)
  }

  g <- bd %>%
    ggplot(aes(x = factor(region %>% stringr::str_wrap(width = salto_labelRegiones), levels = ordenRegiones),
               y = forcats::fct_reorder(tema %>% stringr::str_wrap(60),saldo),
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

  return(g)
}
#' Graficar mapa por categorías
#'
#' @param bd Base de datos resultado de la función 'calcular_ganadorRegion
#' @param variable Variable categórica a graficar
#' @param categorica Si la variable es categórica, cambia la posición del 'legend.position'
#'
#' @return
#' @export
#'
#' @examples
#'
graficar_mapaRegiones <- function(bd, variable, categorica = T){
  g <- bd %>%
    ggplot() +
    geom_sf(aes(fill = !!rlang::sym(variable)), size = .3, alpha = .8, color = "white") +
    theme_minimal()
  if(categorica == F)
  {
    g <- g + theme(axis.line = element_blank(),
                   axis.ticks = element_blank(),
                   panel.grid = element_blank(),
                   legend.position = "right",
                   axis.text = element_blank(),
                   text = element_text(family = "Poppins", size = 14))
  }
  else
  {
    g <- g +
      labs(fill = "") +
      theme(axis.line = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            legend.position = "bottom",
            axis.text = element_blank(),
            text = element_text(family = "Poppins", size = 14))
  }
  return(g)
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
    geom_linerange(aes(ymin = mean-!!rlang::sym(vartype), ymax = mean+!!rlang::sym(vartype)),
                   linetype="solid", color="black", linewidth=.5) +
    geom_vline(aes(xintercept = variable), linetype = "dashed",
               color = "gray60", size=.5) +
    geom_point(aes(y=mean),
               shape=19,  size=6) +
    scale_y_continuous(labels=scales::percent) +
    coord_flip()
  return(g)
}
#' Graficar cruce de una variable vs varias variables pero filtradas
#'
#' @param orden_variablePrincipal Factor de la variable principal
#' @param bd Base de datos producto de la función 'analizar_crucePuntos'
#'
#' @return
#' @export
#'
#' @examples
graficar_cruce_puntosMultiples = function(bd, orden_variablePrincipal) {
  bd |>
    ggplot(aes(x = factor(variablePrincipal, levels = orden_variablePrincipal), y = mean,
               color = tema, group = variablePrincipal)) +
    geom_line(color="#a2d2ff",linewidth=4.5,alpha=0.5) +
    geom_point(size=7) +
    scale_y_continuous(labels=scales::percent) +
    coord_flip() +
    labs(color = "") +
    theme(legend.title =element_blank(),
          axis.text = element_text(size=15),
          legend.text = element_text(size=14))
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
graficar_cruce_bloques <-  function(bd, cruce, variable, vartype, filter, linea_grosor, linea_color){

  if(!is.null(filter)) {

    bd <- bd |>
      filter(!(!!rlang::sym(cruce) %in% filter))

  }

  g <- bd |>
    ggplot(aes(area=coef, fill=!!rlang::sym(variable), subgroup = coef)) +
    treemapify::geom_treemap(alpha=0.7) +
    treemapify::geom_treemap_subgroup_border(aes(), size = linea_grosor, color = linea_color) +
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
#' Graficar lollipop
#'
#' @param bd Base de datos procesada con la función analizar_sankey
#' @param orden Default orden = NULL  Recibe un vector como parametro para indicar el orden de las variables, en caso de ser nulo el orden es por nivel de porcentaje
#' @param limite_graf Default limite_graf = 1 Recibe valores decimales, los cuales indican el limite del eje X a nivel de procentaje. El 1 equivale al 100 %.
#' @param size_pct Tamaño del porcentaje mostrado
#' @param size Tamaño de la linea y el punto de la graficamostrada
#' @param width_cats Salto de linea que se aplica a las categorias mostradas
#'
#' @return
#' @export
#'
graficar_lollipops <- function(bd, orden = NULL, limite_graf = 1, width_cats = 15 , size=3, size_pct = 6) {
  g <-
    bd |>
    ggplot(aes( if(is.null(orden)) x =  reorder(respuesta, pct) else x =  factor(respuesta, levels = orden),
                y = pct)) +
    geom_segment(aes(xend = respuesta,
                     y = 0,
                     yend = pct,
                     color = respuesta),
                 linewidth = size) +
    geom_point(aes(color = respuesta),
               size = size+3) +
    geom_text(aes(label = scales::percent(pct, accuracy = 1.)),
              size = size_pct, hjust = -0.5) +
    coord_flip() +
    scale_x_discrete(labels = function(x) stringr::str_wrap(string = x, width = width_cats)) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0, limite_graf))+
    theme(plot.background = element_rect(color = "transparent", fill = "transparent"),
          panel.background = element_rect(color = "transparent", fill = "transparent"),
          legend.background = element_rect(color = "transparent", fill = "transparent") )
  return(g)
}
#' Graficar sankey
#'
#' @param bd Base de datos procesada con la función analizar_sankey
#' @param size_text_cat Tamaño del texto mostrado en cada nodo el sankey
#' @param variables Vector que contiene las llaves de las cuales se va a hacer el cruce
#' @param colores Argumento usado por scale_fill_manual y sclae_color_manual
#' @param width_text Salto de linea que se aplica a las categorias mostradas
#'
#' @return
#' @export
#'
#' @examples
#' graficar_sankey(bd = bd_estimacion, size_text_cat = 8)
graficar_sankey = function(bd, variables, colores, size_text_cat,width_text = 15){
  bd <-
    bd|>
    mutate(pos_x = ifelse(x == variables[1], as.numeric(x)- 0.1, as.numeric(x) + 0.1))

  bd |>
    ggplot(aes(x = x,
               value = value,
               next_x = next_x,
               node = node,
               next_node = next_node,
               fill = node)) +
    ggsankey::geom_sankey(flow.alpha = 0.8) +
    ggsankey::geom_sankey_text(data = bd,
                               aes(x = pos_x,
                                   label = stringr::str_wrap(string = node, width = width_text),
                                   color = node,
                                   hjust = ifelse(x == variables[1],1,0)),
                               size = size_text_cat, show.legend = F
    )  +
    guides(color = "none", fill = "none") +
    scale_fill_manual(values = colores) +
    scale_color_manual(values = colores) +
    encuestar::tema_default() +
    theme(legend.position = "none",
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          plot.background = element_rect(color = "transparent", fill = "transparent"),
          panel.background = element_rect(color = "transparent", fill = "transparent"),
          legend.background = element_rect(color = "transparent", fill = "transparent"))
}
#' Graficar nube de palabras
#'
#' @param bd Base de datos con la estructura generada por calcular_proporciones_nubes
#' @param max_size Tamano maximo de las nubes de palabras
#'
#' @return
#' @export
#'
#' @examples
graficar_nube_palabras = function(bd, max_size) {
  g <-
    bd |>
    ggplot(aes(label = categoria_corregida, size = pct, color = color)) +
    ggwordcloud::geom_text_wordcloud(area_corr = TRUE) +
    scale_size_area(max_size = max_size) +
    scale_color_identity() +
    theme(theme(plot.background = element_rect(color = "transparent", fill = "transparent"),
                panel.background = element_rect(color = "transparent", fill = "transparent"),
                legend.background = element_rect(color = "transparent", fill = "transparent")))
  return(g)
}
#' Title
#'
#' @param tabla_candidatoOpinion
#' @param orden_opinion
#'
#' @return
#' @export
#'
#' @examples
formatear_tabla_candidatoOpinion = function(tabla_candidatoOpinion, orden_opinion, etiquetas, colores_opinion, color_principal, colores_candidato, size_text_header, size_text_body, salto) {

  tot_opiniones <-
    length(orden_opinion)

  aux <-
    tabla_candidatoOpinion |>
    mutate(Candidato = stringr::str_wrap(string = Candidato, width = salto)) |>
    left_join(tibble(Candidato = names(colores_candidato), color = colores_candidato), by = "Candidato") |>
    flextable::flextable(cwidth = 3, cheight = 0.7, col_keys = names(tabla_candidatoOpinion))

  if("Conocimiento" %in% names(tabla_candidatoOpinion)) {
    aux <-
      aux %>%
      flextable::add_header_row(top = TRUE, values = c("Candidato", "Opinión", "Conocimiento"), colwidths = c(1, tot_opiniones, 1)) %>%
      flextable::merge_at(i = c(1, 2), j = c(1), part = "header") |>
      flextable::merge_at(i = c(1, 2), j = c(2 + tot_opiniones), part = "header")
  } else {
    aux <-
      aux %>%
      flextable::add_header_row(top = TRUE, values = c(etiquetas[1], etiquetas[2]), colwidths = c(1, tot_opiniones + 1)) %>%
      flextable::merge_at(i = c(1, 2), j = c(1), part = "header")
  }

  aux <-
    aux %>%
    flextable::border_outer(part = "header", border = fp_border(color = "black", width = 1)) |>
    flextable::border_inner_v(border = fp_border(color = "black", width = 1), part = "header") |>
    flextable::align(i = 1, j = 2, align = "center", part = "header") |>
    flextable::border_inner_h(part = "body", border = fp_border(color = "black", width = 1)) |>
    flextable::border_inner_v(part = "body", border = fp_border(color = "black", width = 1)) |>
    flextable::border_outer(part = "body", border = fp_border(color = "black", width = 1)) |>
    flextable::fontsize(size = size_text_header, part = "header") |>
    flextable::fontsize(size = size_text_body, part = "body") |>
    flextable::font(fontname = "Poppins", part = "all") |>
    flextable::bold(part = "header", bold = TRUE) |>
    flextable::padding(part = "body", padding.bottom = 0, padding.top = 0) |>
    flextable::autofit()

  if("Conocimiento" %in% names(tabla_candidatoOpinion)) {
    for(i in 2:(tot_opiniones + 2)) {
      aux <-
        aux %>%
        flextable::bg(i = 2, j = i , bg = colores_opinion[i-1], part = "header")
    }
  } else {
    for(i in 2:(tot_opiniones + 2)) {
      aux <-
        aux %>%
        flextable::bg(i = 2, j = i , bg = colores_opinion[i-1], part = "header")
    }
  }

  aux <-
    aux |>
    flextable::color(color = "white", part = "header", i = 2) |>
    flextable::bg(i = 1, bg = color_principal, part = "header")

  if("Conocimiento" %in% names(tabla_candidatoOpinion)) {
    for(i in 1:(length(colores_candidato))) {
      candidato <- names(colores_candidato)[i]
      aux <-
        aux %>%
        flextable::bg(i = eval(parse(text = paste0("~ Candidato == '", candidato, "'"))),
                      j = "Candidato",
                      bg = colores_candidato[i]) |>
        flextable::color(i = eval(parse(text = paste0("~ Candidato == '", candidato, "'"))),
                         j = "Candidato", color = "white", part = "body")
    }
  } else {
  }
  aux <-
    aux %>%
    flextable::color(color = "#2C423F", part = "body")
  return(aux)
}

formatear_tabla_votoCruzado = function(tabla_votoCruzado, var1, var2, filtro_var2, etiquetas,
                                       colores_var1,
                                       colores_var2,
                                       size_text_header,
                                       size_text_body,
                                       salto){

  ncols <-
    {
      if(is.null(filtro_var2)) {
        tabla_votoCruzado |>
          select(!var1) |>
          ncol()
      } else {
        tabla_votoCruzado |>
          select(!var1) |>
          ncol()
      }
    }

  aux <-
    tabla_votoCruzado |>
    flextable::flextable(cwidth = 3, cheight = 0.7, col_keys = names(tabla_votoCruzado)) |>
    flextable::add_header_row(top = TRUE,
                              values = c(etiquetas[1], etiquetas[2]),
                              colwidths = c(1, ncols)) |>
    flextable::merge_at(i = c(1, 2), j = c(1), part = "header") |>
    flextable::border_outer(part = "header", border = officer::fp_border(color = "black", width = 1)) |>
    flextable::border_inner_v(border = officer::fp_border(color = "black", width = 1), part = "header") |>
    flextable::align(i = 1, j = 2, align = "center", part = "header") |>
    flextable::align(align = "center", part = "body") |>
    flextable::align(j = 1, align = "left", part = "body") |>
    flextable::border_inner_h(part = "body", border = officer::fp_border(color = "black", width = 1)) |>
    flextable::border_inner_v(part = "body", border = officer::fp_border(color = "black", width = 1)) |>
    flextable::border_outer(part = "body", border = officer::fp_border(color = "black", width = 1)) |>
    flextable::fontsize(size = size_text_header, part = "header") |>
    flextable::fontsize(size = size_text_body, part = "body") |>
    flextable::font(fontname = "Poppins", part = "all") |>
    flextable::bold(part = "header", bold = TRUE) |>
    flextable::padding(part = "body", padding.bottom = 0, padding.top = 0)
  # ?flextable::autofit()

  for(i in 1:(ncols)) {
    j <- which(names(tabla_votoCruzado |>
                       select(!var1)) == names(colores_var2)[i])
    aux <-
      aux %>%
      flextable::bg(i = 2,
                    j = j + 1,
                    bg = colores_var2[i],
                    part = "header")
  }

  aux <-
    aux |>
    flextable::color(color = "white", part = "header", i = 2) |>
    flextable::bg(i = 1, bg = "white", part = "header")

  for(i in 1:length(colores_var1)) {
    color <- names(colores_var1)[i]
    aux <-
      aux %>%
      flextable::bg(i = eval(parse(text = paste0("~", var1, " == '", color, "'"))),
                    j = var1,
                    bg = colores_var1[i]) |>
      flextable::color(i = eval(parse(text = paste0("~ ", var1, " == '", color, "'"))),
                       j = var1,
                       color = "white",
                       part = "body")
  }
  aux <-
    aux |>
    flextable::color(color = "#2C423F", part = "body", j = 2:ncols)
  return(aux)
}
