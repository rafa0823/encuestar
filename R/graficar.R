if(getRversion() >= "2.15.1")  utils::globalVariables(c("grupo"))
#' Tema transparente para usarse en objetos tipo [ggplot] que vayan a ser exportados
#'
#' Tema utilizado para agregar transparencia al layout del grafico generado.
#'
#' @return Tema de [ggplot2] que agrega transparencia a los siguientes elementos del grafico:
#'  legend.backgroud, panel.background, plot.background, strip.background
#' @export
#' @examples
#' encuesta_demo$muestra$diseno$variables |> dplyr::count(voto_pr_24) |> na.omit() |>  ggplot2::ggplot(ggplot2::aes(x = reorder(voto_pr_24, n), y = n, fill = voto_pr_24)) + ggplot2::geom_col() + ggplot2::coord_flip()
#' encuesta_demo$muestra$diseno$variables |> dplyr::count(voto_pr_24) |> na.omit() |>  ggplot2::ggplot(ggplot2::aes(x = reorder(voto_pr_24, n), y = n, fill = voto_pr_24)) + ggplot2::geom_col() + ggplot2::coord_flip() + encuestar::tema_transparente()
tema_transparente <- function(){
  ggplot2::theme(legend.background = element_rect(color = "transparent", fill = "transparent"),
                 panel.background = element_rect(color = "transparent", fill = "transparent"),
                 plot.background = element_rect(color = "transparent", fill = "transparent"),
                 strip.background = element_rect(color = "transparent", fill = "transparent"))
}
#' Tema usado como identidad grafica en los entregables de Morant
#'
#' Tema de [ggplot2] que contiene el formato de entregables para objetos [ggplot2]
#'
#' @param base_family Parametro de [ggthemes::theme_foundation()] usado como fuente tipografica
#'
#' @return Tema de [ggplot2]
#' @export
#' @examples
#' encuesta_demo$muestra$diseno$variables |> dplyr::count(voto_pr_24) |> na.omit() |>  ggplot2::ggplot(ggplot2::aes(x = reorder(voto_pr_24, n), y = n, fill = voto_pr_24)) + ggplot2::geom_col() + ggplot2::coord_flip()
#' encuesta_demo$muestra$diseno$variables |> dplyr::count(voto_pr_24) |> na.omit() |>  ggplot2::ggplot(ggplot2::aes(x = reorder(voto_pr_24, n), y = n, fill = voto_pr_24)) + ggplot2::geom_col() + ggplot2::coord_flip() + encuestar::tema_morant()
tema_morant <- function(base_family = "Poppins") {
  (ggthemes::theme_foundation(base_size = 15,
                              base_family = base_family) +
     theme(
       line = element_line(colour = "#4C5B61"),
       rect = element_rect(fill = "#FFFFFF", linetype = 0, colour = NA),
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
#' Gráfica de barras horizontales
#'
#' Recibe un [tibble()] y genera un objeto tipo [ggplot]. El producto es una grafica de barras
#'  ordenadas de acuerdo al criterio indicado.
#'
#' @param bd Base de datos con una variable categórica (respuesta) y una numérica (media).
#' @param salto Número entero, se aplica un stringr::str_wrap a la variable categórica.
#' @param porcentajes_fuera Si es T, las labels de los porcentajes aparecen fuera (o sobre) las barras.
#' @param desplazar_porcentajes Si porcentajes_fuera es T, este parametro ajusta las etiquetas de texto.
#' @param orden_respuestas Vector ordenado tipo caracter usado para ordenar los valores del eje x
#'
#' @return Objeto tipo [ggplot]
#'
#' @examples
#' encuestar:::analizar_frecuencias(diseno = encuesta_demo$muestra$diseno, pregunta = "voto_pm_24") |> encuestar:::graficar_barras()
#' encuestar:::analizar_frecuencias(diseno = encuesta_demo$muestra$diseno, pregunta = "seguridad_sonora") |> encuestar:::graficar_barras()
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
#' Graficar lollipop
#'
#' Recibe un [tibble()] y genera un objeto tipo [ggplot]. El producto es una grafica de barras
#'  ordenadas de acuerdo al criterio indicado.
#'
#' @param bd [tibble()] que contiene las variables necesarias para
#' @param orden Vector ordenado tipo caracter que define el orden de las categorias en el eje x
#' @param limits Vector numerico de longitud dos que indica los limites en escala porcentual natural del eje y usando el parametro [limits] de la funcion [ggplot2::scale_y_continuous()]
#' @param size_pct Parametro [size] de la funcion [ggfittext::geom_text()] que controla el tamano del texto que muestra el porcentaje fuera de las lolipos
#' @param size Parametro [size] de la funcion [ggfittext::geom_segment()] y [ggfittext::geom_point()] que controla el tamano del texto que define el tamaño de la linea y el punto de la grafica
#' @param width_cats Valor entero usato como parametro en [stringr::str_wrap()] aplicado a las filas del eje x
#'
#' @return Objeto tipo [ggplot]
#'
#' @examples
#' encuestar:::analizar_frecuencias(diseno = encuesta_demo$muestra$diseno, pregunta = "problema_principal") |>  dplyr::rename(pct = media) |>  encuestar:::graficar_lollipops() + encuestar::tema_morant()
#' encuestar:::analizar_frecuencias(diseno = encuesta_demo$muestra$diseno, pregunta = "problema_inseguridad") |> dplyr::rename(pct = media) |> encuestar:::graficar_lollipops() + encuestar::tema_morant()
graficar_lollipops <- function(bd, orden = NULL, limits = c(0., 1.0), width_cats = 15 , size = 3, size_pct = 6) {
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
                       limits = c(0,limits)) +
    theme(plot.background = element_rect(color = "transparent", fill = "transparent"),
          panel.background = element_rect(color = "transparent", fill = "transparent"),
          legend.background = element_rect(color = "transparent", fill = "transparent") )
  return(g)
}
if(getRversion() >= "2.15.1")  utils::globalVariables(c("familia"))
#' Graficar gauge (donita)
#'
#' Recibe un [tibble()] y genera un objeto tipo [ggplot]. El producto es una grafica de gauge (donita)
#'  que se rellena acuerdo al valor contenido en la escala indicada
#'
#' @param bd  Base de datos con un único row. La variable a considerar se llama "media"
#' @param color_principal Color principal de la barra
#' @param color_secundario Color secundario de la barra
#' @param escala Máximo y mínimo de los valores que puede tomar "media"
#' @param size_text_pct Tamaño del texto dentro del gauge
#'
#' @return Objeto tipo [ggplot]
#'
#' @examples
#' encuestar:::analizar_frecuencias(diseno = encuesta_demo$muestra$diseno, pregunta = "si_voto_24") |> encuestar:::graficar_gauge(color_principal = "red", escala = c(0, 10), size_text_pct = 16)
#' encuestar:::analizar_frecuencias(diseno = encuesta_demo$muestra$diseno, pregunta = "conoce_pm_astiazaran") |> filter(respuesta == "Sí") |>  encuestar:::graficar_gauge(color_principal = "red", escala = c(0, 1), size_text_pct = 16)
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
#' Graficar intervalos
#'
#' Recibe un [tibble()] y genera un objeto tipo [ggplot]. El producto es una grafica de puntos [ggplot2::geom_point()]
#'  con intervalos asociados a los intervalos de confianza
#'
#' @param bd Base de datos con una variable categórica (respuesta) y una numérica (media).
#' @param point_size Tamaño del punto que indica el promedio de la estimación.
#' @param text_point_size Tamaño del texto que acompaña el valor de la estimación
#' @param escala Vector ordenado numerico de valores minimos y maximos asociados a la variable
#'
#' @return Objeto tipo [ggplot]
#'
#' @examples
#' encuestar:::analizar_frecuencias_aspectos(diseno = encuesta_demo$muestra$diseno, diccionario = encuesta_demo$cuestionario$diccionario, patron_pregunta = "afirmacion", aspectos = c("seguridad", "economia", "pais", "hermosillo")) |> dplyr::left_join(encuesta_demo$cuestionario$diccionario |> dplyr::select(aspecto = llaves, tema), by = "aspecto") |> encuestar:::graficar_intervalo_numerica(escala = c(1, 5), point_size = 1, text_point_size = 14)
graficar_intervalo_numerica <- function(bd, escala = c(0, 10), point_size = 1, text_point_size = 8){
  g <-
    bd %>%
    ggplot(aes(y = media, x = stats::reorder(str_wrap(tema, 40), media))) +
    geom_pointrange(aes(ymin = inf, ymax = sup), color = "#850D2D", size = point_size)
  if(escala[2] == 1) {
    g <-
      g +
      geom_text(aes(label = scales::percent(x = media, accuracy = 1.0)),
                nudge_x = .3, size = text_point_size)
  } else {
    g <-
      g +
      geom_text(aes(label = round(media, digits = 2)),
                nudge_x = .3, size = text_point_size)
  }
  g <-
    g +
    coord_flip() +
    labs(title = NULL,
         x = NULL,
         y = NULL) +
    scale_y_continuous(limits = c(escala[1], escala[2]))
  return(g)
}
#' Graficar heatmap (geom_tile)
#'
#' Recibe un [tibble()] y genera un objeto tipo [ggplot]. El producto es una grafica tipo heatmap de
#'  [ggplot2::geom_tile()]
#'
#' @param bd Base de datos con las variables requeridas
#' @param orden_x Orden de los posibles valores de la variable en el eje x
#' @param orden_y Orden de los posibles valores de la variable en el eje x
#' @param color Color principal del hatpmap
#' @param salto_x Valor entero usato como parametro en [stringr::str_wrap()] aplicado a las filas del eje x
#' @param salto_y Valor entero usato como parametro en [stringr::str_wrap()] aplicado a las filas del eje y
#' @param caption Cadena de texto usada en el parametro [caption] de la funcion [ggplot2::labs()]
#' @param size_text_x Parametro [size] de la funcion [ggplot2::element_text()] usado en el parametro [axis.text.x] en el tema particular del grafico
#' @param size_text_y Parametro [size] de la funcion [ggplot2::element_text()] usado en el parametro [axis.text.y] en el tema particular del grafico
#' @param size_text_caption Parametro [size] de la funcion [ggplot2::element_text()] usado en el parametro [plot.caption] en el tema particular del grafico
#'
#' @return Objeto tipo [ggplot]
#'
#' @examples
#' encuestar:::analizar_cruce(diseno = encuesta_demo$muestra$diseno, variable_principal = "sexo", variable_secundaria = "asunto_seguridad", vartype = "cv") |> dplyr::rename(var_y = sexo, var_x = asunto_seguridad, media = coef) |> encuestar::graficar_heatmap(orden_x = c("Mejor", "Peor", "Sigue igual", "Ns/Nc"),orden_y = c("F", "M"))
#' @export
graficar_heatmap <- function(bd, orden_x, orden_y, color = "blue", caption = "", salto_x = 25, salto_y = 25, size_text_x = 14, size_text_y = 16, size_text_caption = 14){
  g <-
    bd |>
    ggplot(aes(x = factor(var_x, levels = orden_x),
               y = factor(var_y, levels = orden_y),
               fill = media,
               label = scales::percent(media, 1.))) +
    geom_tile(color = "white") +
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
#' Recibe un [tibble()] y genera un objeto tipo [ggplot]. El producto es una grafica tipo barras horizontales
#'  con el grafico tipo burbuja y el grafico tipo  barras para los porcentajes de No sabe No contesta
#'
#' @param bd Base de datos con estructura producida por [encuestar:::analizar_frecuencias_aspectos()].
#' @param ns_nc Valor de la variable 'respuesta' asociado a "No sabe" o "No contesta" en la pregunta relativa.
#' @param regular Valor de la variable 'respuesta' asociado a "Regular" en la pregunta relativa.
#' @param grupo_positivo Conjunto de valores de la  variable 'respuesta' considerados como positivos.
#' @param grupo_negativo Conjunto de valores de la  variable 'respuesta' considerados como negativos
#' @param colores Vector ordenado de colores asociados al grupo negativo, regular y positivo.
#' @param burbuja Base de datos con estructura producida por analizar_frecuencias_aspectos filtrada por un valor de interés. Disponible en el enviroment.
#' @param color_burbuja Color de los puntos asociados a la base de datos 'burbuja'
#' @param size_burbuja Es el tamaño máximo de las burbujas en la gráfica.'
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
#' @param color_nsnc Color para usar en parametro [fill] en el grafico de barras asociado a la cateogira No sabe o No contesta
#' @param size_pct Parametro [size] de la funcion [ggfittext::geom_fit_text()] que controla el tamano del texto que muestra el porcentaje dentro de las barras
#' @param mostrar_nsnc Logical. Muestra u oculta la grafica de barras asociada a la cateogira No sabe o No contesta
#' @param salto_respuestas Valor entero usato como parametro en [stringr::str_wrap()] aplicado a todos las leyendas del grafico
#' @param orden_cat Vector ordenado tipo caracter asociado a los aspectos de las variables de opinion
#' @param patron_inicial PENDIENTE
#'
#' @return Objeto tipo [ggplot] compuesto por la union de al menos dos
#'
#' @import patchwork
#' @examples
#' encuestar:::analizar_frecuencias_aspectos(diseno = encuesta_demo$muestra$diseno, diccionario = encuesta_demo$cuestionario$diccionario, patron_pregunta = "opinion_pm", aspectos = c("astiazaran", "delrio")) |> dplyr::left_join(encuesta_demo$cuestionario$diccionario |> dplyr::select(aspecto = llaves, tema)) |> encuestar:::graficar_candidato_opinion(ns_nc = "Ns/Nc", regular = "Regular", grupo_positivo = c("Muy buena", "Buena"), grupo_negativo = c("Muy mala", "Mala"), colores = c("Muy buena" = "green", "Buena" = "yellow", "Regular" = "blue", "Mala" = "orange", "Muy mala" = "red"), color_nsnc = "gray70", tema = encuestar:::tema_morant(), burbuja = NA, orden_resp = c("Muy buena", "Buena", "Regular", "Mala", "Muy mala"), size_pct = 12, caption_opinion = "Opinon", size_text_cat = 16, size_caption_opinion = 12, caption_nsnc = "Ns/Nc", size_caption_nsnc = 14, salto_respuestas = 100)
graficar_candidato_opinion <- function(bd, ns_nc, regular,
                                       grupo_positivo,
                                       grupo_negativo,
                                       colores,
                                       color_nsnc,
                                       burbuja,
                                       color_burbuja,
                                       size_burbuja = 8,
                                       caption_opinion = "",
                                       caption_nsnc = "Ns/Nc",
                                       caption_burbuja,
                                       size_caption_opinion = 12,
                                       size_caption_nsnc = 14,
                                       size_caption_burbuja,
                                       size_text_cat = 16,
                                       size_pct = 12,
                                       orden_resp,
                                       salto = 200,
                                       tema,
                                       mostrar_nsnc = T,
                                       salto_respuestas = 100,
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
      scale_size_area(max_size = size_burbuja) +
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
        final <- g_opinion + g_burbuja + b + patchwork::plot_layout(widths = c(.7, .15, .15), ncol= 3)
      } else {
        final <- g_opinion + g_burbuja + patchwork::plot_layout(widths = c(.7, .15, .15), ncol = 3)
      }
    } else{
      if(mostrar_nsnc) {
        final <- g_opinion + b + patchwork::plot_layout(widths = c(.8, .2))
      } else {
        final <- g_opinion
      }

    }

  } else{
    if(!all(is.na(burbuja))){
      final <- g_opinion + g_burbuja + patchwork::plot_layout(widths = c(.8,.2))
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
#' Recibe un [tibble()] y genera un objeto tipo [ggplot]. El producto es una grafica tipo barras horizontales
#'  combinada con una grafica tipo intervalos. La grafica tipo barras muestra los porcentajes con de
#'  con los que asocian a determinado personaje. La grafica de intervalos muestra el nivel de conocimiento
#'  del mismo.
#'
#' @param bases Lista. 'bases$conoce' contiene la estimación del conocimiento sobre algún personaje. 'bases$partido' tiene la información sobre la asociación a un partido político por personaje
#' @param cliente Vector de códigos cortos que asocian a un personaje con una variable. Por ejemplo, 'personajeC' resaltará entre el conjunto 'personajeA', 'personajeB', 'personajeC'.
#' @param tipo_conoce Tipo de gráfica a mostrar en la estimación de conocimiento. De forma predeterminada son barras, el otro valor son 'intervalos'
#' @param colores_candidato Vector que asigna un color a un candidato de acuerdo al nombre largo (tema)
#' @param solo_respondidos Logical. Omite los partidos en los cuales el personaje no tiene ninguna asociación
#' @param colores_partido Vector que asigna un color a cada partido de acuerdo al nombre largo (tema)
#' @param tema Tema de la gráfica asociado a la paquetería 'encuestar'
#'
graficar_candidatoPartido <- function(bases, cliente, tipo_conoce, colores_candidato, solo_respondidos = T, colores_partido, tema,corte_vis = 0.0){
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
      tema_morant()
  } else{
    a <- bases$conoce %>% ggplot(aes(x = tema, y = media, fill = tema)) +
      # geom_col(show.legend = F) +
      ggchicklet::geom_chicklet(width = .6, alpha =.5, show.legend = F)+
      ggfittext::geom_bar_text(aes(label = scales::percent(media,1))) +
      scale_fill_manual(values = colores_candidato) +
      labs(title = "Conocimiento", y = NULL,x = NULL ) +
      coord_flip() +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0,max(bases$conoce$media), by = .1)) +
      tema_morant()
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
    geom_text(data = bases$partido |> filter(!respuesta %in% c("Ns/Nc"), !tema %in% cliente, corte_vis < media ),
              aes(x = label, y = as.numeric(tema), label = scales::percent(media,accuracy = 1)),
              color = "white", fontface = "plain") +
    geom_text(data = bases$partido %>% filter(tema %in% cliente, corte_vis < media ),
              aes(x = label, y = as.numeric(tema), label = scales::percent(media,accuracy = 1)),
              color = "white", fontface = "bold") +
    geom_text(data = bases$partido %>% filter(respuesta %in% c("Ns/Nc"), corte_vis < media ),
              aes(x = label, y = as.numeric(tema), label = scales::percent(media,accuracy = 1)),
              color = "white", fontface = "bold") +
    scale_fill_manual(values = colores_partido) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
    # geom_text(aes(x = 0, y = as.numeric(tema), label = tema), hjust = 0) +
    labs( y = "", title = "Identificación partidista", x= NULL) +
    tema_morant() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank())

  a + b + patchwork::plot_layout(widths = c(.2,.8)) &
    tema_transparente()
}
#' Graficar barras saldo
#'
#' Recibe un [tibble()] y genera un objeto tipo [ggplot]. El producto es una grafica tipo barras horizontales
#'  diferenciando dos grupos principales: positivos y negativos. Los resultados mostrados omiten la
#'  categoria de No sabe No contesta
#'
#' @param bd [tibble()] que contiene las variables necesarias para generar la grafica
#' @param orden Vector ordenado tipo caracter usado para ordenar los valores del eje x
#' @param grupo_positivo Conjunto de valores de la  variable 'respuesta' considerados como positivos.
#' @param grupo_negativo Conjunto de valores de la  variable 'respuesta' considerados como negativos.
#' @param colores Vector ordenado de colores asociados al grupo negativo, regular y positivo.
#' @param salto_respuestas Valor entero usato como parametro en [stringr::str_wrap()] aplicado a todos las leyendas del grafico
#' @param salto_tema Valor entero usato como parametro en [stringr::str_wrap()] aplicado a todos las categorias del eje x
#' @param caption_opinion Cadena de texto usada en el parametro [caption] de la funcion [ggplot2::labs()]
#' @param size_text_cat Parametro [size] de la funcion [ggplot2::element_text()] usado en el parametro [axis.text.x] en el tema particular del grafico
#' @param size_caption_opinion Parametro [size] de la funcion [ggplot2::element_text()] usado en el parametro [plot.caption] en el tema particular del grafico
#' @param tema Tema grafico de [ggplot2]
#' @param Regular Valor de la variable 'respuesta' asociado a "Regular" en la pregunta relativa.
#' @param size_pct Parametro [size] de la funcion [ggfittext::geom_fit_text()] que controla el tamano del texto que muestra el porcentaje dentro de las barras
#'
#' @return Objeto tipo [ggplot]
graficar_barras_saldo <- function(bd, orden, grupo_positivo, grupo_negativo, Regular, colores, salto_respuestas, salto_tema, caption_opinion, size_text_cat = 10, size_pct, size_caption_opinion,size_text_legend = 10 ,tema = encuestar::tema_morant()){

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
          legend.key.size = unit(1, units = "cm"),
          legend.text = element_text(size = size_text_legend))
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
#' @return Objeto tipo [ggplot]
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
  return(g)
}
#' Graficar de lineas
#'
#' Recibe un [tibble()] y genera un objeto tipo [ggplot]. El producto es una grafica de lineas.
#'
#' @param bd Base de datos con las variables necesarias var_x, var_y, media
#' @param orden_var_x Orden de las categorias en el eje x
#' @param salto_x Valor entero usato como parametro en [stringr::str_wrap()] aplicado a todos las categorias del eje x
#' @param salto_legend Valor entero usato como parametro en [stringr::str_wrap()] aplicado a todos las categorias del eje x
#' @param limits Vector numerico que indica los limites en escala porcentual natural del eje y
#' @param text_nudge_y Parametro [nudge_y] de la funcion [ggrepel::geom_text_repel()] que modifica la posicion del texto en el grafico
#' @param size_text Parametro [size] de la funcion [ggplot2::element_text()] usado en el parametro [axis.text.x] en el tema particular del grafico
#' @param colores_var_y Vector que contiene los codigos de colores para cada categoria de la variable y
#'
#' @return Objeto tipo [ggplot]
#'
#' @examples
#' encuestar:::analizar_cruce(diseno = encuesta_demo$muestra$diseno, variable_principal = "region", variable_secundaria = "voto_pr_24", vartype = "cv") |> na.omit() |> dplyr::rename(var_x = region, var_y = voto_pr_24, media = coef) |> encuestar:::graficar_lineas(orden_var_x = c("Perdidas", "Competitivas", "Voto Blando", "Voto Duro"), colores_var_y = c("Claudia Sheinbaum por MORENA-PT-Partido Verde" = "#A6032F", "Xóchitl Gálvez por PAN-PRI-PRD" = "#0339a6", "No recuerda" = "gray40","No contesta" = "gray60", "Jorge Álvarez Máynez por Movimiento Ciudadano" = "#F27405", "Anulé mi voto" = "black"), limits = c(0, 0.5))
graficar_lineas <- function(bd, orden_var_x, colores_var_y, salto_x = 25, salto_legend = 25, limits = c(0, 0.75), text_nudge_y = 0.01, size_text = 8){
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
    labs(color = "")
  return(g)
}
#' Graficar conocimiento de personajes por región o estrato
#'
#' @param bd Base de datos resultado de la función 'analizar_conocimientoRegion'
#' @param ordenRegiones Vector que indica el orden de las columnas del geom_tile
#' @param salto_labelRegiones Parámetro 'width' usado por stringr::str_wrap en las etiquetas superiores
#'
#' @return
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
#' Graficar lolipos para resaltar diferencias
#'
#' Recibe un [tibble()] y genera un objeto tipo [ggplot]. El producto es una grafica tipo lolipo
#' que resalta las diferencias entre dos categorias
#'
#' @param orden_variablePrincipal Factor de la variable principal
#' @param bd Base de datos producto de la función 'analizar_crucePuntos'
#' @param colores_variables_secundarias Vector que contiene los codigos de colores para cada categoria de la variable secundaria
#' @param nudge_x Parametro [nudge_y] de la funcion [ggplot2::geom_text()] que modifica la posicion del texto en el grafico
#' @param size_geom_text Parametro [size] de la funcion [ggplot2::geom_text()] que modifica la posicion del texto en el grafico
#' @param caption Cadena de texto usada en el parametro [caption] de la funcion [ggplot2::labs()]
#' @param wrap_y Valor entero usato como parametro en [stringr::str_wrap()] aplicado a todos las categorias del eje y
#' @param wrap_caption Valor entero usato como parametro en [stringr::str_wrap()] aplicado al [caption] del grafico
#' @param limits Vector numerico que indica los limites en escala porcentual natural del eje y
#'
#' @return Objeto tipo [ggplot]
#'
#' @examples
#' encuestar:::analizar_cruce_aspectos(diseno = encuesta_demo$muestra$diseno, variable_principal = "sexo", variables_secundarias = paste0("conoce_pm_", c("astiazaran", "delrio")), filtro_variables_secundarias = "Sí", vartype = "cv") |> dplyr::left_join(encuesta_demo$cuestionario$diccionario |> dplyr::distinct(llaves, tema), by = c("variable" = "llaves")) |> dplyr::select(!variable) |> dplyr::rename(variable_principal = sexo) |> encuestar:::graficar_lolipop_diferencias(orden_variablePrincipal = c("F", "M"), colores_variables_secundarias = c("Antonio \"Toño\" Astiazarán" = "red", "María Dolores Del Río" = "blue"), caption = "", wrap_y = 25, wrap_caption = 25, limits = c(0, 0.8)) + encuestar::tema_morant()
graficar_lolipop_diferencias <- function(bd, orden_variablePrincipal, colores_variables_secundarias,
                                         nudge_x = 0.05, size_geom_text = 6,
                                         caption = "", wrap_y = 25, wrap_caption = 25, limits = c(0, 0.75)) {
  g <-
    bd |>
    ggplot(aes(x = factor(variable_principal, levels = orden_variablePrincipal),
               y = mean,
               color = tema,
               group = variable_principal)) +
    geom_line(color = "#a2d2ff", linewidth = 4.5, alpha = 0.5) +
    geom_point(size = 7) +
    geom_text(aes(label = scales::percent(x = mean, accuracy = 1.0)),
              nudge_x = nudge_x,
              size = size_geom_text) +
    coord_flip() +
    labs(color = "",
         caption = stringr::str_wrap(string = caption, width = wrap_caption)) +
    scale_x_discrete(labels = function(x) stringr::str_wrap(string = x, width = wrap_y)) +
    scale_y_continuous(labels = scales::percent,
                       limits = limits) +
    scale_color_manual(values = colores_variables_secundarias)
  return(g)
}
#' Graficar cruce por bloques
#'
#' Recibe un [tibble()] y genera un objeto tipo [ggplot]. El producto es una grafica bloques usando
#' [treemapify::geom_treemap()]. Este tipo de grafico es util para cruces cuya variable principal
#' es de pocos valores como el sexo.
#'
#' @param bd Base de datos producto de la función 'analizar_cruce'
#' @param cruce Variable principal que agrupa los resultados de las estimaciones
#' @param variable Variable secundaria
#' @param colores_variable_secundaria Vector que contiene los codigos de colores para cada categoria de la variable secundaria
#' @param vartype Parametro de [srvyr::survey_mean()] que reporta la variabilidad de la estimacion
#' @param filter Valores de interes de la variable secundaria a mostrar
#' @param linea_grosor Parametro [size] de la funcion [treemapify::geom_treemap_subgroup_border()] que modifica el grosor de las lineas que dividen los subgrupos de un bloque
#' @param linea_color Parametro [color] de la funcion [treemapify::geom_treemap_subgroup_border()] que modifica el color de las lineas que dividen los subgrupos de un bloque
#'
#' @return Objeto tipo [ggplot]
#'
#' @examples
#' encuestar:::analizar_cruce(diseno = encuesta_demo$muestra$diseno, variable_principal = "sexo", variable_secundaria = "voto_pr_24", vartype = "cv") |> encuestar:::graficar_cruce_bloques(cruce = "sexo", variable = "voto_pr_24", colores_variable_secundaria = c("Claudia Sheinbaum por MORENA-PT-Partido Verde" = "#A6032F", "Xóchitl Gálvez por PAN-PRI-PRD" = "#0339a6", "No recuerda" = "gray40","No contesta" = "gray60", "Jorge Álvarez Máynez por Movimiento Ciudadano" = "#F27405", "Anulé mi voto" = "black"), vartype = "cv", filter = NULL, linea_grosor = 2)
graficar_cruce_bloques <- function(bd, cruce, variable, colores_variable_secundaria, vartype, filter = NULL, linea_grosor = 2, linea_color = "white"){
  if(!is.null(filter)) {
    bd <-
      bd |>
      filter(!(!!rlang::sym(cruce) %in% filter))
  }
  g <-
    bd |>
    ggplot(aes(area = coef,
               fill = !!rlang::sym(variable),
               subgroup = coef)) +
    treemapify::geom_treemap(alpha = 0.7) +
    treemapify::geom_treemap_subgroup_border(aes(),
                                             size = linea_grosor,
                                             color = linea_color) +
    facet_wrap(rlang::as_label(rlang::sym(cruce)))

  if(vartype == "cv"){
    g <-
      g +
      treemapify::geom_treemap_text(aes(label = paste0(!!ensym(variable), ", ",
                                                       scales::percent(coef,accuracy = 1),
                                                       pres)),
                                    place = "centre",
                                    grow = TRUE,
                                    reflow = TRUE,
                                    show.legend = FALSE,
                                    color = "white",
                                    family = "Poppins")
  } else {
    g <-
      g +
      treemapify::geom_treemap_text(aes(label = paste0(!!ensym(variable),
                                                       ", ",
                                                       scales::percent(coef, accuracy = 1))),
                                    place = "centre",
                                    grow = TRUE,
                                    reflow = TRUE,
                                    show.legend = FALSE,
                                    color="white",
                                    family = "Poppins")
  }
  g <-
    g +
    scale_fill_manual(values = colores_variable_secundaria) +
    theme(legend.position = "none")
  return(g)
}
#' Graficar sankey
#'
#' @param bd Base de datos procesada con la función [encuestar:::analizar_sankey()]
#' @param size_text_cat Parametro [size] de la funcion [treemapify::geom_treemap_subgroup_border()] que modifica el grosor de las lineas que dividen los subgrupos de un bloque
#' @param variables Vector ordenado tipo caracter que contiene los nombres de las llaves de las cuales se va a hacer el cruce
#' @param colores Argumento usado por scale_fill_manual y sclae_color_manual
#' @param width_text Salto de linea que se aplica a las categorias mostradas
#'
#' @return
#'
#' @examples
#' graficar_sankey(bd = bd_estimacion, size_text_cat = 8)
graficar_sankey <- function(bd, variables, colores, size_text_cat,width_text = 15){
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
    encuestar::tema_morant() +
    theme(legend.position = "none",
          axis.text.y = element_blank(),
          axis.title.x = element_blank())
}
#' Graficar nube de palabras
#'
#' @param bd Base de datos con la estructura generada por calcular_proporciones_nubes
#' @param max_size Tamano maximo de las nubes de palabras
#'
#' @return
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
#'
#' @examples
formatear_tabla_candidatoOpinion = function(tabla_candidatoOpinion, orden_opinion, etiquetas, colores_opinion, color_principal, colores_candidato, size_text_header, size_text_body, salto, color_conocimiento) {
  # browser()

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
      flextable::add_header_row(top = TRUE, values = c("Candidato", "Opinión", "Conocimiento"), colwidths = c(1, tot_opiniones + 1, 1)) %>%
      flextable::merge_at(i = c(1, 2), j = c(1), part = "header") |>
      flextable::merge_at(i = c(1, 2), j = c(2 + tot_opiniones + 1), part = "header")
  } else {
    aux <-
      aux %>%
      flextable::add_header_row(top = TRUE, values = c(etiquetas[1], etiquetas[2]), colwidths = c(1, tot_opiniones + 1)) %>%
      flextable::merge_at(i = c(1, 2), j = c(1), part = "header")
  }

  aux <-
    aux %>%
    flextable::border_outer(part = "header", border = officer::fp_border(color = "black", width = 1)) |>
    flextable::border_inner_v(border = officer::fp_border(color = "black", width = 1), part = "header") |>
    flextable::align(i = 1, j = 2, align = "center", part = "header") |>
    flextable::border_inner_h(part = "body", border = officer::fp_border(color = "black", width = 1)) |>
    flextable::border_inner_v(part = "body", border = officer::fp_border(color = "black", width = 1)) |>
    flextable::border_outer(part = "body", border = officer::fp_border(color = "black", width = 1)) |>
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

  if("Candidato" %in% names(tabla_candidatoOpinion) | "Candidatos" %in% names(tabla_candidatoOpinion) ) {
    for(i in 1:(length(colores_candidato))) {
      candidato <- names(colores_candidato)[i]
      candidato <- stringr::str_wrap(string = candidato, width = salto)

      aux <-
        aux %>%
        flextable::bg(i = eval(parse(text = paste0("~ Candidato == '", candidato, "'"))),
                      j = "Candidato",
                      bg = colores_candidato[i]) |>
        flextable::color(i = eval(parse(text = paste0("~ Candidato == '", candidato, "'"))),
                         j = "Candidato", color = "white", part = "body")
    }
  }
  if("Conocimiento" %in% names(tabla_candidatoOpinion)) {
    aux <-
      aux %>%
      flextable::bg(i = 1,
                    j = "Conocimiento", part = "header",
                    bg = color_conocimiento) |>
      flextable::color(i = 1,
                       j = "Conocimiento",  part = "header",
                       color = "white")
  }

  aux <-
    aux %>%
    flextable::color(color = "#2C423F", part = "body")
  return(aux)
}

#' Title
#'
#' @param tabla_votoCruzado
#' @param var1
#' @param var2
#' @param filtro_var2
#' @param etiquetas
#' @param colores_var1
#' @param colores_var2
#' @param size_text_header
#' @param size_text_body
#' @param salto
#'
#' @return
#'
#' @examples
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
