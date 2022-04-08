
#' Title
#'
#' @param dicc
#'
#' @return
#' \item{a}{Grafica de cascada para saber cuantas preguntas por bloque hay y en total.}
#' \item{b}{Grafica indicando cuales son las etiquetas y cuantos aspectos hay para cada pregunta de aspectos.}
#' \item{c}{Grafica gant, se muestra todo el cuestionario con sus respectivas categorias en una sola grafica.}
#' @export
#'
#' @examples

resumen_cuestionario <- function(dicc){

  # Preguntas por bloque ----------------------------------------------------

  a <- rainfall_p(dicc)

  # Aspectos ----------------------------------------------------------------

  b <- aspectos_dicc(dicc)

  # Preguntas por tipo ------------------------------------------------------

  c <- gant_p_r(dicc)


  return(list(a, b, c))
}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("end","ini"))

#' Title
#'
#' @param dicc
#'
#' @return
#' @export
#'
#' @examples

rainfall_p <- function(dicc){

  bl <- dicc %>% count(bloque) %>%
    mutate(end = cumsum(n)) %>%
    mutate(ini = lag(end,default = 0))

  max <- bl %>% filter(end == max(end)) %>% pull(end)
  min <- bl %>% filter(end == min(end)) %>% pull(bloque)
  a <- bl %>%
    ggplot(aes(x = bloque)) +
    geom_tile(aes(y = ini),alpha = 0) +
    geom_rect(aes(xmin = as.numeric(bloque)-.5, xmax = as.numeric(bloque)+.5, ymin = ini, ymax = end)) +
    geom_hline(yintercept = max ) +
    geom_label(aes(y = end, label = n)) +
    annotate("label", x = min, y = max, label = glue::glue("{max} preguntas")) +
    theme_minimal() +
    labs(x = NULL, y = NULL) +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("pregunta", "preguntas"))

#' Title
#'
#' @param dicc
#'
#' @return
#' @export
#'
#' @examples

aspectos_dicc <- function(dicc){
  c <- dicc %>% filter(str_detect(llaves,"_")) %>% separate(llaves, into = c("aspecto", "pregunta")) %>%
    group_by(aspecto) %>% summarise(n = n(), preguntas = paste(pregunta, collapse = "\n")) %>%
    ggplot(aes(x = stats::reorder(aspecto, -n), y = n, label = preguntas)) +
    geom_col(fill = NA, color = "red") +
    geom_text(aes(y = 0), vjust = 0, nudge_y = .1) +
    theme(rect = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
    geom_label(aes(label = n)) +
    labs(x = NULL, y = NULL)
  return(list(c))
}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("end", "respuestas","ranl","ini","ymin","ymax"))

#' Title
#'
#' @param dicc
#'
#' @return
#' @export
#'
#' @examples

gant_p_r <- function(dicc){

  bl <- dicc %>% count(bloque) %>%
    mutate(end = cumsum(n)) %>%
    mutate(ini = lag(end,default = 0))

  pt <- dicc %>%
    unnest(respuestas) %>%
    mutate(llaves = factor(llaves, levels = unique(llaves)),
           respuestas = factor(respuestas, levels = unique (respuestas))
    ) %>%
    count(bloque, llaves, tipo_pregunta, respuestas)
  labels_y <- pt %>% group_by(respuestas) %>% mutate(ranl = min_rank(llaves)) %>% filter(ranl == 1)
  labels_x <- pt %>% group_by(llaves) %>% mutate(ranl = min_rank(respuestas)) %>% filter(ranl == 1)

  bl_rect <- bl %>% mutate(ymin = 1, ymax = n_distinct(pt$respuestas))
  c <- pt %>%
    ggplot() +
    geom_rect(data = bl_rect, aes(xmin = ini+.5, xmax = end+.5, ymin = ymin-.5, ymax = ymax+.5, fill = bloque),alpha = .5) +
    geom_tile(aes(x = llaves, y = respuestas, fill = tipo_pregunta)) +
    geom_text(data = labels_y, aes(x = llaves, y = respuestas, label = respuestas), hjust = 1, nudge_x = -.5, size = 2) +
    geom_text(data = labels_x, aes(x = llaves, y = respuestas, label = llaves), hjust = 1, nudge_y = -.5, size = 2, angle = 90) +
    theme(rect = element_blank(),axis.text = element_blank(), axis.ticks = element_blank(),
          legend.position = "bottom") +
    labs(x = NULL, y = NULL) +
    scale_x_discrete(expand = expansion(mult = c(.1,0))) +
    scale_y_discrete(expand = expansion(mult = c(.15,0)))

  return(c)

}
