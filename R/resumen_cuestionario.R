
#' Title
#'
#' @param dicc
#'
#' @return
#' @export
#'
#' @examples
resumen_cuestionario <- function(dicc){

# Preguntas por bloque ----------------------------------------------------

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


# Preguntas por tipo ------------------------------------------------------

  pt <- dicc %>%
    unnest(respuestas) %>%
    mutate(llaves = factor(llaves, levels = unique(llaves)),
           respuestas = factor(respuestas, levels = unique (respuestas))
    ) %>%
    count(bloque, llaves, tipo_pregunta, respuestas)
  labels_y <- pt %>% group_by(respuestas) %>% mutate(ranl = min_rank(llaves)) %>% filter(ranl == 1)
  labels_x <- pt %>% group_by(llaves) %>% mutate(ranl = min_rank(respuestas)) %>% filter(ranl == 1)

  bl_rect <- bl %>% mutate(ymin = 1, ymax = n_distinct(pt$respuestas))
  b <- pt %>%
    ggplot() +
    geom_rect(data = bl_rect, aes(xmin = ini+.5, xmax = end+.5, ymin = ymin-.5, ymax = ymax+.5, fill = bloque),alpha = .5) +
    geom_tile(aes(x = llaves, y = respuestas, fill = tipo_pregunta)) +
    geom_text(data = labels_y, aes(x = llaves, y = respuestas, label = respuestas), hjust = 1, nudge_x = -.5, size = 2) +
    geom_text(data = labels_x, aes(x = llaves, y = respuestas, label = llaves), hjust = 1, nudge_y = -.5, size = 2, angle = 90) +
    theme(rect = element_blank(),axis.text = element_blank(), axis.ticks = element_blank(),
          legend.position = "bottom") +
    labs(x = NULL, y = NULL) +
    scale_x_discrete(expand = expansion(add = c(3,0))) +
    scale_y_discrete(expand = expansion(add = c(5,0)))

  return(list(a, b))
}
