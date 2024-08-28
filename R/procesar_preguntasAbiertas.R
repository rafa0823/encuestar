#' Calcular proporciones por categorias de respuestas abiartas
#'
#' @param disenoDiseno muestral que contiene los pesos por individuo y las variables relacionadas.
#' @param llave_categorias Nombre de la variable que contiene las categorias de respuestas abiertas
#' @param separacion_multicategoria Cadena de texto que separa las categorias multiples
#'
#' @return
#' @export
#'
#' @examples
calcular_proporcionesCategorias = function(diseno, llave_categorias, separacion_multicategoria = ">>>"){
  srvyr::as_survey_design(diseno) |>
    group_by(!!rlang::sym(llave_categorias)) |>
    summarise(total = srvyr::survey_total(na.rm = T, vartype = NULL)) |>
    tibble::rownames_to_column(var = "id") %>%
    tidyr::separate_rows(!!rlang::sym(llave_categorias), sep = separacion_multicategoria) |>
    filter(!(!!rlang::sym(llave_categorias) %in% c("sin_categoria", "", NA_character_))) |>
    group_by(!!rlang::sym(llave_categorias)) |>
    summarise(total = sum(total, na.rm = TRUE)) %>%
    rename(categoria = !!rlang::sym(colnames(.)[1])) |>
    mutate(total = as.integer(round(total))) |>
    arrange(desc(total)) |>
    mutate(pct = total/sum(total),
           acum = cumsum(pct))
}
#' Title
#'
#' @param criterio
#' @param cuantiles
#' @param top
#' @param colores
#' @param bd_proporcionesCategorias
#'
#' @return
#' @export
#'
#' @examples
asignar_coloresCategorias = function(bd_proporcionesCategorias, criterio = NULL, cuantiles = 4, top = 3, colores = c("#6a104d", "#ff8cf2")) {
  if(is.null(criterio)) {
    bd_proporcionesCategorias |>
      mutate(color = colores[1])
  }
  else if(criterio == "cuantiles") {
    generar_paletaGradiente = grDevices::colorRampPalette(colors = colores)
    colores <- tibble(cuantil = seq.int(from = 1, to = cuantiles, by = 1),
                      color = generar_paletaGradiente(cuantiles))
    bd_proporcionesCategorias |>
      mutate(cuantil = cut(acum,
                           breaks = seq(0, 1, 1/cuantiles),
                           include.lowest = TRUE,
                           labels = FALSE)) |>
      left_join(colores, by = "cuantil")
  }
  else if(criterio == "top") {
    bd_proporcionesCategorias |>
      mutate(rango = rank(-pct),
             color = dplyr::if_else(condition = rango <= top,
                                    true = colores[1],
                                    false = colores[2]))
  }
}
