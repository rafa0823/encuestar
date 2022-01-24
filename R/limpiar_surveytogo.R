
#' Title
#'
#' @param respuestas
#' @param shp
#' @param mantener
#'
#' @return
#' @export
#'
#' @examples
corregir_cluster <- function(respuestas, shp, mantener) {
  enc_shp <- respuestas %>%
    st_as_sf(coords = c("Longitude","Latitude"), crs = "+init=epsg:4326")


  todas <- enc_shp %>%
    st_join(shp %>% filter(sf::st_geometry_type(.) != "POINT"))

  if(!is.na(mantener)){
    fuera <-  todas %>% filter(is.na(AULR), !CLUSTER %in% mantener)
    fuera_sm <-  todas %>% filter(is.na(AULR), CLUSTER %in% mantener) # estan cerca de un cluster diferente al planeado
  }


  ja <- st_distance(fuera,shp) %>% as_tibble %>% rowwise() %>% mutate(id = which.min(c_across(everything())))
  fuera <- fuera %>% mutate(cluster_3 = as.character(shp$cluster_3[ja$id]))

  if(!is.na(mantener)){
    fuera_sm <- fuera_sm %>% mutate(cluster_3 = as.character(CLUSTER))
    fuera <- bind_rows(fuera, fuera_sm) %>% mutate(distinto = cluster_3 != CLUSTER)
  }
  dentro <-  todas %>%
    filter(!is.na(AULR)) %>%
    mutate(distinto= cluster_3 != CLUSTER,
           cluster_3=as.character(cluster_3))

  nuevos <- dentro %>%
    bind_rows(fuera) %>%
    as_tibble() %>%
    filter(distinto) %>%
    select(cluster_0, cluster_3, distinto)

  respuestas <- left_join(respuestas, nuevos, by="cluster_0")

  respuestas$distinto[is.na(respuestas$distinto)] <- F

  respuestas<- respuestas %>%
    mutate(CLUSTER=if_else(distinto, cluster_3, CLUSTER)) %>%
    select(-cluster_3, -distinto)

  print(glue::glue("Se cambiaron {nuevos %>% count(distinto) %>% pull(n)} clusters ya que la entrevista no está donde se reportó."))
  return(respuestas)

}

#' Title
#'
#' @param encuesta_qro
#'
#' @return
#' @export
#'
#' @examples
match_dicc_base <- function(self) {
  g <- tibble(
    bd = self$respuestas$base %>% select(-(SbjNum:INT15), -(T_Q_47_1:last_col())) %>% names
  ) %>% tibble::rownames_to_column() %>% full_join(
    tibble(
      diccionario = self$cuestionario$diccionario %>% pull(llaves)
    ) %>% tibble::rownames_to_column(), by = c("bd" = "diccionario")
  ) %>% filter(is.na(rowname.x) | is.na(rowname.y)) %>%
    replace_na(list(rowname.y = "No está en el diccionario",
                    rowname.x = "No está en la base")) %>%
    mutate(rowname.x = if_else(str_detect(rowname.x,"No está"), rowname.x, bd) %>%
             forcats::fct_relevel("No está en la base", after = Inf),
           rowname.y = if_else(str_detect(rowname.y,"No está"), rowname.y, bd) %>%
             forcats::fct_relevel("No está en el diccionario", after = Inf)) %>%
    ggplot() + geom_tile(aes(x = rowname.x, y =rowname.y), fill = "red") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = NULL, y = NULL)
  return(g)
}
