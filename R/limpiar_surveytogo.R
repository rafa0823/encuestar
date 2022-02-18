
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
corregir_cluster <- function(respuestas, shp, mantener, nivel, var_n) {
  enc_shp <- respuestas %>%
    st_as_sf(coords = c("Longitude","Latitude"), crs = "+init=epsg:4326")


  todas <- enc_shp %>%
    st_join(shp %>% filter(sf::st_geometry_type(.) != "POINT"))

  if(!is.na(mantener)){
    fuera <-  todas %>% filter(is.na(!!rlang::sym(glue::glue("{var_n}.y"))),
                               ! (!!rlang::sym(glue::glue("{var_n}.x")) %in% mantener))
    fuera_sm <-  todas %>% filter(is.na(!!rlang::sym(glue::glue("{var_n}.y"))),
                                  !!rlang::sym(glue::glue("{var_n}.x")) %in% mantener)
  } else{
    fuera <-  todas %>% filter(is.na(!!rlang::sym(glue::glue("{var_n}.y"))))
  }


  ja <- st_distance(fuera,shp) %>% as_tibble %>% rowwise() %>% mutate(id = which.min(c_across(everything())))
  fuera <- fuera %>% mutate(!!rlang::sym(nivel) := as.character(shp[[nivel]][ja$id]))

  if(!is.na(mantener)){
    fuera_sm <- fuera_sm %>% mutate(!!rlang::sym(nivel) := as.character(!!rlang::sym(glue::glue("{var_n}.y"))))
    fuera <- bind_rows(fuera, fuera_sm)
  }
  fuera <- fuera %>% mutate(distinto = !!rlang::sym(glue::glue("{nivel}")) != !!rlang::sym(glue::glue("{var_n}.x")))

  dentro <-  todas %>%
    filter(!is.na(!!rlang::sym(glue::glue("{var_n}.y")))) %>%
    mutate(distinto= !!rlang::sym(nivel) != !!rlang::sym(glue::glue("{var_n}.x")),
           !!rlang::sym(nivel) := as.character(!!rlang::sym(nivel)))

  nuevos <- dentro %>%
    bind_rows(fuera) %>%
    as_tibble() %>%
    filter(distinto) %>%
    select(cluster_0, !!rlang::sym(nivel), distinto)

  respuestas <- left_join(respuestas, nuevos, by="cluster_0")

  respuestas$distinto[is.na(respuestas$distinto)] <- F

  respuestas <- respuestas %>%
    mutate(!!rlang::sym(glue::glue("{var_n}")) := if_else(distinto, !!rlang::sym(nivel),
                                                            !!rlang::sym(glue::glue("{var_n}")))) %>%
    select(-!!rlang::sym(nivel), -distinto)

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
      diccionario = self$cuestionario$diccionario %>% pull(llaves) %>% as.character() %>%
        append(
          self$cuestionario$documento %>%
            filter(style_name == "Morant_filtros" | style_name == "Preguntas_filtros", stringr::str_detect(text,"\\{")) %>%
            transmute(text = stringr::str_extract(text,"(?<=\\{).+?(?=\\})") %>% stringr::str_squish()) %>% pull(1)
        )
    ) %>% tibble::rownames_to_column(), by = c("bd" = "diccionario")
  ) %>% filter(is.na(rowname.x) | is.na(rowname.y)) %>%
    replace_na(list(rowname.y = "No está en el diccionario",
                    rowname.x = "No está en la base")) %>%
    mutate(rowname.x = if_else(stringr::str_detect(rowname.x,"No está"), rowname.x, bd) %>%
             forcats::fct_relevel("No está en la base", after = Inf),
           rowname.y = if_else(stringr::str_detect(rowname.y,"No está"), rowname.y, bd) %>%
             forcats::fct_relevel("No está en el diccionario", after = Inf)) %>%
    ggplot() + geom_tile(aes(x = rowname.x, y =rowname.y), fill = "red") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = NULL, y = NULL)
  return(g)
}

#' Title
#'
#' @param self
#'
#' @return
#' @export
#'
#' @examples
var_clave_diccionario <- function(self, diseño){
  cluster_id <- diseño$niveles %>% filter(nivel == diseño$ultimo_nivel) %>% pull(llave)
  post_id <- diseño$cuotas %>% select(-Municipio,-Localidad,-contains("cluster"),-n) %>% names

  filtros <- self$cuestionario$documento %>% officer::docx_summary() %>% as_tibble %>%
    filter(style_name == "Morant_filtros" | style_name == "Preguntas_filtros", stringr::str_detect(text,"\\{")) %>%
    transmute(text = stringr::str_extract(text,"(?<=\\{).+?(?=\\})") %>% stringr::str_squish()) %>% pull(1)

  if_else(!cluster_id %in% filtros, glue::glue("La variable {cluster_id} no se encuentra en el cuestionario"),"")
  if_else(!all(post_id %in% filtros),
          glue::glue("Las variables {paste(post_id, collapse = ', ')} no se encuentran en el cuestionario"),"")

}
