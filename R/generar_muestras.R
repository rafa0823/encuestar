#' Compilación del marco muestral para la muestra
#'
#' @param ln
#' @param shp_mza
#' @param shp_loc
#' @param shp_mun
#'
#' @return
#' @export
#'
#' @examples
crear_marcoMuestral <- function(ln, shp_mza, shp_loc, shp_mun){
  shp_mza <- shp_mza %>% filter(st_is_valid(.), STATUS == 1)
  aux <- st_join(shp_loc, shp_mza %>% select(MANZANA))
  shp_lpr <- aux %>% filter(is.na(MANZANA)) %>% select(-MANZANA)

  if("LISTA" %in% colnames(shp_lpr)){
    shp_lpr <- shp_lpr %>% relocate(LISTA, .before = LOCALIDAD)
  }

  shp_lpr <- shp_lpr %>% as_tibble %>% rename(MANZANA = NOMBRE) %>%
    mutate(across(c(ENTIDAD:LOCALIDAD,-contains("LISTA")), ~as.character(.x))) %>%
    select(ENTIDAD:MANZANA) %>% mutate(TIPO = "rural")

  shp_mun <- shp_mun %>% as_tibble %>% rename(NOMBRE_MUN = NOMBRE) %>%
    mutate(across(ENTIDAD:MUNICIPIO, ~as.character(.x))) %>% select(ENTIDAD:NOMBRE_MUN)

  if("LISTA" %in% colnames(shp_mza)){
    shp_mza <- shp_mza %>% relocate(LISTA, .before = MANZANA)
  }

  shp_mza <- shp_mza %>% as_tibble %>% select(ENTIDAD:MANZANA) %>%
    mutate(across(c(ENTIDAD:MANZANA,-contains("LISTA")), ~as.character(.x))) %>% mutate(TIPO = "urbana")

  shp_mza <- shp_mza %>% bind_rows(shp_lpr) %>% arrange(as.numeric(SECCION))

  ln <- ln %>% select(SECCION, contains("LISTA_")) %>% pivot_longer(-SECCION, names_to = "sector",
                                                                    values_to = "n") %>%
    mutate(sector = gsub(pattern = "_18_",replacement =  "_18_18_",x =  sector),
           sector = gsub(pattern = "_19_",replacement =  "_19_19_",x =  sector),
           sector = gsub(pattern = "_Y_",replacement =  "_",x =  sector),
    ) %>%
    separate(sector, into = c("lista","ini","fin","sexo")) %>%
    # mutate(across(ini:fin, parse_number)) %>%
    mutate(fin = as.numeric(fin),
           fin = if_else(is.na(fin),200,fin),
           rango = cut(as.numeric(fin), c(17,24,39,59,Inf),
                       labels = paste0("LN22_",c("18A24","25A39","40A60","60YMAS")))) %>%
    count(SECCION,rango,sexo, wt = n) %>% mutate(sexo = if_else(sexo == "HOMBRES","M","F")) %>%
    unite(rango_sexo, rango:sexo) %>%
    pivot_wider(SECCION,names_from = rango_sexo,values_from = n) %>%
    left_join(ln %>% select(SECCION, `LISTA NOMINAL`)) %>%
    mutate(SECCION = as.character(SECCION)) %>% rename(lista_nominal = `LISTA NOMINAL`)

  ln_mza <- ln %>%
    left_join(shp_mza %>% count(SECCION, name = "n_mza")
    ) %>% mutate(across(2:lista_nominal, ~.x/n_mza)) %>% select(-n_mza)

  mza <- shp_mza %>% left_join(shp_mun) %>%
    left_join(ln_mza, by = "SECCION") %>%
    mutate(across(where(is.numeric), ~if_else(is.na(.x), 0, .x))) %>%
    rownames_to_column(var = "id")# %>% filter(lista_nominal > 0)

  if("LISTA" %in% colnames(mza)){
    mza <- mza %>% mutate(lista_nominal = if_else(is.na(LISTA), lista_nominal, LISTA)) %>% select(-LISTA)
  }

  return(mza)
}
