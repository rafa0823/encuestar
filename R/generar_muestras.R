#' #' Compilación del marco muestral para la muestra
#' #'
#' #' @param ln
#' #' @param shp_mza
#' #' @param shp_loc
#' #' @param shp_mun
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' crear_MarcoMuestral <- function(ln, shp_mza, shp_loc, shp_mun){
#'   shp_mza <- shp_mza %>% filter(st_is_valid(.), STATUS == 1)
#'   aux <- st_join(shp_loc, shp_mza %>% select(MANZANA))
#'   shp_lpr <- aux %>% filter(is.na(MANZANA)) %>% select(-MANZANA)
#'
#'   if("LISTA" %in% colnames(shp_lpr)){
#'     shp_lpr <- shp_lpr %>% relocate(LISTA, .before = LOCALIDAD)
#'   }
#'
#'   shp_lpr <- shp_lpr %>% as_tibble %>% rename(MANZANA = NOMBRE) %>%
#'     mutate(across(c(ENTIDAD:LOCALIDAD,-contains("LISTA")), ~as.character(.x))) %>%
#'     select(ENTIDAD:MANZANA) %>% mutate(TIPO = "rural")
#'
#'   shp_mun <- shp_mun %>% as_tibble %>% rename(NOMBRE_MUN = NOMBRE) %>%
#'     mutate(across(ENTIDAD:MUNICIPIO, ~as.character(.x))) %>% select(ENTIDAD:NOMBRE_MUN)
#'
#'   if("LISTA" %in% colnames(shp_mza)){
#'     shp_mza <- shp_mza %>% relocate(LISTA, .before = MANZANA)
#'   }
#'
#'   shp_mza <- shp_mza %>% as_tibble %>% select(ENTIDAD:MANZANA) %>%
#'     mutate(across(c(ENTIDAD:MANZANA,-contains("LISTA")), ~as.character(.x))) %>% mutate(TIPO = "urbana")
#'
#'   shp_mza <- shp_mza %>% bind_rows(shp_lpr) %>% arrange(as.numeric(SECCION))
#'
#'   ln <- ln %>% select(SECCION, contains("LISTA_")) %>% pivot_longer(-SECCION, names_to = "sector",
#'                                                                     values_to = "n") %>%
#'     mutate(sector = gsub(pattern = "_18_",replacement =  "_18_18_",x =  sector),
#'            sector = gsub(pattern = "_19_",replacement =  "_19_19_",x =  sector),
#'            sector = gsub(pattern = "_Y_",replacement =  "_",x =  sector),
#'     ) %>%
#'     separate(sector, into = c("lista","ini","fin","sexo")) %>%
#'     # mutate(across(ini:fin, parse_number)) %>%
#'     mutate(fin = as.numeric(fin),
#'            fin = if_else(is.na(fin),200,fin),
#'            rango = cut(as.numeric(fin), c(17,24,39,59,Inf),
#'                        labels = paste0("LN22_",c("18A24","25A39","40A60","60YMAS")))) %>%
#'     count(SECCION,rango,sexo, wt = n) %>% mutate(sexo = if_else(sexo == "HOMBRES","M","F")) %>%
#'     unite(rango_sexo, rango:sexo) %>%
#'     pivot_wider(SECCION,names_from = rango_sexo,values_from = n) %>%
#'     left_join(ln %>% select(SECCION, `LISTA NOMINAL`)) %>%
#'     mutate(SECCION = as.character(SECCION)) %>% rename(lista_nominal = `LISTA NOMINAL`)
#'
#'   ln_mza <- ln %>%
#'     left_join(shp_mza %>% count(SECCION, name = "n_mza")
#'     ) %>% mutate(across(2:lista_nominal, ~.x/n_mza)) %>% select(-n_mza)
#'
#'   mza <- shp_mza %>% left_join(shp_mun) %>%
#'     left_join(ln_mza, by = "SECCION") %>%
#'     mutate(across(where(is.numeric), ~if_else(is.na(.x), 0, .x))) %>%
#'     tibble::rownames_to_column(var = "id")# %>% filter(lista_nominal > 0)
#'
#'   if("LISTA" %in% colnames(mza)){
#'     mza <- mza %>% mutate(lista_nominal = if_else(is.na(LISTA), lista_nominal, LISTA)) %>% select(-LISTA)
#'   }
#'   return(mza)
#' }
#'
#' #' Title
#' #'
#' #' @param marco_muestral
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' crear_Cartografia <- function(shp_df, shp_dl, shp_mun, shp_sec, shp_mza, shp_loc){
#'
#'   df <- shp_df %>%
#'     transmute(across(c(ENTIDAD,DISTRITO_F), ~as.character(.x))) %>%
#'     st_make_valid()
#'
#'   dl <- shp_dl %>%
#'     transmute(across(c(ENTIDAD,DISTRITO_L), ~as.character(.x))) %>%
#'     st_make_valid()
#'
#'   mun <- shp_mun %>%
#'     rename(NOMBRE_MUN = NOMBRE) %>%
#'     mutate(across(ENTIDAD:MUNICIPIO, ~as.character(.x))) %>% select(ENTIDAD:NOMBRE_MUN) %>%
#'     st_make_valid()
#'
#'   loc <- shp_loc %>%
#'     rename(MANZANA = NOMBRE) %>%
#'     mutate(across(ENTIDAD:LOCALIDAD, ~as.character(.x))) %>%
#'     select(ENTIDAD:MANZANA) %>%
#'     st_make_valid()
#'
#'   sec <- shp_sec %>%
#'     rename(DISTRITO_F = DISTRITO) %>%
#'     mutate(across(ENTIDAD:SECCION, ~as.character(.x))) %>%
#'     select(ENTIDAD:SECCION) %>%
#'     st_make_valid()
#'
#'   mza <- shp_mza %>%
#'     filter(st_is_valid(.), STATUS == 1) %>%
#'     select(ENTIDAD:MANZANA) %>%
#'     mutate(across(ENTIDAD:MANZANA, ~as.character(.x))) %>%
#'     st_make_valid()
#'
#'   mza <- bind_rows(mza, loc) %>% arrange(as.numeric(SECCION))
#'
#'   return(list(
#'     DISTRITO_F = df,
#'     DISTRITO_L = dl,
#'     MUNICIPIO = mun,
#'     SECCION = sec,
#'     MANZANA = mza
#'   ))
#'
#'   return("Esta es la cartografía")
#'
#' }
#'
#' #' Title
#' #'
#' #' @param diseño
#' #' @param criterio
#' #' @param unidades_nivel
#' #' @param manual
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' asignar_m <- function(diseño, criterio, unidades_nivel, manual){
#'
#'   # Se elige la unidad de muestreo
#'   un_muestreo <- diseño$niveles %>%
#'     filter(!plan_muestra) %>%
#'     slice_min(n = 1, order_by = nivel)
#'   # Se cuentan las unidades secundarias de muestreo
#'   # Sirve para saber si hay suficientes unidades secundarias
#'   aux <- diseño$MarcoMuestral %>%
#'     agrupar_nivel(un_muestreo$nivel) %>% {
#'       if(un_muestreo$nivel==diseño$ultimo_nivel){
#'         summarise(.,n=n_distinct(cluster_0))
#'       }
#'       else
#'         summarise(.,across(matches(glue::glue("(strata|cluster)_{un_muestreo$nivel+1}")),
#'                            ~n_distinct(.x),
#'                            .names = "n"))
#'     }
#'   if(un_muestreo$nivel==diseño$ultimo_nivel){
#'     warning("Por ser el último nivel el criterio de repartición está determinado")
#'     res <- diseño$MarcoMuestral %>%
#'       agrupar_nivel(un_muestreo$nivel) %>%
#'       summarise() %>%
#'       left_join(diseño$n_i %>% last()) %>%
#'       # Aquí se fuerza al número de manzanas determinada
#'       mutate("m_{un_muestreo$nivel}":=round((!!sym(glue::glue("n_{un_muestreo$nivel-1}"))/
#'                                                !!sym(glue::glue("m_{un_muestreo$nivel-1}")) )/
#'                                               diseño$entrevistas_por_upm)) %>%
#'       select(-all_of(c(glue::glue("n_{un_muestreo$nivel-1}"),
#'                        glue::glue("m_{un_muestreo$nivel-1}"))))
#'   }
#'   else{
#'     if(criterio=="uniforme"){
#'       res <- diseño$poblacion$marco_muestral %>%
#'         agrupar_nivel(un_muestreo$nivel) %>%
#'         summarise() %>%
#'         mutate("m_{un_muestreo$nivel}":= round(unidades_nivel/un_muestreo$unidades)) %>%
#'         select(starts_with("strata_"),
#'                starts_with("cluster_"),
#'                glue::glue("m_{un_muestreo$nivel}"))
#'     }
#'     if(criterio == "manual"){
#'       res <- diseño$poblacion$marco_muestral %>%
#'         agrupar_nivel(un_muestreo$nivel) %>%
#'         summarise() %>% mutate("m_{un_muestreo$nivel}":= manual)
#'     }
#'     if(criterio=="unidades"){
#'       res <- aux %>%
#'         mutate("m_{un_muestreo$nivel}":= repartir_cociente(unidades_nivel,unidades_nivel*n/sum(n)))
#'     }
#'     if(criterio=="peso"){
#'       # Se utiliza la variable poblacional de tamaño para repartir las unidades de nivel
#'       # según el peso. Hasta aquí no es relativo al grupo anterior, es una repartición global.
#'       res <- diseño$MarcoMuestral %>%
#'         agrupar_nivel(un_muestreo$nivel) %>%
#'         summarise(n=sum(!!sym(diseño$variable_poblacional))) %>%
#'         # agrupar_nivel(un_muestreo$nivel) %>%
#'         mutate("m_{un_muestreo$nivel}":= repartir_cociente(unidades_nivel,unidades_nivel*n/sum(n))) %>%
#'         # mutate("m_{un_muestreo$nivel}":= round(unidades_nivel/n_groups(.)*n/sum(n))) %>%
#'         select(-n)
#'     }
#'
#'   }
#'   # Se elije el mínimo entre las unidades previstas en el plan de muestreo y las posibles
#'   res <- res %>%
#'     left_join(aux) %>%
#'     mutate("m_{un_muestreo$nivel}" := if_else(!!sym(glue::glue("m_{un_muestreo$nivel}")) < n,
#'                                              !!sym(glue::glue("m_{un_muestreo$nivel}")), as.numeric(n))) %>%
#'     select(-n)
#'   # Se asigna el número total de unidades de muestreo
#'   # Cuando es strata se asigna así mismo y al siguiente
#'   if(un_muestreo$tipo=="strata"){
#'     diseño$niveles <-  diseño$niveles %>%
#'       # El número de estratos. Sirve para el nivel 1, hay que checar para otros niveles
#'       mutate(unidades=if_else(nivel==un_muestreo$nivel,
#'                               as.numeric(nrow(res)), unidades))
#'   }
#'   # Siempre se asigna al siguiente
#'
#'   diseño$niveles <- diseño$niveles %>%
#'     mutate(unidades=if_else(nivel==un_muestreo$nivel+1,
#'                             if_else(un_muestreo$nivel==1,res %>%
#'                                       ungroup() %>%
#'                                       summarise(n=sum(!!sym(glue::glue("m_{un_muestreo$nivel}")))) %>%
#'                                       pull(n),
#'                                     res %>%
#'                                       ungroup() %>%
#'                                       summarise(n=mean(!!sym(glue::glue("m_{un_muestreo$nivel}")))) %>%
#'                                       pull(n) * un_muestreo$unidades),
#'                             unidades))
#'   return(res)
#' }
#'
#' #' Title
#' #'
#' #' @param diseño
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' asignar_n <- function(diseño){
#'   un_muestreo <- diseño$niveles %>%
#'     filter(!plan_muestra) %>%
#'     slice_min(n=1, order_by = nivel)
#'   if(un_muestreo$tipo=="strata"){
#'     res <- diseño$MarcoMuestral %>%
#'       agrupar_nivel(un_muestreo$nivel) %>%
#'       summarise(n=sum(!!sym(diseño$variable_poblacional))) %>%
#'       mutate(n=n/sum(n))
#'     if(un_muestreo$nivel==1) {
#'       res <- res %>%
#'         mutate(., n_1 = diseño$entrevistas_totales*n) %>%
#'         select(-n)
#'     }
#'     else{
#'       anterior <- diseño$niveles %>% filter(nivel==un_muestreo$nivel-1)
#'       diseño$n_i[[glue::glue("{anterior$tipo}_{anterior$nivel}")]] %>%
#'         left_join(res)
#'     }
#'
#'   }
#'   if(un_muestreo$tipo=="cluster"){
#'
#'     anterior <- diseño$niveles %>%
#'       filter(nivel==un_muestreo$nivel-1)
#'
#'     res <- diseño$MarcoMuestral %>%
#'       agrupar_nivel(un_muestreo$nivel) %>%
#'       summarise()
#'
#'     res <- res %>%
#'       left_join(diseño$n_i[[glue::glue("{anterior$tipo}_{anterior$nivel}")]] %>%
#'                                mutate("n_{un_muestreo$nivel}":=!!sym(glue::glue("n_{anterior$nivel}"))/!!sym(glue::glue("m_{anterior$nivel}"))) %>%
#'                                select(starts_with("cluster"),
#'                                       starts_with("strata"),
#'                                       glue::glue("n_{un_muestreo$nivel}")))
#'   }
#'   return(res)
#' }
#'
#' #' Title
#' #'
#' #' @param bd
#' #' @param id
#' #' @param regiones
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' crear_regiones <- function(bd, id, regiones){
#'   aux <- regiones %>% tibble::enframe(name = "region", value = id) %>% tidyr::unnest(all_of(id))
#'   faltan <- bd %>% anti_join(aux) %>% distinct(!!sym(id)) %>% pull(1) %>% paste(collapse = ", ")
#'   if(faltan != ""){
#'     warning(glue::glue("No se ha clasificado los siguientes {id}: {faltan}. \n Favor de agregarlos a la lista regiones"))
#'   }
#'
#'   error <- aux %>% anti_join(bd) %>% distinct(!!sym(id)) %>% pull(1) %>% paste(collapse = ", ")
#'   if(error != ""){
#'     warning(glue::glue("Los siguientes {id} no existen en la base de datos: {error}. \n Favor de rectificar la lista de regiones"))
#'   }
#'
#'   bd %>% left_join(
#'     aux
#'   ) %>% select(region, everything())
#' }
#'
#' #' Title
#' #'
#' #' @param bd
#' #' @param nivel
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' agrupar_nivel <- function(bd, nivel){
#'   niveles <- grep(x = names(bd), pattern = glue::glue("(strata|cluster)_[1-{nivel}]"), value=T)
#'   indices <- stringr::str_extract(niveles, pattern = '(?<=_).*') %>% as.numeric() %>% order()
#'   niveles <- niveles[indices]
#'   bd <- bd %>% group_by(across(all_of(niveles)))
#'   return(bd)
#' }
#'
#' #' Title
#' #'
#' #' @param n
#' #' @param x
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' repartir_cociente <- function(n, x){
#'   if(sum(x)+length(x)-1<=n) stop("No es válido el vector propuesto")
#'   else{
#'     piso <- floor(x)
#'     dif= n-sum(piso)
#'
#'     residuo <- x- piso
#'
#'     nueva <- (order(residuo, decreasing = T)<=dif)+piso
#'   }
#'   return(nueva)
#' }
#'
#' #' Title
#' #'
#' #' @param lflt
#' #' @param muestra
#' #' @param shp
#' #' @param nivel
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' graficar_mapa_muestra_ine <- function(lflt = NULL, muestra, shp, nivel){
#'   pal <- if(nivel == "MUNICIPIO"){
#'     colorFactor(topo.colors(n_distinct(muestra$strata_1)), domain = unique(muestra$strata_1))
#'   } else{
#'     colorFactor(c("orange","red"),c("LOCALIDAD","SECCION"))
#'   }
#'
#'   mapa <- if(is.null(lflt)){
#'     shp %>% purrr::pluck(nivel) %>%
#'       left_join(muestra %>% distinct(MUNICIPIO,strata_1)) %>%
#'       group_by(strata_1) %>% summarise(n()) %>%
#'       sf::st_buffer(dist = 0) %>%
#'       leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
#'       addPolygons(color = ~pal(strata_1), opacity = 1, fill = F) %>%
#'       addLegend(pal = pal, values = ~strata_1, position = "bottomleft")
#'   } else{
#'     if(nivel == "MUNICIPIO"){
#'       lflt %>% addPolygons(data = shp %>% purrr::pluck(nivel) %>% inner_join(muestra %>% distinct(across(all_of(nivel)), .keep_all = T)),
#'                            fillColor = ~pal(strata_1), color = "black", opacity = 1, weight = 1, fillOpacity = 1, label = ~glue::glue("Municipio: {NOMBRE_MUN}"))
#'     } else{
#'       if(nivel == "MANZANA"){
#'         mapear <- shp %>% purrr::pluck(nivel) %>% inner_join(muestra %>% distinct(across(all_of(nivel)), .keep_all = T))
#'
#'         lflt %>%
#'           addCircleMarkers(data = mapear %>% filter(sf::st_geometry_type(.) == "POINT"),
#'                            label = ~glue::glue("Localidad: {MANZANA}"), opacity = 1, fillOpacity = 1,
#'                            fillColor = "#f72585", color = "black", weight = 1) %>%
#'           addLegend(position = "bottomright", colors = "#f72585", labels = "Localidades rurales")
#'
#'       } else{
#'         mapear <- shp %>% purrr::pluck(nivel) %>% inner_join(muestra %>% distinct(across(all_of(nivel)), .keep_all = T))
#'
#'         nivel <- mapear %>% as_tibble %>% select(contains("cluster")) %>% names %>% readr::parse_number() %>% max
#'
#'         popup_cluster <- paste0("cluster_",nivel,": ", as_tibble(mapear)[[paste("cluster",nivel,sep = "_")]])
#'         popup_mun <- paste("Municipio: ", mapear$NOMBRE_MUN)
#'         lflt %>% addPolygons(data = mapear,
#'                              stroke = T, color = "black",
#'                              fillColor = ~pal(nivel), fillOpacity = .2,weight = 1, opacity = 1,
#'                              # popup = ~glue::glue("Sección: {SECCION}")
#'                              popup = paste(popup_mun, popup_cluster, sep = "<br>")
#'         ) %>%
#'           addLegend(data = mapear, pal = pal, values = ~nivel, position = "bottomright")
#'       }
#'     }
#'   }
#'
#'   return(mapa)
#' }
#'
#' #' Title
#' #'
#' #' @param diseño
#' #' @param nivel
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' calcular_fpc <- function(diseño, nivel = 1){
#'   nombres <- names(diseño$MarcoMuestral)
#'   nivel_anterior <- diseño$niveles %>%
#'     filter(nivel == (!!nivel - 1))
#'   nivel_principal <- grep(x = nombres,
#'                           pattern = glue::glue("(strata|cluster)_{nivel}"),
#'                           value = T )
#'   if(length(nivel_principal)!=1) stop("El nivel seleccionado no se encuestra en el marco muestral")
#'   # if(nivel_principal=="cluster_0") stop("Hay que arreglar esto")
#'   # if(nivel == diseño$ultimo_nivel) nivel_secundario <- "cluster_0"
#'   # else{
#'   #   nivel_secundario <- grep(nombres,pattern = glue::glue("(strata|cluster)_{nivel+1}"),
#'   #                            value = T )
#'   #   if(length(nivel_secundario)==0) {
#'   #     warning("El nivel posterior no se encuentra en el marco muestral, se utiliza en cambio el último nivel")
#'   #     nivel_secundario <- "cluster_0"
#'   #   }
#'   # }
#'
#'   aux <- if(nivel == 0) {
#'
#'     nivel_principal <- diseño$niveles %>%
#'       filter(nivel == max(nivel)) %>%
#'       pull(nivel)
#'
#'     diseño$MarcoMuestral %>%
#'       agrupar_nivel(nivel_principal) %>%
#'       mutate(manzanas = n()) %>%
#'       left_join(diseño$n_i %>% .[[grep(nombres,pattern = glue::glue("(strata|cluster)_{diseño$ultimo_nivel}"),
#'                                        value = T )]]) %>%
#'       mutate(
#'         fpc_0= sampling::inclusionprobabilities(n = unique(!!sym(glue::glue("m_{diseño$ultimo_nivel}"))), a = manzanas)
#'       ) %>% distinct(fpc_0) %>% ungroup
#'
#'   } else {
#'
#'     diseño$MarcoMuestral %>%
#'       agrupar_nivel(readr::parse_number(nivel_principal)) %>%
#'       summarise(total = sum(!!sym(diseño$variable_poblacional), na.rm = T)) %>%
#'       left_join(diseño$n_i %>% .[[glue::glue("{nivel_anterior$tipo}_{nivel_anterior$nivel}")]]) %>%
#'       mutate(!!rlang::sym(glue::glue("fpc_{nivel}")) := sampling::inclusionprobabilities(n = unique(!!sym(glue::glue("m_{nivel-1}"))), a = total)) %>%
#'       ungroup %>%
#'       select(-total, -all_of(c(glue::glue("{c('m','n')}_{nivel-1}"))))
#'   }
#'
#'   res <- diseño$MarcoMuestral %>%
#'     left_join(aux)
#'
#'   return(res)
#' }
#'
#' #' Title
#' #'
#' #' @param diseño
#' #' @param nivel
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' calcular_muestra_nivel <- function(diseño, nivel){
#'
#'   nombres <- names(diseño$MarcoMuestral)
#'   nivel_principal <- grep(nombres,pattern = glue::glue("(strata|cluster)_{nivel}"),
#'                           value = T )
#'   if(length(nivel_principal)!=1) stop("El nivel seleccionado no se encuestra en el marco muestral")
#'   if(nivel_principal=="cluster_0") stop("Hay que arreglar esto")
#'   if(nivel == diseño$ultimo_nivel) nivel_secundario <- "cluster_0"
#'   else{
#'     nivel_secundario <- grep(nombres,pattern = glue::glue("(strata|cluster)_{nivel+1}"),
#'                              value = T ) %>% readr::parse_number()
#'     if(length(nivel_secundario)==0) {
#'       warning("El nivel posterior no se encuentra en el marco muestral, se utiliza en cambio el último nivel")
#'       nivel_secundario <- "cluster_0"
#'     }
#'   }
#'
#'   bd <- if(is.null(diseño$muestra)) diseño$MarcoMuestral else diseño$muestra %>% purrr::pluck(length(diseño$muestra)) %>% tidyr::unnest(data)
#'   # bd <- diseño$MarcoMuestral
#'   muestra  <- bd %>%
#'     agrupar_nivel(nivel_secundario) %>%
#'     mutate(total = sum(!!sym(diseño$variable_poblacional),na.rm = T)) %>%
#'     group_by(total, .add = T) %>%
#'     tidyr::nest() %>%
#'     ungroup() %>%
#'     # semi_join(
#'     #   diseño$n_i %>% .[[nivel_principal]] %>% filter(!!rlang::sym(glue::glue("m_{nivel}")) >0)
#'     # ) %>%
#'     split(.[[nivel_principal]]) %>% purrr::map_df(~{
#'       n_nivel <- diseño$n_i %>% .[[nivel_principal]] %>%
#'         filter(!!sym(nivel_principal) == unique(.x[[nivel_principal]])) %>% pull(glue::glue("m_{nivel}"))
#'
#'       if(nivel_secundario == "cluster_0") sorteado <- .x %>% slice_sample(n = n_nivel) else{
#'         if(is.null(diseño$sobre_muestra)){
#'           no_necesita_sm <- T
#'         } else{
#'           no_necesita_sm <- diseño$sobre_muestra %>% semi_join(.x) %>% nrow() == 0
#'         }
#'
#'         if(no_necesita_sm){
#'           sorteado <-  .x %>% slice_sample(weight_by = total,n = n_nivel)
#'         } else{
#'           n_nivel_original <- diseño$sobre_muestra %>% semi_join(.x) %>% pull(m_1_vieja)
#'           sorteo_normal <- .x %>% slice_sample(weight_by = total,n = n_nivel_original)
#'
#'           ya <- sorteo_normal %>% semi_join(
#'             diseño$poblacion$marco_muestral %>% semi_join(diseño$sobre_muestra)
#'           )
#'
#'           n_nivel_sm <- diseño$sobre_muestra %>% semi_join(.x) %>% pull(m_sm) -nrow(ya)
#'
#'           sorteo_sm <- .x %>% semi_join(
#'             diseño$poblacion$marco_muestral %>% semi_join(diseño$sobre_muestra)
#'           ) %>%
#'             anti_join(sorteo_normal)
#'
#'           if(nrow(sorteo_sm) > n_nivel_sm) {
#'             sorteo_sm <- sorteo_sm %>%
#'               slice_sample(weight_by = total,n = n_nivel_sm)
#'           }
#'
#'           sorteado <- bind_rows(sorteo_normal, sorteo_sm)
#'
#'         }
#'
#'       }
#'
#'
#'       return(sorteado)
#'     })
#'
#'   nombre <- diseño$niveles %>% filter(nivel == !!nivel+1) %>% pull(variable)
#'   if(length(nombre) == 0) nombre <- "MZA"
#'   res <- diseño$muestra %>% append(list(muestra) %>% purrr::set_names(nombre))
#'   return(res)
#'
#' }
#'
#' #' Title
#' #'
#' #' @param diseño
#' #' @param ajustar
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' cuotas_ine <- function(diseño, ajustar){
#'   u_nivel <- diseño$niveles %>%
#'     filter(nivel == diseño$ultimo_nivel)
#'
#'   u_cluster <- u_nivel %>%
#'     transmute(paste(tipo,nivel,sep = "_")) %>%
#'     pull(1)
#'
#'   muestra <- diseño$muestra %>%
#'     purrr::pluck(length(diseño$muestra))
#'
#'   bd <- diseño$MarcoMuestral %>%
#'     semi_join(muestra %>% distinct(!!rlang::sym(u_cluster)))
#'
#'   ent <- muestra %>%
#'     left_join(diseño$n_i$cluster_0) %>%
#'     count(!!rlang::sym(u_cluster), wt = n_0, name = "entrevistas")
#'
#'   cuotas <- bd %>%
#'     select(Municipio = NOMBRE_MUN,
#'            Seccion = SECCION,
#'            !!rlang::sym(u_cluster),
#'            contains("LN22")) %>%
#'     group_by(Municipio, Seccion,!!rlang::sym(u_cluster)) %>%
#'     summarise(across(everything(),.fns = sum, na.rm = T),.groups =  "drop") %>%
#'     tidyr::pivot_longer(c(-Municipio, -Seccion,-!!sym(u_cluster)), names_to = "edad", values_to = "cantidad") %>%
#'     tidyr::separate(edad, c("basura","rango","sexo")) %>% select(-basura) %>%
#'     group_by(!!rlang::sym(u_cluster)) %>%
#'     mutate(pct = cantidad/sum(cantidad)) %>%
#'     ungroup %>%
#'     left_join(ent) %>%
#'     mutate(n =  round(pct*entrevistas))
#'
#'   if(ajustar){
#'
#'     revision <- cuotas %>%
#'       count(!!rlang::sym(u_cluster), wt = n)
#'
#'     ci <- revision %>%
#'       left_join(ent) %>%
#'       mutate(diff = entrevistas - n) %>%
#'       select(a = 1,b = 4) %>%
#'       purrr::pmap_df(function(a, b){
#'         aux <- cuotas %>% filter(!!rlang::sym(u_cluster) == !! a)
#'         ya <- abs(b)
#'         while(ya != 0){
#'           alea <- min(nrow(aux), ya)
#'           aleatorio <- sample(x = seq_len(nrow(aux)), size = alea)
#'           aux[aleatorio, "n"] <- aux[aleatorio, "n"] + sign(b)
#'           ya <- ya - alea
#'         }
#'         return (aux)
#'       })
#'
#'     cool <- ci %>%
#'       count(!!rlang::sym(u_cluster), wt = n) %>%
#'       left_join(ent) %>%
#'       filter(n != entrevistas)
#'
#'     if(nrow(cool) == 0 & nrow(ci) == nrow(cuotas)) print("Exacto")
#'     ci <- ci %>%
#'       select(-cantidad,-pct,-entrevistas,-Seccion)
#'   } else {
#'     ci <- cuotas
#'   }
#'
#'   return(ci)
#' }
#'
#' #' Title
#' #'
#' #' @param diseño
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' llaves <- function(diseño){
#'   diseño$niveles %>%
#'     ggplot(aes(y = nivel, x = 0, label = glue::glue("{variable} - {llave}"), fill = tipo)) +
#'     geom_label(hjust = "inward") +
#'     geom_text(aes(x = 1, label = stringr::str_wrap(descripcion,width = 30)),
#'               hjust = 0) +
#'     scale_x_continuous(limits = c(0,2)) +
#'     labs(x = NULL) +
#'     theme(rect = element_blank(), legend.position = "bottom",
#'           legend.title = element_blank(),
#'           axis.text.x = element_blank(),
#'           axis.ticks.x = element_blank())
#' }
#'
#' #' Title
#' #'
#' #' @param diseño
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' plan <- function(diseño){
#'   diseño$niveles %>%
#'     filter(nivel == 0) %>%
#'     select(llave, unidades) %>%
#'     mutate(Entrevistas = diseño$entrevistas_totales,
#'            llave = "Planeado") %>%
#'     rename(Manzanas = unidades) %>%
#'     add_row(llave = "Recálculo",
#'             Manzanas = NA,
#'             Entrevistas = diseño$niveles %>% filter(nivel == 0) %>% pull(unidades) * diseño$entrevistas_por_upm) %>%
#'     add_row(llave = "Resultado",
#'             Manzanas = diseño$muestra %>% purrr::pluck(diseño$ultimo_nivel) %>% nrow,
#'             Entrevistas = sum(diseño$cuotas$n)) %>%
#'     tidyr::pivot_longer(-llave, names_to = "total") %>%
#'     na.omit() %>%
#'     ggplot() + geom_line(aes(x = llave, y = factor(value), group = total)) +
#'     labs(x = NULL, y = NULL) +
#'     facet_wrap(~total, scales = "free") +
#'     theme(rect = element_blank())
#' }
#'
#' #' Title
#' #'
#' #' @param self
#' #' @param prop_vars
#' #' @param var_extra
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' revision_ine <- function(self, prop_vars = NULL, var_extra = NULL){
#'   bd <- self$muestra %>%
#'     purrr::pluck("SECCION") %>%
#'     mutate(data = map(data, ~.x %>%
#'                         select(SECCION, contains("strata"), contains("cluster"),
#'                                contains("fpc")) %>% select(-contains("_0")) %>% distinct(.keep_all = T))) %>%
#'     tidyr::unnest(data)
#'
#'   mm <- self$InformacionElectoral
#'
#'   bd <- bd %>% left_join(mm, by = c("SECCION" = "seccion"))
#'   # bd <- bd %>% mutate(across(prop_vars,~.x/!!rlang::sym(self$variable_poblacional),.names = "{.col}_prop"))
#'
#'   clusters <- bd %>% select(contains("cluster")) %>% names()
#'   strata <- bd %>% select(contains("strata")) %>% names()
#'   fpc <- bd %>% select(contains("fpc")) %>% names()
#'
#'   diseño <- survey::svydesign(data = bd,
#'                               ids = survey::make.formula(clusters),
#'                               strata = survey::make.formula(strata),
#'                               fpc = survey::make.formula(fpc), pps = "brewer")
#'   options(survey.lonely.psu="remove")
#'
#'   tb <- c(var_extra) %>%
#'     purrr::map_df(.f = ~{
#'       puntual <- survey::svytotal(survey::make.formula(.x), design = diseño, na.rm = T, deff = T)
#'       original <- mm %>% summarise(original = sum(!!rlang::sym(.x),na.rm = T))
#'       intervalo <- confint(puntual) %>% as_tibble
#'       tb <- tibble(original,
#'                    puntual = puntual %>% as.numeric,
#'                    intervalo,
#'                    variable = .x,
#'                    deff = !!puntual %>% as_tibble() %>% pull("deff")) %>%
#'         mutate(color = if_else(between(original,`2.5 %`,`97.5 %`),"blue","orange"))
#'     }) %>%
#'     mutate(variable = reorder(variable,original)) %>% mutate(longitud = `97.5 %` - `2.5 %`)
#'
#'
#'   a <- tb %>% ggplot() +
#'     geom_segment(aes(x = puntual, xend = original, y =variable, yend = variable )) +
#'     geom_point(aes(x = original, y = variable, color = color), size = 5) +
#'     ggrepel::geom_text_repel(data = tb %>% top_n(5,wt = longitud),
#'                              aes(x = `2.5 %`, y = as.numeric(variable)+.5, label = scales::comma(longitud)),
#'                              hjust = 1, vjust = 0) +
#'     geom_segment(data = tb %>% top_n(5,wt = longitud),
#'                  aes(x = `2.5 %`,
#'                      xend = `97.5 %`,
#'                      y = as.numeric(variable)+.5,
#'                      yend = as.numeric(variable)+.5)) +
#'     scale_color_identity() +
#'     scale_x_continuous(labels = scales::comma) +
#'     geom_rect(aes(xmin = `2.5 %`,
#'                   xmax = `97.5 %`,
#'                   ymin = as.numeric(variable)-.5,
#'                   ymax = as.numeric(variable)+.5
#'                   # xmin = `2.5 %`, xmax = `97.5 %`
#'     ), alpha = .5, fill = "gray70") +
#'     labs(x = NULL, y = NULL, title = "Totales") +
#'     theme(rect = element_blank()) + scale_y_discrete(expand = expansion(add = c(0,1)))
#'
#'   if(!is.null(prop_vars)){
#'     tb_prop <- prop_vars %>%
#'       purrr::map_df(~{
#'       # puntual <- survey::svymean(survey::make.formula(.x), design = diseño, na.rm = T)
#'       puntual <- survey::svyratio(numerator =survey::make.formula(.x),
#'                                   denominator = survey::make.formula(self$variable_poblacional),
#'                                   design = diseño, na.rm = T, deff = T)
#'
#'       original <- mm %>%
#'         summarise(original = sum(!!rlang::sym(.x), na.rm = T)/sum(!!rlang::sym(self$variable_poblacional),
#'                                                                   na.rm = T))
#'
#'       intervalo <- confint(puntual) %>% as_tibble
#'       tb <- tibble(original,
#'                    puntual = puntual$ratio %>% as.numeric,
#'                    intervalo,
#'                    variable = .x %>% paste0("_prop")
#'       ) %>% mutate(color = if_else(between(original,`2.5 %`,`97.5 %`),"blue","orange"))}) %>%
#'       mutate(variable = reorder(variable,original)) %>% mutate(longitud = `97.5 %` - `2.5 %`)
#'
#'     b <- tb_prop %>% ggplot() +
#'       geom_segment(aes(x = puntual, xend = original, y =variable, yend = variable )) +
#'       geom_point(aes(x = original, y = variable, color = color), size = 5) +
#'       geom_text(data = tb_prop %>% top_n(5,wt = longitud),
#'                 aes(x = `2.5 %`, y = as.numeric(variable)+.5, label = scales::percent(longitud)),
#'                 hjust = 1, vjust = 0) +
#'       geom_segment(data = tb_prop %>% top_n(5,wt = longitud),
#'                    aes(x = `2.5 %`,
#'                        xend = `97.5 %`,
#'                        y = as.numeric(variable)+.5,
#'                        yend = as.numeric(variable)+.5)) +
#'       scale_color_identity() +
#'       scale_x_continuous(labels = scales::percent_format()) +
#'       geom_rect(aes(xmin = `2.5 %`,
#'                     xmax = `97.5 %`,
#'                     ymin = as.numeric(variable)-.5,
#'                     ymax = as.numeric(variable)+.5
#'                     # xmin = `2.5 %`, xmax = `97.5 %`
#'       ), alpha = .5, fill = "gray70") +
#'       labs(x = NULL, y = NULL, title = "Proporciones") +
#'       theme(rect = element_blank()) + scale_y_discrete(expand = expansion(add = c(0,1)))
#'   }
#'
#'   res <- if(!is.null(prop_vars)) cowplot::plot_grid(a,b) else a
#'
#'   return(list(res, tb))
#'
#' }
#'
#' #' Title
#' #'
#' #' @param diseño
#' #' @param shp
#' #' @param zoom
#' #' @param dir
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' google_maps_ine <- function(diseño, shp, zoom, dir = "Mapas"){
#'
#'   u_nivel <- diseño$niveles %>% filter(nivel == diseño$ultimo_nivel)
#'   u_cluster <- u_nivel %>% transmute(paste(tipo,nivel,sep = "_")) %>% pull(1)
#'   bd <- diseño$muestra %>% purrr::pluck(length(diseño$muestra)) %>% tidyr::unnest(data)
#'
#'   cluster <- bd %>% distinct(!!rlang::sym(u_cluster)) %>% pull(1)
#'   ya <- list.files(path=dir) %>% gsub('^.*_\\s*|\\s*.png.*$', '', .)
#'   cluster <- cluster[!cluster %in% ya]
#'   # agebs <- agebs %>% mutate(CVE_AGEB = paste0(22,CVE_MUN,CVE_LOC,CVE_AGEB))
#'   shp_mapa <- shp %>% purrr::pluck(u_nivel %>% pull(variable)) %>% inner_join(bd)
#'   man_shp <- shp %>% purrr::pluck("MANZANA") %>% inner_join(bd)
#'
#'
#'   for(i in cluster){
#'     aux_s <- diseño$cuotas %>% filter(!!rlang::sym(u_cluster) == i)
#'     s <- aux_s %>%
#'       mutate(n = glue::glue("{n} entrevistas")) %>%
#'       tidyr::pivot_wider(names_from = c("rango", "sexo"),values_from = "n") %>% select(-1) %>%
#'       mutate(Total = glue::glue("{sum(aux_s$n)} entrevistas")) %>% relocate(Total,.before = 1)
#'     cuotas <- paste(s %>% names(), s, sep = ": ") %>% paste(collapse = "\n")
#'     man <- man_shp %>% filter(!!rlang::sym(u_cluster) == i)
#'     aux_mapeo <- shp_mapa %>% filter(!!rlang::sym(u_cluster) == i)
#'     caja <- aux_mapeo %>% st_make_valid() %>% sf::st_union() %>% sf::st_centroid() %>% sf::st_coordinates() %>% as.numeric()
#'     nc_map <- ggmap::get_map(location = caja, maptype = "roadmap",
#'                              source = "google",force = T, zoom = zoom)
#'     Google <- ggmap::ggmap(nc_map)
#'     # Google
#'     puntos <- man %>% filter(sf::st_geometry_type(.) == "POINT")
#'     man <- man %>% filter(sf::st_geometry_type(.) != "POINT")
#'     g <- Google +
#'       geom_sf(data = aux_mapeo,
#'               inherit.aes = F, alpha = 0, color = "blue") +
#'       geom_sf(data = man,
#'               inherit.aes = F, alpha = 0, color = "red") +
#'       geom_sf(data = puntos,
#'               inherit.aes = F, alpha = 1, color = "red") +
#'       geom_sf_label(data = puntos, color = "red",
#'                     inherit.aes = F, aes(label = MANZANA), hjust = "inward",
#'                     vjust = "inward", size = 2) +
#'       # scale_x_continuous(limits = c(caja[1], caja[3])) + scale_y_continuous(limits = c(caja[2],caja[4])) +
#'       guides(fill = "none") +
#'       theme_minimal() +
#'       ggtitle(glue::glue("Municipio: {unique(aux_mapeo$NOMBRE_MUN)}  \n {u_cluster}: {i}")) +
#'       labs(subtitle =  cuotas) +
#'       theme(plot.title = element_text(hjust = 1), plot.subtitle = element_text(size = 10, hjust = 0))
#'
#'     ggsave(g, filename= sprintf("%s.png", i),
#'            path=dir,width = 11,height = 8.5,units = "in",dpi = "print", bg = "white")
#'   }
#'   beepr::beep()
#'
#' }
#'
#' #' Title
#' #'
#' #' @param diseño
#' #' @param shp
#' #' @param id
#' #' @param zoom
#' #' @param dir
#' #' @param ajustar_cuotas
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' sustituir_muestra_ine <- function(diseño, shp, id, zoom, dir, ajustar_cuotas){
#'
#'   # sólo se puede sustituir el último nivel
#'   t_nivel <- diseño$niveles %>% filter(nivel == !!diseño$ultimo_nivel) %>% pull(variable)
#'   nivel <- diseño$niveles %>% filter(variable == !!t_nivel) %>% transmute(paste(tipo,nivel,sep = "_")) %>% pull(1)
#'   # nivel del conjunto al que pertenece
#'   nivel_anterior <- diseño$niveles %>% filter(nivel == diseño$ultimo_nivel -1) %>%
#'     transmute(paste(tipo,nivel,sep = "_")) %>% pull(1)
#'   # muestra del nivel
#'   muestra <- diseño$muestra[[t_nivel]]
#'   # conjnuto seleccionado
#'   subcluster <- muestra %>% filter(!!rlang::sym(nivel) == id) %>% pull(nivel_anterior)
#'   #nueva muestra
#'   nuevo <- diseño$MarcoMuestral %>%
#'     filter(!!rlang::sym(nivel_anterior) == subcluster) %>%
#'     anti_join(muestra) %>%
#'     muestreaR:::agrupar_nivel(readr::parse_number(nivel)) %>%
#'     mutate(total = sum(!!rlang::sym(diseño$variable_poblacional))) %>%
#'     group_by(total, .add = T) %>%
#'     tidyr::nest() %>%
#'     ungroup %>%
#'     slice_sample(n = 1, weight_by = total)
#'
#'   # nuevo$data %>% pluck(1,"NOM_MUN")
#'   ####podría haber un error aquí#######
#'   manzanas <- diseño$muestra$MZA %>% filter(!!rlang::sym(nivel) == id) %>% nrow
#'   nuevas_manzanas <- nuevo %>% tidyr::unnest(data) %>% slice_sample(n = manzanas) %>%
#'     group_by(across(strata_1:total),cluster_0) %>% tidyr::nest()
#'   #####################################
#'   #recalcular n_i$cluster_0
#'   cl0_quitar <- diseño$muestra$MZA %>% filter(!!rlang::sym(nivel) == id) %>% pull(cluster_0)
#'   enc_0 <- diseño$n_i$cluster_0 %>% filter(cluster_0 %in% cl0_quitar) %>% pull(n_0)
#'   if(length(cl0_quitar) != nrow(nuevas_manzanas)) stop(glue::glue("Volver a correr. La {t_nivel} muestreada no tiene el mismo número de manzanas que la que desea sustituir."))
#'   diseño$n_i$cluster_0 <- diseño$n_i$cluster_0 %>% filter(!cluster_0 %in% nuevas_manzanas$cluster_0) %>%
#'     bind_rows(
#'       diseño$n_i$cluster_0 %>% filter(cluster_0 %in% nuevas_manzanas$cluster_0) %>%
#'         mutate(n_0 = enc_0)
#'     ) %>% arrange(cluster_0)
#'   #sustituir muestra
#'   diseño$muestra[[t_nivel]] <- diseño$muestra[[t_nivel]] %>% filter(!!rlang::sym(nivel) != id) %>% bind_rows(nuevo)
#'   diseño$muestra$MZA <- diseño$muestra$MZA %>% filter(!!rlang::sym(nivel) != id) %>% bind_rows(nuevas_manzanas)
#'   #calcular cuota del nuevo
#'
#'   aux_cuotas <- encuestar:::cuotas_ine(diseño, ajustar = ajustar_cuotas)
#'   cuotas_nuevo <- aux_cuotas %>% anti_join(diseño$cuotas, by = nivel)
#'
#'   cuotas_viejo <- diseño$cuotas %>% filter(!!rlang::sym(nivel) == id)
#'
#'   diseño$cuotas <- diseño$cuotas %>% anti_join(cuotas_viejo, by = nivel) %>%
#'     bind_rows(
#'       cuotas_nuevo
#'     ) %>% arrange(!!rlang::sym(nivel))
#'
#'   # library(ggmap)
#'   # ggmap::ggmap(ggmap::get_map())
#'   encuestar:::google_maps_ine(diseño = diseño, shp = shp, zoom = zoom, dir = dir)
#'
#'   return(diseño)
#'   # diseño %>% readr::write_rds(glue::glue("auditoria/data/diseño_qro{i}.rda"))
#' }
