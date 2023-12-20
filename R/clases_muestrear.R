Muestreo <- R6::R6Class(
  classname = "Muestra",
  public = list(
    shp_df = NULL,
    shp_dl = NULL,
    shp_mun = NULL,
    shp_sec = NULL,
    shp_mza = NULL,
    shp_loc = NULL,
    lista_nominal = NULL,
    electoral = NULL,
    entidad = NULL,
    entrevistas_totales = NULL,
    upm = NULL,
    entrevistas_por_upm = NULL,
    id_upm = NULL,
    llave_muestreo = NULL,
    variable_poblacional = NULL,
    Cartografia = NULL,
    Diseño = NULL,
    initialize = function(shp_df = shp_df,
                          shp_dl = shp_dl,
                          shp_mun = shp_mun,
                          shp_sec = shp_sec,
                          shp_mza = shp_mza,
                          shp_loc = shp_loc,
                          electoral = electoral,
                          lista_nominal = lista_nominal,
                          entidad = entidad,
                          entrevistas_totales = entrevistas_totales,
                          upm = upm,
                          entrevistas_por_upm = entrevistas_por_upm,
                          id_upm = id_upm,
                          llave_muestreo = llave_muestreo,
                          variable_poblacional = variable_poblacional){

      # Datos cartográficos -----------------------------------------------------

      self$shp_df <- shp_df
      self$shp_dl <- shp_dl
      self$shp_mun <- shp_mun
      self$shp_sec <- shp_sec
      self$shp_mza <- shp_mza
      self$shp_loc <- shp_loc

      self$Cartografia <- Cartografia$new(Muestra = self)

      # Datos de muestreo -------------------------------------------------------

      self$lista_nominal <- lista_nominal
      self$electoral <- electoral
      self$entidad <- entidad
      self$entrevistas_totales <- entrevistas_totales
      self$upm <- upm
      self$entrevistas_por_upm <- entrevistas_por_upm
      self$id_upm <- id_upm
      self$llave_muestreo <- llave_muestreo
      self$variable_poblacional <- variable_poblacional

      self$Diseño <- Diseño$new(Muestra = self)

    }
  )
)

Cartografia <- R6::R6Class(
  classname = "Cartografía",
  public = list(
    Muestra = NULL,
    shp_df = NULL,
    shp_dl = NULL,
    shp_mun = NULL,
    shp_sec = NULL,
    shp_mza = NULL,
    shps = NULL,
    initialize = function(Muestra = Muestra){

      self$shp_df <- Muestra$shp_df %>%
        transmute(across(.cols = c(ENTIDAD, DISTRITO_F), .fns = ~as.character(.x))) %>%
        st_make_valid()

      self$shp_dl <- Muestra$shp_dl %>%
        transmute(.cols = across(c(ENTIDAD, DISTRITO_L), .fns = ~as.character(.x))) %>%
        st_make_valid()

      self$shp_mun <- Muestra$shp_mun %>%
        rename(NOMBRE_MUN = NOMBRE) %>%
        mutate(across(.cols = ENTIDAD:MUNICIPIO, .fns = ~as.character(.x))) %>%
        select(ENTIDAD:NOMBRE_MUN) %>%
        st_make_valid()

      self$shp_sec <- Muestra$shp_sec %>%
        rename(DISTRITO_F = DISTRITO) %>%
        mutate(across(.cols = ENTIDAD:SECCION, .fns = ~as.character(.x))) %>%
        select(ENTIDAD:SECCION) %>%
        st_make_valid()

      shp_mza <- Muestra$shp_mza %>%
        filter(st_is_valid(.), STATUS == 1) %>%
        select(ENTIDAD:MANZANA) %>%
        mutate(across(.cols = ENTIDAD:MANZANA, .fns = ~as.character(.x))) %>%
        st_make_valid()

      shp_loc <- Muestra$shp_loc %>%
        rename(MANZANA = NOMBRE) %>%
        mutate(across(.cols = ENTIDAD:LOCALIDAD, .fns = ~as.character(.x))) %>%
        select(ENTIDAD:MANZANA) %>%
        st_make_valid()

      self$shp_mza <- bind_rows(shp_mza, shp_loc) %>%
        arrange(as.numeric(SECCION))

      self$shps <- encuestar:::crear_Cartografia(shp_df = Muestra$shp_df,
                                                 shp_dl = Muestra$shp_dl,
                                                 shp_mun = Muestra$shp_mun,
                                                 shp_sec = Muestra$shp_sec,
                                                 shp_mza = Muestra$shp_mza,
                                                 shp_loc = Muestra$shp_loc)

    },
    graficar_mapa = function(lflt = NULL, bd, nivel){
      nivel_p <- if(nivel == "MANZANA"){
        "MZA"
      } else nivel
      encuestar:::graficar_mapa_muestra_ine(lflt = lflt,
                                            muestra = if(!is.data.frame(bd)) bd %>% purrr::pluck(nivel_p) %>% tidyr::unnest(data) else bd,
                                            shp = self$shps,
                                            nivel = nivel)
    },
    crear_mapas = function(diseño, shp, zoom, dir){
      google_maps_ine(diseño, shp = shp, zoom = zoom, dir = dir)
    }
  )
)

Diseño <- R6::R6Class(
  classname = "Diseño muestral",
  public = list(
    Muestra = NULL, # EVALUAR SI ES NECESARIO
    MarcoMuestral = NULL,
    InformacionElectoral = NULL,
    # poblacion = NULL, # la población es en realidad el marco muestral - PARÁMETRO DE USUARIO
    entrevistas_totales = NULL, # antes se llamaba 'n' - PARÁMETRO DE USUARIO
    # upm = NULL, # antes se llamaba 'unidad_muestreo' - PARÁMETRO DE USUARIO (private)
    entrevistas_por_upm = NULL, # antes se llamaba 'n_0' - PARÁMETRO DE USUARIO
    id_upm = NULL, # PARÁMETRO DE USUARIO
    llave_muestreo = NULL, # PARÁMETRO DE USUARIO
    variable_poblacional = NULL, # PARÁMETRO DE USUARIO -COMIENZAN PARAMETROS POR DEFECTO DE MUESTREAR
    ultimo_nivel = -1,
    n_i = list(),
    niveles = tibble::tibble(nivel = NULL,
                             tipo = NULL,
                             descripcion = NULL,
                             llave = NULL,
                             aprobado = NULL,
                             unidades = NULL,
                             plan_muestra = NULL),
    muestra_obj = NULL, # antes se llamaba 'muestra' pero hay que evitar conflictos
    cuotas = NULL,
    n_sustitucion = 0,
    dir.exportar = NULL,
    sobre_muestra = NULL,
    initialize = function(Muestra = Muestra){
      self$Muestra <- Muestra
      self$MarcoMuestral <- encuestar:::crear_MarcoMuestral(ln = Muestra$lista_nominal,
                                                            shp_mza = Muestra$shp_mza,
                                                            shp_loc = Muestra$shp_loc,
                                                            shp_mun =  Muestra$shp_mun)

      self$InformacionElectoral <- Muestra$electoral %>%
        mutate(seccion = as.character(seccion)) %>%
        left_join(self$MarcoMuestral %>% group_by(SECCION) %>%
                    summarise(across(c(lista_nominal, contains("LN22_")), ~sum(.x))),
                  by = c("seccion" = "SECCION"))

      # Revisar o hacer check de comparación entre la lista nominal y
      # la lista nominal creada

      # self$lista_nominal$`LISTA NOMINAL` |> sum()
      # self$Marco_muestral$lista_nominal |> sum()

      # INICIAR RECONSTRUCCIÓN DEL PLAN DE MUESTRA

      # self$poblacion <- self$MarcoMuestral ENTRÓ EN DESUSO
      self$entrevistas_totales <- Muestra$entrevistas_totales
      private$upm <- Muestra$upm
      self$entrevistas_por_upm <- Muestra$entrevistas_por_upm
      self$variable_poblacional <- Muestra$variable_poblacional

      # RECONSTRUCCIÓN COMPLETADA

      self$niveles <- self$agregar_nivel(variable = Muestra$id_upm,
                                         tipo = "cluster",
                                         descripcion = Muestra$upm,
                                         llave = Muestra$llave_muestreo)
      self$n_i <- self$plan_muestra(nivel = self$ultimo_nivel)
    },
    print = function(){
      mensaje <- cat(
        glue::glue(
          "Diseño que representa a la población de {self$MarcoMuestral$nombre} con una muestra de tamaño {scales::comma(self$entrevistas_totales)} cuya unidad de muestreo es {private$upm}.

                               Se realizarán {self$entrevistas_por_upm} entrevistas por unidad mínima.

                               Para efectos del diseño muestral se utilizará {self$variable_poblacional} del censo 2020 del INEGI para cuantificar el tamaño de la población."))
      return(mensaje)
    },
    regiones = function(id_upm, regiones){
      self$MarcoMuestral <- encuestar:::crear_regiones(self$MarcoMuestral,
                                                       id = id_upm,
                                                       regiones = regiones)

    },
    agregar_nivel = function(variable, tipo, descripcion, llave){
      if(!tipo %in% c("strata", "cluster")) stop("Tipo debe ser igual a cluster o strata")
      # Al último nivel le agregamos uno
      self$ultimo_nivel <- self$ultimo_nivel+1
      # Modificar el marco muestral
      self$MarcoMuestral <- self$MarcoMuestral %>%
        {if(self$ultimo_nivel!=0) encuestar:::agrupar_nivel(., nivel=self$ultimo_nivel)
          else .
        } %>%
        group_by(!!sym(variable), add=T) %>%
        mutate("{tipo}_{self$ultimo_nivel}":= cur_group_id()) %>%
        ungroup()
      # Modificar sel niveles
      self$niveles <- self$niveles %>%
        add_row(.data = tibble(nivel=self$ultimo_nivel,
                               variable=variable,
                               tipo=tipo,
                               descripcion=descripcion,
                               llave=llave,
                               unidades=NA_integer_,
                               aprobado=F,
                               plan_muestra=F))

      return(self$niveles)
    },
    plan_muestra = function(nivel, criterio, unidades_nivel, manual){
      nivel_l <- nivel
      if(nivel_l==0){
        # Se asigna el nivel 0
        res <- self$MarcoMuestral %>%
          group_by(cluster_0) %>%
          summarise(m_0=self$entrevistas_por_upm,
                    n_0=self$entrevistas_por_upm)
        # Se le agrega el total de unidades al nivel
        self$niveles <- self$niveles %>%
          mutate(unidades = if_else(nivel == 0,
                                    ceiling(self$entrevistas_totales/self$entrevistas_por_upm),
                                    as.numeric(unidades)))
        # Se etiqueta en la lista
        res <- list(cluster_0=res)
      }
      else{
        # Cuando es el último nivel
        if(nivel_l == self$ultimo_nivel){
          res <- asignar_m(self, unidades_nivel = ) %>%
            left_join(asignar_n(self))
          res <- purrr::set_names(list(res), glue::glue("{self$niveles %>%
                                                     filter(nivel==nivel_l) %>%
                                                     pull(tipo)}_{nivel_l}"))
        }
        # Si no es nivel=0 ni es nivel=ultimo_nivel
        else{
          # Si no es el último nivel
          # Primero se asigna m
          # Después se asigna n
          if(criterio != "manual"){
            res <- encuestar:::asignar_m(diseño = self,
                                         criterio = criterio,
                                         unidades_nivel = unidades_nivel,
                                         manual = manual) %>%
              left_join(encuestar:::asignar_n(diseño = self))
          } else{

            res <- encuestar:::asignar_m(diseño = self,
                                         criterio = criterio,
                                         unidades_nivel = unidades_nivel, manual = manual) %>%
              mutate(!!rlang::sym(glue::glue("n_{nivel_l}")) := !!rlang::sym(glue::glue("m_{nivel_l}"))/sum(!!rlang::sym(glue::glue("m_{nivel_l}"))) * self$n)
          }

          # Se etiqueta
          res <- purrr::set_names(list(res), glue::glue("{self$niveles %>%
                                                     filter(nivel==nivel_l) %>%
                                                     pull(tipo)}_{nivel_l}"))
        }
      }
      self$niveles <- self$niveles %>%
        mutate(plan_muestra=(nivel<=nivel_l))
      self$n_i <- c(self$n_i, res)
      return(res)
    },
    eliminar_nivel = function(nivel){
      aux <- nivel
      self$niveles <- self$niveles %>%
        filter(nivel<aux)
      self$ultimo_nivel <- nivel-1
      self$MarcoMuestral <- self$MarcoMuestral %>%
        select(-matches(glue::glue("(cluster|strata)_[{nivel}-9]")))
      self$n_i <- self$n_i[-grep(glue::glue("(cluster|strata)_[{nivel}-9]"),
                                 names(self$n_i))]
    },
    fpc = function(nivel){
      self$MarcoMuestral <- encuestar:::calcular_fpc(self, nivel = nivel)
    },
    extraer_muestra = function(nivel){
      m <- encuestar:::calcular_muestra_nivel(self, nivel = nivel)
      aux <- m %>% purrr::pluck(length(m))
      if(nivel == self$ultimo_nivel){
        if(self$niveles %>% filter(nivel == 0) %>% pull(unidades) != nrow(aux)){

          ajuste <- ((self$niveles %>% filter(nivel == 0) %>% pull(unidades)) - nrow(aux))*self$entrevistas_por_upm

          nuevo <- self$n_i$cluster_0 %>%
            semi_join(aux) %>%
            sample_n(size = abs(ajuste), replace = T) %>%
            mutate(sumar = sign(ajuste)) %>%
            group_by(cluster_0) %>%
            summarise(m_0 = unique(m_0),
                      n_0 = unique(n_0),
                      sumar = sum(sumar)) %>%
            mutate(n_0 = n_0 + sumar) %>%
            select(-sumar)

          self$n_i$cluster_0 <- self$n_i$cluster_0 %>%
            anti_join(nuevo, by = "cluster_0") %>%
            bind_rows(nuevo) %>%
            arrange(cluster_0)
        }
      }
      self$muestra_obj <- self$muestra_obj |> append(m)
    },
    calcular_cuotas = function(ajustar = T){
      self$cuotas <- encuestar:::cuotas_ine(diseño = self, ajustar = ajustar)
    },
    revisar_muestra = function(prop_vars, var_extra){
      a <- encuestar:::llaves(self)
      b <- encuestar:::plan(self)
      c <- encuestar:::revision_ine(self = self, prop_vars = prop_vars, var_extra = var_extra)
      return(list(a, b, c))
    },
    exportar = function(shp = self$Muestra$Cartografia$shps, zoom = 16, carpeta = "./Insumos"){
      self$dir.exportar <- carpeta
      if(!file.exists(carpeta)) dir.create(carpeta)
      if(!file.exists(glue::glue("{carpeta}/Mapas"))) dir.create(glue::glue("{carpeta}/Mapas"))

      # self$Muestra$Cartografia$crear_mapas(diseño = self,
      #                                      shp = self$Muestra$Cartografia$shps,
      #                                      zoom = zoom,
      #                                      dir = glue::glue("{carpeta}/Mapas"))

      self$cuotas %>% readr::write_excel_csv(glue::glue("{carpeta}/cuotas.csv"))
      readr::write_rds(self, glue::glue("{carpeta}/diseño.rda"))
      shp %>% readr::write_rds(glue::glue("{carpeta}/shp.rda"))
    },
    sustituir_muestra = function(shp = self$Muestra$Cartografia$shps, id, zoom = 16, ajustar_cuotas = T, carpeta = "./Insumos"){
      self <- encuestar:::sustituir_muestra_ine(diseño = self, shp = shp, id = id, zoom = zoom, dir = carpeta, ajustar_cuotas = ajustar_cuotas)

      if(!file.exists(glue::glue("{carpeta}/Mapas/Eliminadas"))) dir.create(glue::glue("{carpeta}/Mapas/Eliminadas"))

      file.rename(glue::glue("{carpeta}/Mapas/{id}.png"),
                  glue::glue("{carpeta}/Mapas/Eliminadas/{id}.png"))

      self$n_sustitucion <- self$n_sustitucion + 1
      readr::write_rds(self, glue::glue("{carpeta}/diseño{self$n_sustitucion}.rda"))
      self$cuotas %>% readr::write_excel_csv(glue::glue("{carpeta}/cuotas{self$n_sustitucion}.csv"))
    }
  ),
  private = list(upm = NULL)
)
