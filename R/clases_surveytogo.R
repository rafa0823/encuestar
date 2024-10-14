#' Esta es la clase Encuesta
#' @description La clase Encuesta contiene todos los metodos, y atributos que puede tener una encuesta.
#' @field respuestas Base de datos de respuestas
#' @field muestra Base de datos de muestra.
#' @field dicionario Base de datos de diccionario
#' @export
#' @import dplyr ggplot2 tidyr sf purrr stringr

Encuesta <- R6::R6Class("Encuesta",
                        public = list(
                          respuestas = NULL,
                          n_simulaciones = NA,
                          opinometro_id = NULL,
                          quitar_vars = NULL,
                          cuestionario = NULL,
                          bd_categorias = NULL,
                          muestra = NULL,
                          auditoria_telefonica=NA,
                          bd_correcciones = NULL,
                          Resultados = NULL,
                          shp_completo = NULL,
                          shp = NULL,
                          tipo_encuesta = NULL,
                          mantener = NULL,
                          auditoria = NULL,
                          patron = NA,
                          auditar = NA,
                          vars_tendencias = NULL,
                          sin_peso = NA,
                          rake = NA,
                          mantener_falta_coordenadas = NULL,
                          dir_app = NULL,
                          #' @description
                          #' Create a person
                          #' @param respuestas Name of the person
                          #' @param diccionario Hair colour
                          initialize = function(respuestas = NA,
                                                n_simulaciones = NULL,
                                                opinometro_id = NULL,
                                                bd_categorias = NULL,
                                                quitar_vars = NA,
                                                muestra = NA,
                                                auditoria_telefonica = NA,
                                                bd_correcciones = NULL,
                                                cuestionario = NA,
                                                shp = NA,
                                                tipo_encuesta = "ine",
                                                mantener = NA,
                                                patron = NA,
                                                auditar = NA,
                                                vars_tendencias = NULL,
                                                sin_peso = F,
                                                rake = T,
                                                mantener_falta_coordenadas = F,
                                                dir_app = "auditoria"
                          ) {
                            sf_use_s2(F)
                            tipo_encuesta <- match.arg(tipo_encuesta, c("inegi","ine"))
                            self$sin_peso <- sin_peso
                            self$quitar_vars <- quitar_vars
                            self$rake <- rake
                            self$tipo_encuesta <- tipo_encuesta
                            self$patron <- patron
                            self$auditar <- auditar
                            self$vars_tendencias <- vars_tendencias
                            self$mantener_falta_coordenadas <- mantener_falta_coordenadas
                            self$n_simulaciones <- if("logical" %in% class(respuestas)) n_simulaciones else 0
                            self$opinometro_id <- opinometro_id
                            self$bd_categorias <- bd_categorias
                            # Valorar si no es mejor un active binding
                            un <- muestra$niveles %>% filter(nivel == muestra$ultimo_nivel)
                            nivel <- un %>% unite(nivel, tipo, nivel) %>% pull(nivel)
                            var_n <- un %>% pull(variable)

                            # Valorar active binding
                            self$cuestionario <- Cuestionario$new(documento = cuestionario, patron)
                            # Valorar active binding
                            self$auditoria_telefonica <- auditoria_telefonica %>% distinct(SbjNum, .keep_all = T)

                            # Base de respuestas incorrectas y su correccion
                            self$bd_correcciones <- bd_correcciones

                            self$shp_completo <- shp

                            self$shp <-
                              shp$shp %>%
                              purrr::pluck(var_n) %>%
                              inner_join(muestra$muestra %>%
                                           purrr::pluck(var_n) %>%
                                           unnest(data) %>%
                                           distinct(!!rlang::sym(var_n) := !!rlang::sym(var_n),
                                                    !!rlang::sym(nivel)))
                            self$mantener <- mantener

                            catalogo_variables <-
                              catalogo_variables |>
                              bind_rows(self$cuestionario$diccionario |>
                                          select(variable = llaves) |>
                                          mutate(plataforma = "cuestionario",
                                                 primer_nivel = "cuestionario",
                                                 segundo_nivel = dplyr::if_else(condition = variable %in% c("cluster",
                                                                                                            "edad",
                                                                                                            "sexo"),
                                                                                true = "sistema",
                                                                                false = "cuestionario")))

                            if("data.frame" %in% class(respuestas)) {

                              intentos_efectivos <-
                                respuestas |>
                                select(SbjNum, num_range("INT", 1:20)) |>
                                mutate(across(.cols = !SbjNum, .fns = ~ as.character(.x))) |>
                                tidyr::pivot_longer(cols = !SbjNum, names_to = "variable", values_to = "rechazo") |>
                                filter(grepl(pattern = 'Iniciar entrevista', x = rechazo)) |>
                                mutate(intento_efectivo = gsub(pattern = "INT", replacement = "", x = variable)) |>
                                select(SbjNum, intento_efectivo)

                              geolocalizacion_efectiva <-
                                purrr::pmap_df(.l = list(ids = intentos_efectivos %>% pull(SbjNum),
                                                         intento_efectivo = intentos_efectivos %>% pull(intento_efectivo)),
                                               .f = ~ obtener_ubicacionEfectiva_surveyToGo(bd_respuestas = respuestas,
                                                                                           id = ..1,
                                                                                           intento_efectivo = ..2))

                              respuestas <-
                                respuestas |>
                                left_join(geolocalizacion_efectiva, by = "SbjNum") |>
                                mutate(Latitude = GPS_INT_LA,
                                       Longitude = GPS_INT_LO) |>
                                transmute(across(all_of(catalogo_variables |>
                                                          filter(plataforma %in% c("surveytogo", "encuestar", "cuestionario"),
                                                                 segundo_nivel %in% c("sistema", "cuestionario")) |>
                                                          pull(variable))))

                            }

                            if(!is.null(opinometro_id)) {

                              opinometro <- Opinometro$new(id_cuestionarioOpinometro = self$opinometro_id,
                                                           diccionario = self$cuestionario$diccionario)

                              respuestas <- opinometro$bd_respuestas_cuestionario

                            }

                            if(!is.null(bd_categorias)) {

                              respuestas <-
                                respuestas |>
                                left_join(bd_categorias,
                                          by = "SbjNum")

                            }

                            if(!("data.frame" %in% class(respuestas)) & !is.null(n_simulaciones)){
                              respuestas <- self$simular_surveytogo(cuestionario = self$cuestionario,
                                                                    n = self$n_simulaciones,
                                                                    diseño = muestra,
                                                                    shp = shp)
                            }

                            self$respuestas <- Respuestas$new(base = respuestas %>% mutate(cluster_0 = SbjNum),
                                                              encuesta = self,
                                                              catalogo = catalogo_variables,
                                                              mantener_falta_coordenadas = self$mantener_falta_coordenadas,
                                                              muestra_completa = muestra,
                                                              patron = patron,
                                                              nivel = nivel,
                                                              var_n = var_n)

                            # Muestra (recalcula fpc)
                            self$muestra <- Muestra$new(muestra = muestra,
                                                        respuestas = self$respuestas$base,
                                                        nivel = nivel,
                                                        var_n = var_n)

                            # Informacion muestral
                            self$respuestas$vars_diseno(muestra = self$muestra, var_n = var_n, tipo_encuesta = self$tipo_encuesta)
                            # Diseno
                            self$muestra$extraer_diseno(respuestas = self$respuestas$base,
                                                        marco_muestral = self$muestra$muestra$poblacion$marco_muestral,
                                                        tipo_encuesta = self$tipo_encuesta,
                                                        sin_peso = self$sin_peso,
                                                        rake = self$rake)

                            print(glue::glue("La base de campo contiene ", as.character(nrow(respuestas)), " filas"))
                            print(glue::glue("La base de eliiminadas contiene ", as.character(nrow(self$auditoria_telefonica)), " filas"))
                            print(glue::glue("La base de entrevistas efectivas contiene ", as.character(nrow(self$muestra$diseno$variables)), " filas"))

                            #Preguntas
                            self$Resultados <- Resultados$new(encuesta = self, diseno = NULL, diccionario = NULL, tema = tema_morant())

                            # Shiny app de auditoria
                            self$dir_app <- dir_app
                            self$auditoria <- Auditoria$new(self, tipo_encuesta = self$tipo_encuesta, dir = self$dir_app)
                            file.copy(overwrite = FALSE,
                                      from = system.file("constantes_y_funciones/constantes.R",
                                                         package = "encuestar",
                                                         mustWork = TRUE),
                                      to = "R")
                            file.copy(overwrite = FALSE,
                                      from = system.file("constantes_y_funciones/funciones.R",
                                                         package = "encuestar",
                                                         mustWork = TRUE),
                                      to = "R")
                            source(file = paste0(getwd(), "/R/constantes.R"))
                            beepr::beep()
                            print(glue::glue("Las siguientes variables no son de sistema, plataforma o están en el diccionario"))
                            print(match_dicc_base(self), n = Inf)
                          },

                          simular_surveytogo = function(cuestionario, n, diseño, shp){
                            #simular respuestas
                            respuestas <- cuestionario$diccionario %>% mutate(n = n) %>%
                              pmap_dfc(function(llaves, respuestas, tipo_pregunta, n,...){
                                if(tipo_pregunta == "numericas") {
                                  aux_r <- respuestas[1] %>%
                                    str_split(pattern = "-") %>%
                                    pluck(1) %>%
                                    as.numeric()

                                  respuestas <- seq(aux_r[1], aux_r[2]) %>%
                                    as.character() %>%
                                    c(respuestas[2])
                                }

                                tibble(llaves = sample(respuestas,size = n, replace = T)) %>%
                                  set_names(llaves)
                              })
                            #ubicación aleatoria en muestra
                            secc <- diseño$poblacion$marco_muestral %>%
                              semi_join(diseño$muestra$MZA %>%
                                          distinct(cluster_2),
                                        by = "cluster_2") %>%
                              distinct(cluster_2,SECCION)

                            respuestas <- shp$shp$SECCION %>%
                              semi_join(secc) %>%
                              st_sample(size = n) %>%
                              st_coordinates() %>%
                              as_tibble %>%
                              sf::st_as_sf(coords = c("X","Y"), crs = "+init=epsg:4326") %>%
                              st_join(shp$shp$SECCION) %>%
                              select(SECCION) %>%
                              left_join(secc) %>%
                              transmute(SECCION = as.character(cluster_2)) %>%
                              bind_cols(st_coordinates(.)) %>%
                              as_tibble() %>% select(-geometry) %>% rename(Longitude = X, Latitude = Y) %>% bind_cols(respuestas) %>%
                              tibble::rownames_to_column(var = "SbjNum") %>% mutate(SbjNum = as.numeric(SbjNum))
                            #simular sexo y edad para postestratificación
                            respuestas <- respuestas %>%
                              mutate(sexo = sample(c("Hombre", "Mujer"),
                                                   size = n, replace = T),
                                     edad = sample(18:100,size = n, replace = T))
                            #formato de base
                            respuestas <- respuestas %>%
                              mutate(Srvyr = NA,
                                     Date = sample(x = seq(from = lubridate::today(),
                                                           to = lubridate::today() + lubridate::days(3),
                                                           by = "days"),
                                                   size = nrow(respuestas),
                                                   replace = T),
                                     INT15 = NA,
                                     T_Q = NA)

                            respuestas <- respuestas %>%
                              relocate(Srvyr, Date, .before = Longitude) %>%
                              relocate(SECCION, INT15, .after = Latitude)
                          },

                          error_muestral_maximo = function(quitar_patron = NULL){
                            aux <- self$cuestionario$diccionario %>% filter(tipo_pregunta == "multiple")
                            if(!is.null(quitar_patron)) {
                              quitar_patron <- paste(quitar_patron, collapse = "|")
                              aux <- aux %>% filter(!grepl(quitar_patron, x = llaves))
                            }

                            aux <- aux %>% mutate(n = map_int(respuestas,~length(.x)))
                            aux <- aux %>%
                              pull(llaves) %>% map_df(~{
                                # nas <- self$respuestas$base %>% summarise(any(is.na(c_across(.x)))) %>% pull(1)
                                survey::svymean(survey::make.formula(.x), design = self$muestra$diseno, na.rm = T) %>%
                                  tibble::as_tibble(rownames = "respuesta") %>%
                                  # rename(SE = 3) %>%
                                  mutate(pregunta = .x,
                                         # tiene_na = !!nas,
                                         # respuesta = stringr::str_replace(string = respuesta,
                                         #                                  pattern = as.character(.x),
                                         #                                  replacement = "")
                                  ) %>% select(respuesta, mean, SE)
                              }) %>% mutate(SE = qnorm(.95)*SE)

                            labels <- aux %>% summarise(inf = quantile(SE,.25,na.rm = T),
                                                        mid = quantile(SE,.5,na.rm = T),
                                                        sup = quantile(SE,.75,na.rm = T),
                                                        max = max(SE,na.rm = T)
                            ) %>% mutate(iqr_min = inf-1.5*(sup-inf),
                                         iqr_max = sup + 1.5*(sup-inf)) %>%
                              pivot_longer(everything(), names_to = "stat", values_to = "valor")

                            a <- aux %>%
                              ggplot() +
                              geom_boxplot(aes(x = 0, y = SE)) +
                              geom_label(data = labels, aes(x = 0, y = valor, label = scales::percent(valor)),
                                         hjust = 0, vjust = 0, nudge_x = .01) + labs(x = NULL) +
                              scale_y_continuous(labels = scales::percent_format(1))

                            b <- aux %>% filter(SE >= labels %>% filter(stat == "sup") %>% pull(valor)) %>%
                              ggplot() + geom_col(aes(y = reorder(respuesta, SE), x = SE)) +
                              geom_vline(xintercept = labels %>% filter(stat == "sup") %>% pull(valor))+
                              labs(y = NULL)+
                              scale_x_continuous(labels = scales::percent_format(1))

                            print(a + b)
                            return(aux)
                          },

                          exportar_entregable = function(carpeta = "entregables", agregar = NULL, quitar = NULL){

                            if(!file.exists(carpeta)) dir.create(carpeta)

                            # Exportar bd
                            exportar_bd(self, carpeta, agregar, quitar)

                            # Exportar diccionario
                            self$cuestionario$diccionario %>%
                              unnest(respuestas) %>%
                              readr::write_excel_csv(glue::glue("{carpeta}/diccionario.csv"))

                          }),

                        private = list()
)

#' Esta es la clase Respuestas
#' @export
Respuestas <- R6::R6Class("Respuestas",
                          inherit = Encuesta,
                          public = list(
                            eliminadas = NULL,
                            cluster_corregido = NULL,
                            base = NULL,
                            catalogo = NULL,
                            n = NULL,
                            m = NULL,
                            sin_coordenadas = NULL,
                            mantener_falta_coordenadas = NULL,
                            #' @description
                            #' Crear respuesta
                            #' @param base Base de datos de respuestas.
                            initialize = function(base,
                                                  catalogo = NULL,
                                                  encuesta,
                                                  muestra_completa,
                                                  mantener_falta_coordenadas,
                                                  patron,
                                                  nivel,
                                                  var_n) {
                              shp <- encuesta$shp
                              mantener <- encuesta$mantener
                              diccionario <- encuesta$cuestionario$diccionario

                              if(!identical(names(encuesta$auditoria_telefonica), c("SbjNum", "razon"))) stop("Los nombres de las columnas de la base de datos de auditoría telefónica deben ser: 'SbjNum, razon'")

                              if(!is.null(encuesta$bd_correcciones)){
                                if(!identical(names(encuesta$bd_correcciones), c("SbjNum", "llave", "capturada", "correccion"))) stop("Los nombres de las columnas de la base de datos de correcciones deben ser: 'SbjNum, llave, capturada, correccion'")
                              }

                              auditoria_telefonica <- encuesta$auditoria_telefonica
                              bd_correcciones <- encuesta$bd_correcciones
                              muestra <- muestra_completa$muestra %>% purrr::pluck(var_n)

                              self$base <- base
                              self$catalogo <- catalogo

                              # Parar si faltan variables de sistema o del cuestionario
                              self$nombres(catalogo_variables = self$catalogo, self$base, diccionario)

                              # Quitar patrones a respuestas
                              self$q_patron(self$base, diccionario, patron)

                              # Parar si opciones de respuesta no coinciden con diccionario
                              self$categorias(self$base, diccionario)

                              # Advertir si hay opciones de respuestas que tienen count = 0
                              self$respuestas_sin_seleccion(bd = self$base, diccionario)

                              # Limpiar las que no pasan auditoria telefonica
                              self$eliminar_auditoria_telefonica(auditoria_telefonica)

                              # Corregir respuestas registradas mal por los encuestadores
                              if(!is.null(bd_correcciones)) {

                                self$base <- self$corregir_respuestas(respuestas = self$base, bd_correcciones_raw = bd_correcciones)

                              }

                              self$crear_variablesSecundarias(diccionario = diccionario)

                              # Mantener respuestas que no tienen coordenadas
                              if(mantener_falta_coordenadas){

                                self$coordenadas_faltantes()

                              } else {

                                # Limpiar las respuestas que no tienen coordenadas
                                self$eliminar_falta_coordenadas()

                              }

                              # Eliminar entrevistas cuyo cluster no pertenece a la muestra
                              self$eliminar_fuera_muestra(self$base, muestra, nivel, var_n)

                              # Corregir cluster equivocado
                              self$correccion_cluster(self$base, shp, mantener, nivel, var_n)

                              # Calcular distancia de la entrevista al cluster correcto
                              self$calcular_distancia(base = self$base,
                                                      encuesta = encuesta,
                                                      muestra = muestra_completa,
                                                      var_n = var_n, nivel = nivel)

                              # Limpiar las que no tienen variables de diseno
                              # self$eliminar_faltantes_diseno() # no entiendo por qué
                              if(mantener_falta_coordenadas){
                                self$base <- self$base %>% bind_rows(
                                  self$sin_coordenadas
                                )
                              }

                              # Cambiar variables a tipo numerica
                              numericas <- diccionario %>% filter(tipo_pregunta == "numericas") %>%
                                pull(llaves)

                              self$base <- self$base %>%
                                mutate(across(all_of(numericas), ~readr::parse_number(.x)))

                              # self$eliminadas <- anti_join(base, self$base, by = "SbjNum")
                            },

                            nombres = function(catalogo_variables, bd, diccionario){

                              faltantes <-
                                catalogo_variables |>
                                filter(segundo_nivel %in% c("sistema", "cuestionario")) |>
                                filter(!variable %in% c("id", "fecha_inicio", "ubicacion_aplicada")) |>
                                anti_join(tibble(variable = names(bd)),
                                          by = 'variable')

                              # faltantes <- is.na(match(diccionario$llaves, names(bd)))
                              if(nrow(faltantes) > 0){
                                stop(glue::glue("Las siguientes variables no se encuentran en la base de datos: {paste(faltantes$variable, collapse = ', ')}"))
                              }

                            },

                            q_patron = function(bd, dicc, patron){
                              if(!is.na(patron)){
                                aux <- bd %>%
                                  mutate(across(c(all_of(dicc$llaves),where(is.character)),
                                                ~stringr::str_squish(gsub(x = .x,
                                                                          pattern = patron,
                                                                          ""))))
                                self$base <- aux
                              }
                            },

                            categorias = function(bd, diccionario){

                              discrepancia <-
                                diccionario %>%
                                filter(tipo_pregunta != "Abierta") |>
                                pull(llaves) %>%
                                map_df(~{
                                  res <-
                                    bd %>%
                                    count(across(all_of(.x))) %>%
                                    na.omit %>%
                                    pull(1)

                                  m <- match(res,
                                             diccionario %>%
                                               filter(llaves == .x) %>%
                                               mutate(respuestas = stringr::str_split(string = respuestas, pattern = "_")) |>
                                               pull(respuestas) |>
                                               pluck(1)
                                  )

                                  tibble(llave = .x,
                                         sin_respuestas = res[is.na(m)]
                                  )

                                })

                              if(nrow(discrepancia)>0){

                                print(glue::glue("La siguiente tabla muestra las respuestas en la base de campo que no están contempladas en el diccionario"))
                                print(discrepancia |>
                                        rename(codigo_pregunta = llave,
                                               respuesta_campo = sin_respuestas),
                                      n = Inf)
                                print(glue::glue("Revise las respuestas entre la base de campo y el cuestionario de procesamiento y corrija. Estas discrepancias no deberían existir."),
                                      immediate. = T)

                              }

                            },

                            respuestas_sin_seleccion = function(bd, diccionario){

                              bd_sinregistros <-
                                diccionario |>
                                filter(tipo_pregunta != "Abierta") |>
                                pull(llaves) %>%
                                purrr::map_df(.x = .,
                                              .f = ~ {
                                                respuestas_sin_registros <-
                                                  diccionario %>%
                                                  filter(llaves == .x) %>%
                                                  mutate(respuestas = stringr::str_split(string = respuestas, pattern = "_")) |>
                                                  pull(respuestas) |>
                                                  pluck(1) |>
                                                  as_tibble() |>
                                                  anti_join(bd |>
                                                              count(across(all_of(.x))) %>%
                                                              arrange(1) |>
                                                              pull(1) |>
                                                              as_tibble(),
                                                            by = "value") |>
                                                  pull()

                                                tibble(codigo_pregunta = .x,
                                                       respuesta_sin_registros = respuestas_sin_registros)

                                              })

                              if(nrow(bd_sinregistros) > 0) {

                                warning(paste("La siguiente tabla muestra las respuestas que tienen cero registros en la base de respuestas", sep = ""),
                                        immediate. = T)
                                print(bd_sinregistros, n = Inf)

                              }

                            },

                            eliminar_auditoria_telefonica = function(auditoria_telefonica){

                              if(("SbjNum" %in% names(self$base)) &
                                 ("SbjNum" %in% names(auditoria_telefonica))){
                                if(is.character(auditoria_telefonica$SbjNum)) auditoria_telefonica <- auditoria_telefonica %>% mutate(SbjNum = readr::parse_double(SbjNum))
                                # Se eliminan por no pasar la auditoria telefonica
                                n <- nrow(self$base)

                                self$eliminadas <- self$base %>% inner_join(auditoria_telefonica, by = "SbjNum")

                                self$base <- self$base %>%
                                  anti_join(auditoria_telefonica, by="SbjNum")

                                # Mandar mensaje
                                print(
                                  glue::glue("Se eliminaron {n-nrow(self$base)} encuestas por auditoria telefonica")
                                )
                              }
                              else cat("Identificador SbjNum no presente en alguna de las bases para eliminar por auditoria telefonica")
                              return(self$base)

                            },

                            corregir_respuestas = function(respuestas, bd_correcciones_raw){

                              variables_corregidas <-
                                bd_correcciones_raw |>
                                distinct(llave) |>
                                pull()

                              bd_correcciones <-
                                bd_correcciones_raw |>
                                select(!capturada) |>
                                mutate(llave = paste0(llave, "_correccion")) |>
                                tidyr::pivot_wider(id_cols = SbjNum,
                                                   names_from = llave,
                                                   values_from = correccion)

                              respuestas <-
                                respuestas |>
                                left_join(bd_correcciones, by = "SbjNum") %>%
                                mutate(across(.cols = all_of(variables_corregidas),
                                              .fns = ~ dplyr::if_else(condition = !is.na(get(paste0(cur_column(), "_correccion"))),
                                                                      true = get(paste0(cur_column(), "_correccion")),
                                                                      false = .))) |>
                                select(!all_of(names(bd_correcciones)[-1]))

                              return(respuestas)

                            },

                            crear_variablesSecundarias = function(diccionario){
                              self$base <-
                                self$base |>
                                dplyr::mutate(generacion = case_when(edad >= 18 & edad <= 25 ~ "Generación Z (18 a 25 años)",
                                                                     edad >= 26 & edad <= 40 ~ "Millenials (26 a 40 años)",
                                                                     edad >= 41 & edad <= 55 ~ "Generación X (41 a 55 años)",
                                                                     edad >= 56  ~ "Baby Boomers (56 años o más)"),
                                              generacion = factor(generacion, levels = c("Generación Z (18 a 25 años)",
                                                                                         "Millenials (26 a 40 años)",
                                                                                         "Generación X (41 a 55 años)",
                                                                                         "Baby Boomers (56 años o más)"))) |>
                                dplyr::mutate(amai_jefegrado = case_when(jefe_grado %in% c("No estudió", "No contesta") ~ 0,
                                                                         jefe_grado == "Primaria incompleta" ~ 6,
                                                                         jefe_grado == "Primaria completa" ~ 11,
                                                                         jefe_grado == "Secundaria incompleta" ~ 12,
                                                                         jefe_grado == "Secundaria completa" ~ 18,
                                                                         jefe_grado == "Preparatoria incompleta" ~ 23,
                                                                         jefe_grado == "Preparatoria completa" ~ 27,
                                                                         jefe_grado == "Licenciatura incompleta" ~ 36,
                                                                         jefe_grado == "Licenciatura completa" ~ 59,
                                                                         jefe_grado == "Diplomado o maestría" ~ 85,
                                                                         jefe_grado == "Diplomado o maestría" ~ 85,
                                                                         jefe_grado == "Doctorado" ~ 85, .default = NA),
                                              #+ Se crean las variables para definir nivel socieconómico segun el estandar AMAI 2022
                                              #+ Revisar: https://amai.org/descargas/Nota_Metodologico_NSE_2022_v5.pdf
                                              amai_cantidadwc = case_when(cantidad_wc == "0" ~ 0,
                                                                          cantidad_wc == "1" ~ 24,
                                                                          cantidad_wc == "2 o más" ~ 47,
                                                                          .default = NA),
                                              amai_cantidadautos = case_when(cantidad_autos == "0" ~ 0,
                                                                             cantidad_autos == "1" ~ 22,
                                                                             cantidad_autos == "2 o más" ~ 43,
                                                                             .default = NA),
                                              amai_internet=case_when(internet == "No tiene" ~ 0,
                                                                      internet == "Sí tiene" ~ 32,
                                                                      .default = NA),
                                              amai_trabajo=case_when(trabajo == "0" ~ 0,
                                                                     trabajo == "1" ~ 15,
                                                                     trabajo == "2" ~ 31,
                                                                     trabajo == "3" ~ 46,
                                                                     trabajo == "4 o más" ~ 61,
                                                                     .default = NA),
                                              amai_cantidadcuartos = case_when(cantidad_cuartos == "0" ~ 0,
                                                                               cantidad_cuartos == "1" ~ 8,
                                                                               cantidad_cuartos == "2" ~ 16,
                                                                               cantidad_cuartos == "3" ~ 24,
                                                                               cantidad_cuartos == "4 o más" ~ 32,
                                                                               .default = NA)) %>%
                                dplyr::mutate(suma_amai = rowSums(select(., contains("amai_")), na.rm = TRUE),
                                              nivel_socioec = case_when(
                                                (suma_amai >= 0 & suma_amai <= 47) ~ "E",
                                                (suma_amai >= 48 & suma_amai <= 94) ~ "D",
                                                (suma_amai >= 95 & suma_amai <= 115) ~ "D_mas",
                                                (suma_amai >= 116 & suma_amai <= 140) ~ "C_menos",
                                                (suma_amai >= 141 & suma_amai <= 167) ~ "C",
                                                (suma_amai >= 168 & suma_amai <= 201) ~ "C_mas",
                                                suma_amai >= 202 ~ "A_B",
                                                .default = NA),
                                              nivel_socioec = factor(x = nivel_socioec,
                                                                     levels = c("E",
                                                                                "D",
                                                                                "D_mas",
                                                                                "C_menos",
                                                                                "C",
                                                                                "C_mas",
                                                                                "A_B")))

                            },

                            coordenadas_faltantes = function(){

                              self$sin_coordenadas <- self$base %>% filter(is.na(Longitude) | is.na(Latitude))
                              self$base <- self$base %>% filter(!is.na(Longitude) | !is.na(Latitude))

                            },

                            eliminar_falta_coordenadas = function(){

                              if("Longitude" %in% names(self$base) & "Latitude" %in% names(self$base)){
                                n <- nrow(self$base)
                                self$base <- self$base %>% filter(!is.na(Longitude) | !is.na(Latitude))
                                self$eliminadas <- self$eliminadas %>% bind_rows(
                                  self$base %>% filter(is.na(Longitude) | is.na(Latitude)) %>% mutate(razon = "Sin coordenadas")
                                )
                                print(
                                  glue::glue("Se eliminaron {n-nrow(self$base)} encuestas por falta de coordenadas")
                                )
                              } else{
                                print("No existe la columna Longitude ni Latitude en la base de respuestas")
                              }
                            },

                            correccion_cluster = function(base, shp, mantener, nivel, var_n){

                              aux <- corregir_cluster(base, shp, mantener, nivel, var_n)

                              self$cluster_corregido <- self$base %>%
                                select(all_of(c("SbjNum", "Srvyr", "Date", "Longitude", "Latitude", var_n))) %>%
                                anti_join(aux, by = c("SbjNum", var_n)) %>%
                                left_join(aux %>% select(all_of(c("SbjNum", var_n))), by = "SbjNum") %>%
                                rename(anterior = 6, nueva = 7)

                              self$base <- aux
                            },

                            eliminar_fuera_muestra = function(respuestas, muestra, nivel, var_n){

                              self$base <- respuestas %>%
                                semi_join(muestra %>% mutate(!!rlang::sym(nivel) := as.character(!!rlang::sym(nivel))),
                                          by = set_names(nivel,var_n))

                              self$eliminadas <- self$eliminadas %>% bind_rows(
                                respuestas %>%
                                  anti_join(muestra %>% mutate(!!rlang::sym(nivel) := as.character(!!rlang::sym(nivel))),
                                            by = set_names(nivel,var_n)) %>%
                                  mutate(razon = "Cluster no existente")
                              )
                              print(glue::glue("Se eliminaron {nrow(respuestas) - nrow(self$base)} entrevistas ya que el cluster no pertenece a la muestra"))
                            },

                            calcular_distancia = function(base, encuesta, muestra, var_n, nivel){

                              aux_sf <- base %>% st_as_sf(coords = c("Longitude", "Latitude"),crs = 4326)

                              pol <- dist_poligonos(aux_sf, shp = encuesta$shp, var_n, nivel)
                              puntos <- dist_puntos(aux_sf, encuesta, muestra, var_n, nivel)

                              if(nrow(puntos) > 0){
                                respuestas <- pol %>%
                                  anti_join(
                                    puntos %>% as_tibble, by = "SbjNum"
                                  ) %>% bind_rows(
                                    puntos
                                  )
                              } else {
                                respuestas <- pol
                              }

                              self$base <- respuestas %>% left_join(base %>% select(SbjNum, Longitude, Latitude), by = "SbjNum")
                            },

                            eliminar_faltantes_diseno = function(){
                              n <- nrow(self$base)

                              self$base <- self$base %>%
                                filter(
                                  across(
                                    .cols = c(starts_with("id_"),
                                              starts_with("fpc_"),
                                              starts_with("prob_"),
                                              starts_with("pob_")),
                                    .fns = ~ !is.na(.x)
                                  ))
                              print(
                                glue::glue("Se eliminaron {n-nrow(self$base)} encuestas por tener datos faltantes en las variables de diseno muestral")
                              )
                              return(self$base)
                            },

                            vars_diseno = function(muestra, var_n, tipo_encuesta){
                              vars_join <- c(var_n,
                                             names(muestra$base)[is.na(match(names(muestra$base), names(self$base)))]
                              )

                              self$base <- self$base %>%
                                inner_join(muestra$base %>% select(all_of(vars_join)))

                              if(tipo_encuesta == "inegi"){
                                self$base <- self$base %>%
                                  mutate(rango_edad = as.character(cut(as.integer(edad),c(17, 24, 59, 200),
                                                                       c("18A24","25A59","60YMAS"))),
                                         sexo = if_else(sexo == "Mujer", "F", "M"))
                              }

                              if(tipo_encuesta == "ine"){
                                self$base <- self$base %>%
                                  mutate(rango_edad = cut(as.numeric(edad), c(17,24,39,59,Inf),
                                                          labels = c("18A24","25A39","40A59","60YMAS")),
                                         sexo = if_else(sexo == "Mujer", "F", "M"))
                              }

                              if(sum(grepl("region", muestra$muestra$niveles$variable)) > 0){
                                var_reg <- muestra$muestra$niveles %>% filter(variable == "region") %>%
                                  unite("var_reg", c(tipo, nivel)) %>% pull(var_reg)
                                self$base <- self$base %>%
                                  inner_join(muestra$muestra$poblacion$marco_muestral %>%
                                               distinct(across(all_of(var_reg)), region), by = var_reg)
                              }
                            }

                          )
)


#'Esta es la clase de muestra
#'@export
Muestra <- R6::R6Class("Muestra",
                       public=list(
                         muestra = NULL,
                         base=NULL,
                         diseno_original = NULL,
                         region = NULL,
                         calibracion = NULL,
                         diseno=NULL,
                         calibraciones = NULL,
                         initialize =function(muestra, respuestas, nivel, var_n){
                           self$muestra <- muestra

                           self$base <- muestra$muestra %>% purrr::pluck(var_n)
                           self$recalcular_fpc(respuestas = respuestas, nivel, var_n)
                         },
                         recalcular_fpc = function(respuestas, nivel, var_n){

                           var_pob <- self$muestra$variable_poblacional

                           pob <- self$base %>%
                             tidyr::unnest(data) %>%
                             count(!!rlang::sym(nivel), wt= !!rlang::sym(var_pob), name="poblacion") %>%
                             mutate(!!rlang::sym(var_n) := as.character(!!rlang::sym(nivel))) %>%
                             select(-!!rlang::sym(nivel))

                           respuesta_fpc <- respuestas %>%
                             count(!!rlang::sym(var_n)) %>%
                             left_join(pob, by= var_n) %>%
                             mutate(fpc_0=n/poblacion) %>%
                             select(!!rlang::sym(var_n), fpc_0)

                           muestra <- self$base %>%
                             mutate(data = map(data,~.x %>% distinct(across(contains("fpc"))))) %>%
                             tidyr::unnest(data) %>%
                             select(-fpc_0) %>%
                             mutate(!!rlang::sym(var_n) := as.character(!!rlang::sym(nivel))) %>%
                             inner_join(respuesta_fpc, by= var_n)

                           self$base <- muestra

                         },
                         extraer_diseno = function(respuestas, marco_muestral, tipo_encuesta, sin_peso, rake){
                           if(sin_peso){
                             self$diseno <- survey::svydesign(
                               ids=~1,
                               data = respuestas
                             )
                           } else{
                             r <- try(
                               survey::svydesign(
                                 pps="brewer",
                                 ids=crear_formula_nombre(respuestas, "cluster_"),
                                 fpc = crear_formula_nombre(respuestas, "fpc_"),
                                 strata = crear_formula_nombre(respuestas, "strata_"),
                                 data = respuestas
                               )
                               ,T)

                             diseno <- if("try-error" %in% class(r)){
                               message("Se intenta muestreo estratificado por estrato. Faltan unidades a muestrear.")
                               out <- survey::svydesign(
                                 pps="brewer",
                                 ids = ~1,
                                 strata = crear_formula_nombre(respuestas, "strata_"),
                                 data = respuestas
                               )
                               out
                             } else{
                               r
                             }

                             if(rake){
                               if(tipo_encuesta == "inegi"){
                                 pob <- marco_muestral %>%
                                   transmute(
                                     P_18A24_F,
                                     P_18A24_M,
                                     P_25A59_F = P_18YMAS_F - P_18A24_F - P_60YMAS_F,
                                     P_25A59_M = P_18YMAS_M - P_18A24_M - P_60YMAS_M,
                                     P_60YMAS_F, P_60YMAS_M) %>%
                                   summarise(across(everything(), ~sum(.x,na.rm = T))) %>%
                                   pivot_longer(everything()) %>% mutate(name = gsub("P_","",name)) %>%
                                   separate(name, into = c("rango_edad", "sexo"))
                               }

                               if(tipo_encuesta == "ine"){
                                 pob <- marco_muestral %>%
                                   select(contains("LN22_")) %>%
                                   summarise(across(everything(), ~sum(.x,na.rm = T))) %>%
                                   pivot_longer(everything()) %>% mutate(name = gsub("LN22_","",name)) %>%
                                   separate(name, into = c("rango_edad", "sexo"))
                               }


                               pobG <- pob %>% count(rango_edad, wt = value, name = "Freq")
                               pobS<- pob %>% count(sexo, wt = value, name = "Freq")
                               self$diseno <- survey::rake(diseno, list(~rango_edad, ~sexo), list(pobG, pobS))
                             } else{
                               self$diseno <- diseno
                             }
                           }

                         },
                         revisar_sexo = function(){
                           self$diseno$variables %>% count(sexo) %>%
                             mutate(pct =n/sum(n), tipo = "encesta") %>% bind_rows(
                               self$muestra$poblacion$marco_muestral %>%
                                 select(contains("LN22_")) %>%
                                 summarise(across(everything(), ~sum(.x,na.rm = T))) %>%
                                 pivot_longer(everything()) %>% mutate(name = gsub("LN22_","",name)) %>%
                                 separate(name, into = c("rango_edad", "sexo")) %>%
                                 count(sexo, wt = value) %>%
                                 mutate(pct = n/sum(n), tipo = "real")
                             ) %>% ggplot(aes(x = tipo, y = pct, color = sexo)) + geom_point() +
                             ggrepel::geom_text_repel(aes(label = paste0(scales::percent(pct,.01))),force_pull = 5) +
                             geom_line(aes(group = sexo)) +
                             scale_y_continuous(labels = scales::percent) +
                             labs(y = NULL,  x = NULL) +
                             theme_minimal()
                         },
                         revisar_rango_edad = function(){
                           self$diseno$variables %>% count(rango_edad) %>%
                             mutate(pct = n/sum(n), tipo = "encuesta") %>% bind_rows(
                               self$muestra$poblacion$marco_muestral %>%
                                 select(contains("LN22_")) %>%
                                 summarise(across(everything(), ~sum(.x,na.rm = T))) %>%
                                 pivot_longer(everything()) %>% mutate(name = gsub("LN22_","",name)) %>%
                                 separate(name, into = c("rango_edad", "sexo")) %>%
                                 count(rango_edad, wt = value )%>%
                                 mutate(pct = n/sum(n), tipo = "real")
                             ) %>% ggplot(aes(x = tipo, y = pct, color = rango_edad)) + geom_point() +
                             ggrepel::geom_text_repel(aes(label = paste0(scales::percent(pct,.01))),force_pull = 5) +
                             geom_line(aes(group = rango_edad)) +
                             scale_y_continuous(labels = scales::percent) +
                             labs(y = NULL,  x = NULL) +
                             theme_minimal()
                         },
                         diseno_region = function(seleccion){
                           self$region <- seleccion

                           if(is.null(self$diseno_original)){
                             self$diseno_original <- self$diseno
                           }

                           self$diseno <- subset(self$diseno_original, region %in% self$region)
                         },
                         regresar_diseno_original = function(){
                           self$region <- NULL
                           self$calibracion <- NULL
                           self$diseno_original -> self$diseno
                         },
                         agregar_calibracion = function(vars, poblacion, nombre){
                           calibracion <- survey::calibrate(self$diseno,
                                                            survey::make.formula(vars),
                                                            population = poblacion)

                           self$calibraciones <- self$calibraciones |>
                             append(
                               list(
                                 list(
                                   diseno = calibracion,
                                   variables = vars
                                 )
                               ) |>
                                 purrr::set_names(nombre))
                         },
                         revisar_calibracion = function(nombre){
                           aux <- self$calibraciones |> purrr::pluck(nombre)
                           survey::svytotal(survey::make.formula(aux |> pluck("variables")),
                                            design = aux |> pluck("diseno"))
                         },
                         comparar_calibraciones = function(variables, valor_variables, vartype){
                           list(original = self$diseno) |>
                             append(self$calibraciones |> purrr::map(~.x$diseno)) |>
                             comparar_disenos(variables, valor_variables, vartype)
                         },
                         elegir_calibracion = function(nombre){

                           self$calibracion <- nombre

                           if(is.null(self$diseno_original)){
                             self$diseno_original <- self$diseno
                           }

                           self$diseno <- self$calibraciones |> purrr:::pluck(nombre, "diseno")
                         }
                       ))

#'Esta es la clase cuestionario
#'@export
#'
Cuestionario <- R6::R6Class("Cuestionario",
                            public = list(
                              documento = NULL,
                              aprobado = NULL,
                              diccionario = NULL,
                              initialize = function(documento, patron){
                                if("data.frame" %in% class(documento)){
                                  llaves_repetidas = comprobar_unicidadLlaves(diccionario = documento)
                                  if(0 < llaves_repetidas) {
                                    stop(glue::glue("Hay ", llaves_repetidas, " llaves repetidas en el diccionario"))
                                  }
                                  self$diccionario <- documento
                                } else{
                                  self$documento <- documento %>% officer::docx_summary() %>% as_tibble
                                  self$diccionario <- private$crear_diccionario(patron)
                                }
                              },
                              aprobar = function(){
                                self$aprobado <- T
                                return(invisible(self))
                              },
                              checar_pregunta = function(pregunta){
                                pregunta_chr <- rlang::expr_text(ensym(pregunta))
                                bd <- self$diccionario %>%
                                  filter(llaves==pregunta_chr)
                                pertenece <- (nrow(bd)==1)
                                if(pertenece){
                                  return(map(bd,~.x))
                                }
                                else return(NULL)
                              },
                              resumen = function(){

                                res <- resumen_cuestionario(self$diccionario)

                                return(res)

                              }
                            )
                            ,
                            private = list(
                              crear_diccionario = function(patron){
                                diccionario <- diccionario_cuestionario(self$documento, patron)
                                return(diccionario)
                              })
)

#'Esta es la clase de Resultados
#'@export
#'
Resultados <- R6::R6Class(classname = "Resultados",
                          public = list(
                            encuesta = NULL,
                            diseno = NULL,
                            diccionario = NULL,
                            Descriptiva = NULL,
                            Regiones = NULL,
                            Modelo = NULL,
                            Cruce = NULL,
                            Especial = NULL,
                            Tendencias = NULL,
                            tema = NULL,
                            graficadas = NULL,
                            initialize = function(encuesta, diseno, diccionario, tema = tema_morant()){

                              self$encuesta <- encuesta

                              self$diseno <- diseno
                              if(!is.null(self$encuesta)){

                                self$diccionario <- self$encuesta$cuestionario$diccionario

                              } else {

                                self$diccionario <- diccionario

                              }

                              self$tema <- tema

                              self$Descriptiva <- Descriptiva$new(encuesta = self$encuesta,
                                                                  diseno = self$diseno,
                                                                  diccionario = self$diccionario,
                                                                  tema = self$tema,
                                                                  graficadas = self$graficadas)

                              self$Cruce <- Cruce$new(encuesta = self$encuesta,
                                                      diseno = self$diseno,
                                                      diccionario = self$diccionario,
                                                      tema = self$tema,
                                                      graficadas = self$graficadas)

                              self$Especial <- Especial$new(encuesta = self$encuesta,
                                                            diseno = self$diseno,
                                                            diccionario = self$diccionario,
                                                            tema = self$tema,
                                                            graficadas = self$graficadas)

                              self$Tendencias <- Tendencias$new(encuesta = self$encuesta)

                              if(!is.null(self$encuesta)) {

                                self$Regiones <- Regiones$new(encuesta = self$encuesta,
                                                              diccionario = self$encuesta$cuestionario$diccionario,
                                                              tema = self$tema)

                                self$Modelo <- Modelo$new(diseno = self$encuesta$muestra$diseno,
                                                          diccionario = self$encuesta$cuestionario$diccionario,
                                                          tema = self$tema,
                                                          graficadas = self$graficadas)

                              } else {

                                self$Regiones <- NULL
                                self$Modelo <- Modelo$new(diseno = self$diseno,
                                                          diccionario = self$diccionario,
                                                          tema = self$tema,
                                                          graficadas = self$graficadas)

                              }
                            },
                            faltantes = function(){
                              gant_p_r(self$encuesta$cuestionario$diccionario %>% filter(!llaves %in% self$graficadas))
                            }
                          )
)

#'Esta es la clase de Descriptiva
#'@export
#'
Descriptiva <- R6::R6Class(classname = "Descriptiva",
                           public = list(
                             encuesta = NULL,
                             diseno = NULL,
                             diccionario = NULL,
                             tema = NULL,
                             graficadas = NULL,
                             initialize = function(encuesta = NULL, diseno = NULL, diccionario = NULL, tema, graficadas = NULL){
                               self$encuesta <- encuesta
                               self$diseno <- diseno
                               self$diccionario <- diccionario
                               self$tema <- tema
                               self$graficadas <- graficadas
                             },
                             barras_categorica = function(codigo, salto = 20, porcentajes_fuera = F, desplazar_porcentajes = 0, pct_otros = 0.01, orden_respuestas = NA){

                               llave_aux <- codigo
                               if(!(llave_aux %in% self$graficadas)){
                                 if(llave_aux %in% self$diccionario$llaves){
                                   self$graficadas <- self$graficadas %>% append(llave_aux)
                                 } else {
                                   stop(glue::glue("La llave {llave_aux} no existe en el diccionario"))
                                 }
                               } else {
                                 warning(glue::glue("La llave {llave_aux} ya fue graficada con anterioridad"))
                               }

                               tema <- self$diccionario |>
                                 filter(llaves == rlang::ensym(codigo)) |>
                                 pull(tema)

                               if(is.null(self$diseno)) {

                                 diseno <- self$encuesta$muestra$diseno

                               } else {

                                 diseno <- self$diseno

                               }

                               analizar_frecuencias(diseno = diseno, pregunta = {{codigo}}) |>
                                 mutate(tema = tema) |>
                                 mutate(respuesta = forcats::fct_lump_min(f = respuesta, min = pct_otros, w = media, other_level = "Otros"),
                                        respuesta = dplyr::if_else(condition = respuesta == "Otro", true = "Otros", false = respuesta)) %>%
                                 group_by(respuesta) |>
                                 summarise(media = sum(media)) |>
                                 graficar_barras(salto = salto,
                                                 porcentajes_fuera = porcentajes_fuera,
                                                 desplazar_porcentajes = desplazar_porcentajes,
                                                 orden_respuestas = orden_respuestas) +
                                 self$tema

                             },
                             barras_aspectos = function(patron_inicial, aspectos = NULL, salto = 20, filtro = "respuesta == 'Sí'", porcentajes_fuera = F, desplazar_porcentajes = 0){

                               if(is.null(filtro) | is.null(aspectos)) {

                                 stop(paste("Especifique la respuesta en la cual hacer filtro con el argumento `filtro` o indique los aspectos correctos."))

                               } else {

                                 tema <- self$diccionario |>
                                   filter(llaves == rlang::ensym(patron_inicial)) |>
                                   pull(pregunta)

                                 if(is.null(self$diseno)) {

                                   diseno <- self$encuesta$muestra$diseno

                                 } else {

                                   diseno <- self$diseno

                                 }

                                 analizar_frecuencias_aspectos(diseno = diseno,
                                                               diccionario = self$diccionario,
                                                               patron_pregunta = {{patron_inicial}},
                                                               aspectos = aspectos) |>
                                   left_join(self$diccionario |>
                                               select(llaves, tema), by = c("aspecto" = "llaves")) |>
                                   filter(eval(rlang::parse_expr(filtro))) |>
                                   transmute(respuesta = tema, media) |>
                                   graficar_barras(salto = salto,
                                                   porcentajes_fuera = porcentajes_fuera,
                                                   desplazar_porcentajes = desplazar_porcentajes,
                                                   orden_respuestas = NA) +
                                   self$tema

                               }

                             },
                             barras_multirespuesta = function(patron_inicial, salto = 20, porcentajes_fuera = F, desplazar_porcentajes = 0){

                               if(is.null(self$diseno)) {

                                 diseno <- self$encuesta$muestra$diseno

                               } else {

                                 diseno <- self$diseno

                               }

                               analizar_frecuencias_multirespuesta(diseno = diseno,
                                                                   patron_inicial) %>%
                                 graficar_barras(salto = salto,
                                                 porcentajes_fuera = porcentajes_fuera,
                                                 desplazar_porcentajes = desplazar_porcentajes) +
                                 self$tema

                             },
                             gauge_numerica = function(codigo, color = "#850D2D", escala = c(0, 10), size_text_pct = 14){

                               llave_aux <- codigo
                               if(!(llave_aux %in% self$graficadas)){
                                 if(llave_aux %in% self$diccionario$llaves){
                                   self$graficadas <- self$graficadas %>% append(llave_aux)
                                 } else {
                                   stop(glue::glue("La llave {llave_aux} no existe en el diccionario"))
                                 }
                               } else {
                                 warning(glue::glue("La llave {llave_aux} ya fue graficada con anterioridad"))
                               }

                               if(is.null(self$diseno)) {

                                 diseno <- self$encuesta$muestra$diseno

                               } else {

                                 diseno <- self$diseno

                               }

                               bd_estimacion <- analizar_frecuencias(diseno = diseno, pregunta = {{codigo}})

                               tema <- self$diccionario |>
                                 filter(llaves == rlang::ensym(codigo)) |>
                                 pull(tema)

                               bd_estimacion |>
                                 mutate(tema = tema) |>
                                 graficar_gauge(color_principal = color,
                                                escala = escala,
                                                size_text_pct = size_text_pct)

                             },
                             gauge_categorica = function(codigo, filtro, color = "#850D2D", escala = c(0, 1), size_text_pct = 14){

                               if(is.null(filtro)) {

                                 stop(paste("Especifique la respuesta en la cual hacer filtro con el argumento `filtro`"))

                               }

                               else {

                                 llave_aux <- codigo
                                 if(!(llave_aux %in% self$graficadas)){
                                   if(llave_aux %in% self$diccionario$llaves){
                                     self$graficadas <- self$graficadas %>% append(llave_aux)
                                   } else {
                                     stop(glue::glue("La llave {llave_aux} no existe en el diccionario"))
                                   }
                                 } else {
                                   warning(glue::glue("La llave {llave_aux} ya fue graficada con anterioridad"))
                                 }

                                 if(is.null(self$diseno)) {

                                   diseno <- self$encuesta$muestra$diseno

                                 } else {

                                   diseno <- self$diseno

                                 }

                                 bd_estimacion <- analizar_frecuencias(diseno = diseno, pregunta = {{codigo}}) |>
                                   filter(eval(rlang::parse_expr(filtro)))

                                 tema <- self$diccionario |>
                                   filter(llaves == rlang::ensym(codigo)) |>
                                   pull(tema)

                                 bd_estimacion %>%
                                   mutate(tema = tema) |>
                                   graficar_gauge(color_principal = color,
                                                  escala = escala,
                                                  size_text_pct = size_text_pct)
                               }

                             },
                             intervalo_numerica = function(patron, aspectos, escala = c(0, 10), point_size = 1, text_point_size = 8){

                               # llave_aux <- codigo
                               # if(!(llave_aux %in% self$graficadas)){
                               #   if(llave_aux %in% self$diccionario$llaves){
                               #     self$graficadas <- self$graficadas %>% append(llave_aux)
                               #   } else {
                               #     stop(glue::glue("La llave {llave_aux} no existe en el diccionario"))
                               #   }
                               # } else {
                               #   warning(glue::glue("La llave {llave_aux} ya fue graficada con anterioridad"))
                               # }

                               if(is.null(self$diseno)) {

                                 diseno <- self$encuesta$muestra$diseno

                               } else {

                                 diseno <- self$diseno

                               }

                               analizar_frecuencias_aspectos(diseno = diseno,
                                                             diccionario = self$diccionario,
                                                             patron_pregunta = {{patron}},
                                                             aspectos = aspectos) %>%
                                 left_join(self$diccionario %>%
                                             select(aspecto = llaves, tema), by = "aspecto") |>
                                 graficar_intervalo_numerica(escala = escala, point_size = point_size, text_point_size = text_point_size) +
                                 self$tema


                             },
                             lollipops_categorica = function(codigo, orden = NULL, limits = c(0, 1.0), width_cats = 15 , size=3, size_pct = 6,pct_otros = 0.01){
                               llave_aux <- codigo
                               if(!(llave_aux %in% self$graficadas)){
                                 if(llave_aux %in% self$diccionario$llaves){
                                   self$graficadas <- self$graficadas %>% append(llave_aux)
                                 } else {
                                   stop(glue::glue("La llave {llave_aux} no existe en el diccionario"))
                                 }
                               } else {
                                 warning(glue::glue("La llave {llave_aux} ya fue graficada con anterioridad"))
                               }

                               tema <- self$diccionario |>
                                 filter(llaves == rlang::ensym(codigo)) |>
                                 pull(tema)

                               if(is.null(self$diseno)) {

                                 diseno <- self$encuesta$muestra$diseno

                               } else {

                                 diseno <- self$diseno

                               }

                               analizar_frecuencias(diseno = diseno, pregunta = {{codigo}}) |>
                                 mutate(tema = tema) |>
                                 rename(pct = media) |>
                                 mutate(respuesta = forcats::fct_lump_min(f = respuesta, min = pct_otros, w = pct, other_level = "Otros"),
                                        respuesta = dplyr::if_else(condition = respuesta == "Otro", true = "Otros", false = respuesta)) %>%
                                 group_by(respuesta) |>
                                 summarise(pct = sum(pct)) |>
                                 graficar_lollipops(orden = orden,
                                                    limits = limits,
                                                    width_cats = width_cats ,
                                                    size = size,
                                                    size_pct = size_pct) +
                                 self$tema
                             },
                             lollipops_multirespuesta = function(patron_inicial, orden = NULL, limits = c(0, 1.0), width_cats = 15 , size=3, size_pct = 6){

                               if(is.null(self$diseno)) {

                                 diseno <- self$encuesta$muestra$diseno

                               } else {

                                 diseno <- self$diseno

                               }

                               analizar_frecuencias_multirespuesta(diseno = diseno,
                                                                   patron_inicial) %>%
                                 rename(pct = media) |>
                                 graficar_lollipops(orden = orden,
                                                    limits = limits,
                                                    width_cats = width_cats ,
                                                    size = size,
                                                    size_pct = size_pct)  +
                                 self$tema

                             }
                           ))

#'Esta es la clase de Cruce
#'@export
#'
Cruce <- R6::R6Class(classname = "Cruce",
                     public = list(
                       encuesta = NULL,
                       diseno = NULL,
                       diccionario = NULL,
                       tema = NULL,
                       graficadas = NULL,
                       initialize = function(encuesta = NULL, diseno = NULL, diccionario = NULL, tema, graficadas = NULL){
                         self$encuesta <- encuesta
                         self$diseno <- diseno
                         self$diccionario <- diccionario
                         self$tema <- tema
                         self$graficadas <- graficadas
                       },
                       # puntos = function(cruce, variables, vartype = "se", valor_variables){
                       #
                       #   if(is.null(self$diseno)) {
                       #
                       #     diseno <- self$encuesta$muestra$diseno
                       #
                       #   } else {
                       #
                       #     diseno <- self$diseno
                       #
                       #   }
                       #
                       #   cruce_aspectos(diseno = diseno,
                       #                              cruce = cruce,
                       #                              variables = variables,
                       #                              vartype = vartype,
                       #                              valor_variables = valor_variables) |>
                       #     left_join(self$diccionario |>
                       #                 distinct(llaves, tema), by = c("variable" = "llaves")) |>
                       #     select(!variable) |>
                       #     rename(variable = tema) |>
                       #     graficar_crucePuntos(cruce = cruce, vartype = vartype) +
                       #     self$tema
                       #
                       # },
                       # brechasDuales = function(var1, var2_filtro, filtro, vartype = "cv", line_rich = FALSE, line_linewidth = 2, line_hjust = "ymax", line_vjust = -0.3){
                       #
                       #   if(is.null(self$diseno)) {
                       #
                       #     diseno <- self$encuesta$muestra$diseno
                       #
                       #   } else {
                       #
                       #     diseno <- self$diseno
                       #
                       #   }
                       #
                       #   analizar_cruceBrechas(diseno = srvyr::as_survey_design(diseno),
                       #                                     var1 = var1,
                       #                                     var2_filtro = var2_filtro,
                       #                                     filtro = filtro,
                       #                                     vartype = vartype) |>
                       #     graficar_cruce_brechasDuales(var1 = var1,var2_filtro = var2_filtro, vartype = vartype,
                       #                                              line_rich = line_rich,
                       #                                              line_linewidth = line_linewidth,
                       #                                              line_hjust = line_hjust,
                       #                                              line_vjust = line_vjust) +
                       #     self$tema
                       # },
                       # brechasMultiples = function(por_grupo, variables, vartype = "cv", valor_variables, line_rich = FALSE, line_linewidth = 2, line_hjust = "ymax", line_vjust = -0.3){
                       #
                       #   if(is.null(self$diseno)) {
                       #
                       #     diseno <- self$encuesta$muestra$diseno
                       #
                       #   } else {
                       #
                       #     diseno <- self$diseno
                       #
                       #   }
                       #
                       #   analizar_crucePuntos(srvyr::as_survey_design(diseno),
                       #                                    cruce = por_grupo,
                       #                                    variables = variables, vartype = vartype, valor_variables = valor_variables) %>%
                       #     {
                       #       if(vartype == "cv"){
                       #         mutate(., pres=case_when(`cv` >.15 & `cv` <.30 ~ "*",
                       #                                  `cv` >.30 ~ "**",
                       #                                  TRUE ~""))
                       #       } else {
                       #         .
                       #       }
                       #     } |>
                       #     left_join(self$diccionario,
                       #               join_by(variable == llaves)) |> select(-variable) |>
                       #     rename(variable = tema) %>%
                       #     graficar_cruce_brechasMultiples(cruce = por_grupo, vartype = vartype,
                       #                                                 line_rich = line_rich,
                       #                                                 line_linewidth = line_linewidth,
                       #                                                 line_hjust = line_hjust,
                       #                                                 line_vjust = line_vjust) +
                       #     self$tema
                       #
                       # },
                       bloques = function(variable_principal, variable_secundaria, colores_variable_secundaria,
                                          filter = NULL,
                                          vartype = "cv", linea_grosor = 2, linea_color = "white",na_rm = TRUE){

                         if(is.null(self$diseno)) {

                           diseno <- self$encuesta$muestra$diseno

                         } else {

                           diseno <- self$diseno

                         }

                         analizar_cruce(diseno = diseno,
                                        variable_principal = variable_principal,
                                        variable_secundaria = variable_secundaria,
                                        vartype = vartype,
                                        na_rm = na_rm) |>
                           graficar_cruce_bloques(cruce = variable_principal,
                                                  variable = variable_secundaria,
                                                  colores_variable_secundaria = colores_variable_secundaria,
                                                  vartype = vartype,
                                                  filter = filter,
                                                  linea_grosor = linea_grosor,
                                                  linea_color = linea_color) +
                           self$tema
                       },
                       lineas = function(variable_principal,
                                         variable_secundaria,
                                         orden_variable_principal,
                                         valores_variable_secundaria = NULL,
                                         colores_variable_secundaria,
                                         limits =  c(0, 0.75),
                                         wrap_x = 25,
                                         wrap_legend = 25,
                                         text_nudge_y = 0.01,
                                         size_text = 8,
                                         size_text_x = 16,
                                         size_text_y = 14,
                                         size_text_legend = 14,
                                         na_rm = TRUE){

                         if(is.null(self$diseno)) {

                           diseno <- self$encuesta$muestra$diseno

                         } else {

                           diseno <- self$diseno

                         }

                         analizar_cruce(diseno = diseno,
                                        variable_principal = variable_principal,
                                        variable_secundaria = variable_secundaria,
                                        vartype = "cv",
                                        na_rm = na_rm) %>%
                           {
                             if(!is.null(valores_variable_secundaria)) {
                               filter(., !!rlang::sym(variable_secundaria) %in% valores_variable_secundaria)
                             } else {
                               .
                             }
                           } %>%
                           rename(var_x = !!rlang::sym(variable_principal),
                                  var_y = !!rlang::sym(variable_secundaria),
                                  media = coef) |>
                           graficar_lineas(orden_var_x = orden_variable_principal,
                                           colores = colores_variable_secundaria,
                                           salto_x = wrap_x,
                                           salto_legend = wrap_legend,
                                           limits = limits,
                                           text_nudge_y = text_nudge_y,
                                           size_text = size_text) +
                           self$tema +
                           theme(legend.position = "bottom",
                                 axis.text.x = element_text(size = size_text_x),
                                 axis.text.y = element_text(size = size_text_y),
                                 legend.text = element_text(size = size_text_legend))
                       },
                       lolipop_diferencias = function(variable_principal,
                                                      variables_secundarias,
                                                      filtro_variables_secundarias,
                                                      orden_variablePrincipal,
                                                      colores_variables_secundarias,
                                                      caption,
                                                      nudge_x = 0.05,
                                                      size_geom_text = 6,
                                                      invertir_variables = F,
                                                      vartype = "cv",
                                                      limits = c(0, 0.75),
                                                      wrap_y = 25,
                                                      wrap_caption = 25,
                                                      size_text_x = 16,
                                                      size_text_y = 16,
                                                      size_text_caption = 16,
                                                      size_text_legend = 16,
                                                      ver_diferencias = TRUE) {
                         if(is.null(self$diseno)) {

                           diseno <- self$encuesta$muestra$diseno

                         } else {

                           diseno <- self$diseno

                         }

                         bd_estimacion <-
                           analizar_cruce_aspectos(diseno = diseno,
                                                   variable_principal = variable_principal,
                                                   variables_secundarias = variables_secundarias,
                                                   filtro_variables_secundarias = filtro_variables_secundarias,
                                                   vartype = vartype) |>
                           left_join(self$diccionario |>
                                       distinct(llaves, tema), by = c("variable" = "llaves")) |>
                           select(!variable) |>
                           rename("variable_principal" := variable_principal)

                         bd_estimacion <-
                           if(invertir_variables) {
                             bd_estimacion |>
                               transmute(aux = variable_principal,
                                         variable_principal = tema,
                                         tema = aux,
                                         mean,
                                         cv) |>
                               select(!aux)
                           } else {
                             bd_estimacion
                           }

                         bd_estimacion <-
                           if(ver_diferencias) {
                             bd_estimacion |>
                               group_by(variable_principal)|>
                               mutate(mean_diff_pos = min(mean) + (max(mean)-min(mean))/2,
                                      mean_dif = (max(mean)-min(mean)))|>
                               ungroup()
                           } else {
                             bd_estimacion
                           }

                         bd_estimacion |>
                           graficar_lolipop_diferencias(orden_variablePrincipal = orden_variablePrincipal,
                                                        colores_variables_secundarias = colores_variables_secundarias,
                                                        nudge_x = nudge_x,
                                                        size_geom_text = size_geom_text,
                                                        caption = caption,
                                                        wrap_y = wrap_y,
                                                        wrap_caption = wrap_caption,
                                                        limits = limits) +
                           {if(ver_diferencias)  geom_text(aes(label = scales::percent(x = mean_dif , accuracy = 1.),y = mean_diff_pos, colour = 'gray' ),
                                                           nudge_x = -nudge_x, size = size_geom_text, show.legend = F) }+
                           self$tema +
                           theme(legend.position = "bottom",
                                 axis.text.x = element_text(size = size_text_x),
                                 axis.text.y = element_text(size = size_text_y),
                                 legend.text = element_text(size = size_text_legend),
                                 plot.caption = element_text(size = size_text_caption))

                       },
                       sankey_categorica = function(variables = NULL, colores, size_text_cat = 6, width_text = 15,omitir_valores_variable1 = NULL, omitir_valores_variable2 = NULL){

                         if(is.null(variables)) {

                           stop(paste("Especifique las variables a analizar con el argumento `variables`"))

                         } else {

                           if(is.null(self$diseno)) {

                             diseno <- self$encuesta$muestra$diseno

                           } else {

                             diseno <- self$diseno

                           }

                           bd_estimacion <- analizar_sankey(diseno = diseno,
                                                            variables = variables,
                                                            filtro_var1 = omitir_valores_variable1,
                                                            filtro_var2 = omitir_valores_variable2)

                           graficar_sankey(bd = bd_estimacion,
                                           variables = variables,
                                           colores = colores,
                                           size_text_cat = size_text_cat,
                                           width_text = width_text)
                         }
                       },
                       tabla_votoCruzado = function(var1,
                                                    var2,
                                                    filtro_var2 = NULL,
                                                    etiquetas = c("variable 1", "variable 2"),
                                                    colores_var1,
                                                    colores_var2,
                                                    size_text_header = 18,
                                                    size_text_body = 14,
                                                    salto = 20,
                                                    na_rm = TRUE){

                         if(is.null(self$diseno)) {

                           diseno <- self$encuesta$muestra$diseno

                         } else {

                           diseno <- self$diseno

                         }

                         # if(is.null(filtro_var2)) {
                         #   filtro_var2 <-
                         #     diseno$variables |>
                         #     as_tibble() |>
                         #     distinct(!!rlang::sym(var2)) |>
                         #     pull()
                         # }

                         bd_votoCruzado <-
                           calcular_tabla_votoCruzado(diseno = diseno,
                                                      var1 = var1,
                                                      var2 = var2,
                                                      filtro_var2 = filtro_var2,
                                                      na_rm = na_rm)

                         tabla_salida <-
                           formatear_tabla_votoCruzado(tabla_votoCruzado = bd_votoCruzado,
                                                       var1 = var1,
                                                       var2 = var2,
                                                       filtro_var2 = filtro_var2,
                                                       etiquetas = etiquetas,
                                                       colores_var1 = colores_var1,
                                                       colores_var2 = colores_var2,
                                                       size_text_header = size_text_header,
                                                       size_text_body = size_text_body,
                                                       salto = salto)

                         return(tabla_salida)
                       }
                     ))

#'Esta es la clase de Especial
#'@export
#'
Especial <- R6::R6Class(classname = "Especial",
                        public = list(
                          encuesta = NULL,
                          diseno = NULL,
                          diccionario = NULL,
                          tema = NULL,
                          graficadas = NULL,
                          initialize = function(encuesta = NULL, diseno = NULL, diccionario = NULL, tema, graficadas = NULL){
                            self$encuesta <- encuesta
                            self$diseno <- diseno
                            self$diccionario <- diccionario
                            self$tema <- tema
                            self$graficadas <- graficadas
                          },
                          candidatoOpinion = function(patron_inicial,
                                                      aspectos,
                                                      ns_nc = "Ns/Nc",
                                                      regular = "Regular",
                                                      llave_burbuja = NA,
                                                      filtro_burbuja = "respuesta == 'Sí'",
                                                      size_burbuja = 8,
                                                      grupo_positivo,
                                                      grupo_negativo,
                                                      orden_resp,
                                                      colores_opinion = c("red", "yellow", "green"),
                                                      color_nsnc = "gray70",
                                                      color_burbuja = "blue",
                                                      caption_opinion = "",
                                                      caption_nsnc = "Ns/Nc",
                                                      caption_burbuja = "Conocimiento",
                                                      size_caption_opinion = 12,
                                                      size_caption_nsnc = 14,
                                                      size_caption_burbuja = 14,
                                                      size_text_cat = 16,
                                                      size_pct = 12,
                                                      burbuja = T,
                                                      salto = 20,
                                                      salto_respuestas = 100,
                                                      mostrar_nsnc = T,
                                                      orden_cat = NULL){

                            if(is.null(self$diseno)) {

                              diseno <- self$encuesta$muestra$diseno

                            } else {

                              diseno <- self$diseno

                            }

                            if(!is.na(llave_burbuja)){

                              bd_burbuja <- analizar_frecuencias_aspectos(diseno = diseno,
                                                                          diccionario = self$diccionario,
                                                                          patron_pregunta = llave_burbuja,
                                                                          aspectos = aspectos) %>%
                                filter(eval(rlang::parse_expr(filtro_burbuja))) %>%
                                left_join(self$diccionario %>% select(aspecto = llaves, tema))
                            } else {
                              bd_burbuja <- NA
                            }

                            analizar_frecuencias_aspectos(diseno = diseno,
                                                          diccionario = self$diccionario,
                                                          patron_pregunta = patron_inicial,
                                                          aspectos = aspectos) |>
                              left_join(self$diccionario %>%
                                          select(aspecto = llaves, tema)) %>%
                              graficar_candidato_opinion(ns_nc = ns_nc,
                                                         regular = regular,
                                                         grupo_positivo= grupo_positivo,
                                                         grupo_negativo = grupo_negativo,
                                                         caption_opinion = caption_opinion,
                                                         caption_nsnc = caption_nsnc,
                                                         caption_burbuja = caption_burbuja,
                                                         size_caption_opinion = size_caption_opinion,
                                                         size_caption_nsnc = size_caption_nsnc,
                                                         size_caption_burbuja = size_caption_burbuja,
                                                         size_text_cat = size_text_cat,
                                                         size_pct = size_pct,
                                                         orden_resp = orden_resp,
                                                         colores = colores_opinion,
                                                         color_nsnc = color_nsnc,
                                                         burbuja = bd_burbuja,
                                                         color_burbuja = color_burbuja,
                                                         size_burbuja = size_burbuja,
                                                         salto = salto,
                                                         tema = self$tema,
                                                         mostrar_nsnc = mostrar_nsnc,
                                                         salto_respuestas = salto_respuestas,
                                                         orden_cat = orden_cat,
                                                         patron_inicial = patron_inicial)

                          },
                          candidatoOpinion2 = function(patron_opinion,
                                                       aspectos,
                                                       ns_nc = "Ns/Nc",
                                                       regular = "Regular",
                                                       patron_conocimiento,
                                                       filtro_conocimiento = "respuesta == 'Sí'",
                                                       orden_opinion,
                                                       etiquetas = c("Candidato", "Opinión"),
                                                       colores_opinion = c("red", "yellow", "green"),
                                                       colores_candidato = colores_candidato,
                                                       color_principal = "#BF498A",
                                                       color_conocimiento = "blue",
                                                       size_text_header = 18,
                                                       size_text_body = 14,
                                                       salto = 20,
                                                       salto_respuestas = 100){

                            if(is.null(self$diseno)) {

                              diseno <- self$encuesta$muestra$diseno

                            } else {

                              diseno <- self$diseno

                            }

                            calcular_tabla_candidatoOpinion(diseno = diseno,
                                                            diccionario = self$diccionario,
                                                            patron_opinion = patron_opinion,
                                                            patron_conocimiento = patron_conocimiento,
                                                            aspectos = aspectos,
                                                            filtro_conocimiento = filtro_conocimiento,
                                                            orden_opinion = orden_opinion,
                                                            ns_nc = ns_nc,
                                                            salto_respuestas = salto_respuestas) %>%
                              formatear_tabla_candidatoOpinion(orden_opinion = orden_opinion,
                                                               etiquetas = etiquetas,
                                                               colores_opinion = colores_opinion,
                                                               color_principal = color_principal,
                                                               colores_candidato = colores_candidato,
                                                               size_text_header = size_text_header,
                                                               size_text_body = size_text_body,
                                                               salto = salto,
                                                               color_conocimiento = color_conocimiento)

                          },
                          candidatoPartido = function(llave_partido, llave_conocimiento, respuesta_conoce, candidatos, corte_otro, cliente, colores_candidatos, colores_partido,corte_vis = 0.0){

                            if(is.null(self$diseno)) {

                              diseno <- self$encuesta$muestra$diseno

                            } else {

                              diseno <- self$diseno

                            }

                            analizar_candidatoPartido(diseno = diseno,
                                                      diccionario = self$diccionario,
                                                      llave_partido = llave_partido,
                                                      llave_conocimiento = llave_conocimiento,
                                                      respuesta_conoce = respuesta_conoce,
                                                      candidatos = candidatos,
                                                      corte_otro = corte_otro) %>%
                              purrr::map(~.x %>%
                                           left_join(self$diccionario %>% select(aspecto = llaves, tema))
                              ) %>%
                              graficar_candidatoPartido(cliente = cliente,
                                                        tipo_conoce = "intervalos",
                                                        colores_candidato = colores_candidatos,
                                                        colores_partido = colores_partido,
                                                        solo_respondidos = T,
                                                        tema = self$tema,
                                                        corte_vis = corte_vis)
                          },
                          candidatoSaldo = function(llave_opinion,
                                                    candidatos,
                                                    positivos,
                                                    negativos,
                                                    regular = "Regular",
                                                    ns_nc = "Ns/Nc",
                                                    color_positivo = "green",
                                                    color_negativo = "red",
                                                    orden_cat = NULL,
                                                    salto_cat = 25,
                                                    caption_opinion,
                                                    size_text_cat = 14,
                                                    size_text_legend = 10,
                                                    size_pct = 12,
                                                    size_caption_opinion = 10){

                            if(is.null(self$diseno)) {

                              diseno <- self$encuesta$muestra$diseno

                            } else {

                              diseno <- self$diseno

                            }

                            bd_saldo <- analizar_saldoOpinion(diseno = diseno,
                                                              diccionario = self$diccionario,
                                                              llave_opinion = llave_opinion,
                                                              candidatos = candidatos,
                                                              grupo_positivo = positivos,
                                                              grupo_negativo = negativos)

                            bd_saldo_aux <-
                              analizar_frecuencias_aspectos(diseno = diseno,
                                                            diccionario = self$diccionario,
                                                            patron_pregunta = llave_opinion,
                                                            aspectos = candidatos) |>
                              left_join(self$diccionario %>%
                                          select(aspecto = llaves, tema), by = "aspecto")

                            if(!is.null(ns_nc)){
                              bd <- bd_saldo_aux %>% group_by(tema) %>% complete(respuesta = ns_nc, fill = list(media = 0)) %>% ungroup
                            }

                            grupo_negativo = negativos
                            grupo_positivo = positivos

                            ejemplo <- bd %>%
                              filter(respuesta != regular) |>
                              mutate(respuesta = case_when(respuesta %in% positivos ~ "Positiva",
                                                           respuesta %in% negativos ~ "Negativa",
                                                           .default = respuesta)) |>
                              group_by(tema, respuesta) |>
                              summarise(media = sum(media, na.rm = TRUE),
                                        .groups = "drop") |>
                              mutate(media = dplyr::if_else(condition = respuesta == "Negativa",
                                                            true = -media,
                                                            false = media)) |>
                              mutate(etiqueta = scales::percent(abs(media), 1),
                                     media = if_else(respuesta %in% grupo_negativo, -1*media, media),
                                     media = if_else(respuesta == regular, media/2, media)) %>%
                              group_by(tema) %>%
                              mutate(saldo = sum(as.numeric(!(respuesta %in% c(regular, ns_nc)))*media))

                            orden <- ejemplo %>% arrange(saldo) %>% pull(tema) %>% unique %>% na.omit

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

                            ejemplo %>%
                              {if(!is.null(ns_nc)) filter(., respuesta!= ns_nc) else .}  %>%
                              mutate(respuesta = factor(respuesta, levels = c("Negativa", "Positiva"))) |>
                              graficar_barras_saldo(orden = orden,
                                                    grupo_positivo = "Positiva",
                                                    grupo_negativo = "Negativa",
                                                    Regular = NA_character_,
                                                    colores = c("Positiva" = color_positivo,
                                                                "Negativa" = color_negativo),
                                                    salto_respuestas = 50,
                                                    salto_tema = salto_cat,
                                                    caption_opinion = caption_opinion,
                                                    size_text_cat = size_text_cat,
                                                    size_pct = size_pct,
                                                    size_caption_opinion = size_caption_opinion,
                                                    size_text_legend = size_text_legend)

                          },
                          metodo_morena = function(personajes, atributos){
                            if(is.null(self$diseno)) {

                              diseno <- self$encuesta$muestra$diseno

                            } else {

                              diseno <- self$diseno

                            }
                            analizar_morena(diseno = diseno, diccionario = self$diccionario, personajes = personajes, atributos = atributos) %>%
                              graficar_morena(personajes = personajes, atributos = atributos)
                          }
                        ))

#'Esta es la clase de Regiones
#'@export
#'
Regiones <- R6::R6Class(classname = "Regiones",
                        public = list(
                          encuesta = NULL,
                          diseno = NULL,
                          diccionario = NULL,
                          tema = NULL,
                          shp_regiones = NULL,
                          initialize = function(encuesta = NULL, diseno = NULL, diccionario = NULL, tema){
                            self$encuesta <- encuesta
                            self$diseno <- diseno
                            self$diccionario <- diccionario
                            self$tema <- tema
                            self$crear_shp_regiones()
                          },
                          mapa_ganador = function(variable,region = 'region', lugar = 1, na_rm = T){

                            if(is.null(self$diseno)) {

                              diseno <- self$encuesta$muestra$diseno

                            } else {

                              diseno <- self$diseno

                            }

                            self$shp_regiones|>
                              left_join(
                                analizar_cruce(diseno = diseno,
                                                           variable_principal = region,
                                                           variable_secundaria = variable,
                                                           vartype = 'cv',na_rm = na_rm)|>
                                  group_by(region)|>
                                  filter(dense_rank(-coef) == lugar)|>
                                  select(- c('_cv','pres')),
                                by = 'region')|>
                              filter(!is.na(region))%>%
                              graficar_mapaRegiones(variable = {{variable}})
                          },
                          mapa_degradadoNumerico = function(variable){

                            if(is.null(self$diseno)) {

                              diseno <- self$encuesta$muestra$diseno

                            } else {

                              diseno <- self$diseno

                            }

                            analizar_frecuenciasRegion(regiones = self$shp,
                                                       variable = {{variable}},
                                                       diseno = diseno) %>%
                              graficar_mapaRegiones(variable = {{variable}}, categorica = F)
                          },
                          heatmap_conocimiento = function(patron_llaveConocimiento, candidatos, respuesta, ordenRegiones = NULL, salto_labelRegiones = 5){

                            if(is.null(self$diseno)) {

                              diseno <- self$encuesta$muestra$diseno

                            } else {

                              diseno <- self$diseno

                            }

                            bd_analizar_conocimientoRegion <- analizar_conocimientoRegion(patron_llaveConocimiento = patron_llaveConocimiento,
                                                                                          aspectos_llaveConocimiento = candidatos,
                                                                                          filtro_respuestaConocimiento = respuesta,
                                                                                          diseno = diseno,
                                                                                          diccionario = self$diccionario)
                            graficar_conocimientoRegion(bd = bd_analizar_conocimientoRegion, ordenRegiones = ordenRegiones, salto_labelRegiones = salto_labelRegiones)
                          },
                          heatmap_saldoOpinion = function(patron_llaveOpinion, candidatos, ns_nc, cat_negativo, cat_regular, cat_positivo,
                                                          ordenRegiones = NULL, salto_labelRegiones = 5){

                            if(is.null(self$diseno)) {

                              diseno <- self$encuesta$muestra$diseno

                            } else {

                              diseno <- self$diseno

                            }

                            analizar_saldoRegion(patron_llaveOpinion = patron_llaveOpinion, aspectos_llaveOpinion = candidatos, ns_nc = ns_nc, cat_negativo = cat_negativo, cat_regular = cat_regular, cat_positivo = cat_positivo, diseno = diseno, diccionario = self$diccionario) %>%
                              graficar_saldoRegion(ordenRegiones = ordenRegiones, salto_labelRegiones = salto_labelRegiones)

                          },
                          mapa_resaltarRegion = function(region, color){
                            self$shp_regiones %>%
                              mutate(color = dplyr::if_else(condition = region %in% !!region,
                                                            true = color,
                                                            false = "gray70")) %>%
                              ggplot(aes(fill = color), color = "black") +
                              geom_sf() +
                              scale_fill_identity() +
                              theme_void() +
                              labs(tit = region)
                          },
                          crear_shp_regiones = function(){
                            sf_use_s2(T)
                            self$shp_regiones <-
                              self$encuesta$shp_completo$shp$SECCION %>%
                              left_join(self$encuesta$muestra$muestra$poblacion$marco_muestral %>%
                                          distinct(SECCION, region), by = "SECCION") %>%
                              group_by(region) %>%
                              summarise(n()) %>%
                              sf::st_buffer(dist = 0)
                            sf_use_s2(F)
                          },
                          resaltar_region = function(color){
                            if(is.null(self$encuesta$muestra$region)){
                              stop("Correr clase$muestra$diseno_region para indicar la region seleccionada")
                            }
                            self$encuesta$Resultados$Regiones$shp_regiones |>
                              mutate(color = if_else(region %in% self$encuesta$muestra$region, color, "gray70")) %>%
                              ggplot(aes(fill = color)) +
                              geom_sf(color = "black") +
                              scale_fill_identity() +
                              theme_void() +
                              labs(tit = self$encuesta$muestra$region)
                          }
                        ))

#'Esta es la clase de Tendencias
#'@export
#'
Tendencias <- R6::R6Class(classname = "Tendencias",
                          public = list(
                            encuesta = NULL,
                            bd_resultados = NULL,
                            initialize = function(encuesta = NULL){
                              self$encuesta <- encuesta
                              self$bd_resultados <- self$encuesta$respuestas$base |>
                                mutate(peso = weights(self$encuesta$muestra$diseno))
                            },
                            intencion_voto = function(variable, valores_interes, colores, sin_peso = T, linea_peso = F, size_fech = 8,size_text_legend = 12){
                              bd_mediaMovil <-
                                calcular_mediaMovil(bd_resultados = self$bd_resultados,
                                                    variable = variable,
                                                    valores_interes = valores_interes,
                                                    sin_peso = sin_peso) |>
                                rename(pct = !!rlang::sym(paste0("movil_", variable)))
                              g <-
                                bd_mediaMovil |>
                                ggplot(aes(x = hora, y = pct, color = !!rlang::sym(variable))) +
                                geom_point(size = 3) +
                                geom_line(linewidth = 1, show.legend = F) +
                                labs(subtitle = "Intención de voto", color = "") +
                                scale_x_datetime(date_breaks = "1 days",
                                                 labels = scales::date_format("%B %d")) +
                                scale_y_continuous(labels = scales::percent) +
                                {if(sin_peso) geom_vline(aes(xintercept =  pos_gen),
                                                         color = "red", size = 1)   }+
                                {if(!sin_peso)
                                  geom_vline(aes(xintercept =  pos_gen_snp),
                                             color = "red", size = 1)}+
                                {if(!sin_peso & linea_peso) geom_vline(aes(xintercept =  pos_gen),
                                                                       color = "red", linetype = "dashed", size = 1)}+
                                scale_color_manual(values = colores) +
                                tema_morant() +
                                theme(panel.grid.major.y = element_line(colour = "#C5C5C5",
                                                                        linetype = "dotted"),
                                      legend.position = "bottom",
                                      axis.text.x = element_text(size = size_fech),
                                      legend.text = element_text(size = size_text_legend))
                              return(g)
                            },
                            conocimiento = function(variables, colores, sin_peso = T, valores_interes = "Sí", linea_peso = F, size_fech = 8,size_text_legend = 12){
                              bd_mediaMovil<- variables|>
                                purrr::map(~{
                                  var_aux <- .x
                                  calcular_mediaMovil(bd_resultados = self$bd_resultados,
                                                      variable = .x,
                                                      valores_interes = valores_interes,
                                                      sin_peso = sin_peso)|>
                                    rename_with(~paste0(.x,'_',var_aux),starts_with('pos_gen') )
                                })|>
                                reduce(left_join, by = 'hora' )|>
                                rename_with(~{gsub(paste0('_',variables[1]),'',.x)},matches(paste0('(pos_gen.*',variables[1],')')))|>
                                select(-matches('pos_gen_.*_'))|>
                                tidyr::pivot_longer(cols = contains('movil_'),
                                                    names_to = 'variable',
                                                    values_to = 'pct')

                              g <-
                                bd_mediaMovil |>
                                ggplot(aes(x = hora, y = pct, color = variable)) +
                                geom_point(size = 3) +
                                geom_line(linewidth = 1, show.legend = F) +
                                labs(subtitle = "Conocimiento", color = "") +
                                scale_color_manual(values = purrr::set_names(colores, paste0("movil_", variables)),
                                                   labels = purrr::set_names(variables, paste0("movil_", variables))) +
                                scale_x_datetime(date_breaks = "1 days",
                                                 labels = scales::date_format("%B %d")) +
                                scale_y_continuous(labels = scales::percent) +
                                {if(sin_peso) geom_vline(aes(xintercept =  pos_gen),
                                                         color = "red", size = 1)   }+
                                {if(!sin_peso)
                                  geom_vline(aes(xintercept =  pos_gen_snp),
                                             color = "red", size = 1)}+
                                {if(!sin_peso & linea_peso) geom_vline(aes(xintercept =  pos_gen),
                                                                       color = "red", linetype = "dashed", size = 1)}+
                                tema_morant() +
                                theme(panel.grid.major.y = element_line(colour = "#C5C5C5",
                                                                        linetype = "dotted"),
                                      legend.position = "bottom",
                                      axis.text.x = element_text(size = size_fech),
                                      legend.text = element_text(size = size_text_legend))
                              return(g)
                            },
                            intencion_voto_region = function(variable, valores_interes, colores, sin_peso = T, variable_region = "region",  linea_peso = F, size_fech = 8,size_text_legend = 12){
                              bd_mediaMovil <-
                                calcular_mediaMovil_region(bd_resultados = self$bd_resultados,
                                                           variable = variable,
                                                           valores_interes = valores_interes,
                                                           variable_region = variable_region,
                                                           sin_peso = sin_peso) |>
                                rename(pct = !!rlang::sym(paste0("movil_", variable)))
                              g <-
                                bd_mediaMovil|>
                                ggplot(aes(x = hora, y = pct, color = !!rlang::sym(variable))) +
                                geom_point(size = 3) +
                                geom_line(linewidth = 1, show.legend = F) +
                                labs(subtitle = "Intención de voto", color = "") +
                                scale_x_datetime(date_breaks = "1 days",
                                                 labels = scales::date_format("%B %d")) +
                                scale_y_continuous(labels = scales::percent) +
                                scale_color_manual(values = colores) +
                                tema_morant() +
                                theme(panel.grid.major.y = element_line(colour = "#C5C5C5",
                                                                        linetype = "dotted")) +
                                {if(sin_peso) geom_vline(aes(xintercept =  pos_gen),
                                                         color = "red", size = 1)   }+
                                # {if(sin_peso) geom_vline(aes(xintercept =  pos_reg_gen),
                                #                          color = "blue", size = 1)   }+
                                {if(!sin_peso)
                                  geom_vline(aes(xintercept =  pos_gen_snp),
                                             color = "red", size = 1)}+
                                # {if(!sin_peso)
                                # geom_vline(aes(xintercept =  pos_reg_gen_snp),
                                #            color = "blue", size = 1)}+
                                {if(!sin_peso &  linea_peso) geom_vline(aes(xintercept =  pos_gen),
                                                                        color = "red", linetype = "dashed", size = 1)}+
                                # {if(!sin_peso) geom_vline(aes(xintercept =  pos_reg_gen),
                                #                           color = "blue", linetype = "dashed", size = 1)}+
                                facet_wrap(as.formula(paste0("~", variable_region)))+
                                theme(legend.position = "bottom",
                                      axis.text.x = element_text(size = size_fech),
                                      legend.text = element_text(size = size_text_legend))
                              return(g)
                            },
                            conocimiento_region = function(variables, colores, sin_peso = T, valores_interes = "Sí", variable_region = "region",  linea_peso = F, size_fech = 8,size_text_legend = 12){
                              bd_mediaMovil <-variables|>
                                purrr::map(~{
                                  var_aux <- .x
                                  calcular_mediaMovil_region(bd_resultados = self$bd_resultados,
                                                             variable = .x,
                                                             valores_interes = valores_interes,
                                                             variable_region = variable_region,
                                                             sin_peso = sin_peso)|>
                                    rename_with(~paste0(.x,'_',var_aux),starts_with('pos_gen') )|>
                                    rename_with(~paste0(.x,'_',var_aux),starts_with('pos_reg_gen') )

                                })|>
                                reduce(left_join, by = c(variable_region,'hora') )|>
                                rename_with(~{gsub(paste0('_',variables[1]),'',.x)},matches(paste0('(pos_gen.*',variables[1],')')))|>
                                rename_with(~{gsub(paste0('_',variables[1]),'',.x)},matches(paste0('(pos_reg_gen.*',variables[1],')')))|>
                                select(-matches('pos_gen_.*_'),-matches('pos_reg_gen_.*_') )|>
                                tidyr::pivot_longer(cols = contains('movil_'),
                                                    names_to = 'variable',
                                                    values_to = 'pct')

                              g <-
                                bd_mediaMovil |>
                                ggplot(aes(x = hora, y = pct, color = variable)) +
                                geom_point(size = 3) +
                                geom_line(linewidth = 1, show.legend = F) +
                                labs(subtitle = "Conocimiento", color = "") +
                                scale_color_manual(values = purrr::set_names(colores, paste0("movil_", variables)),
                                                   labels = purrr::set_names(variables, paste0("movil_", variables))) +
                                scale_x_datetime(date_breaks = "1 days",
                                                 labels = scales::date_format("%B %d")) +
                                scale_y_continuous(labels = scales::percent) +
                                tema_morant() +
                                theme(panel.grid.major.y = element_line(colour = "#C5C5C5",
                                                                        linetype = "dotted")) +
                                {if(sin_peso) geom_vline(aes(xintercept =  pos_gen),
                                                         color = "red", size = 1)   }+
                                # {if(sin_peso) geom_vline(aes(xintercept =  pos_reg_gen),
                                #                          color = "blue", size = 1)   }+
                                {if(!sin_peso)
                                  geom_vline(aes(xintercept =  pos_gen_snp),
                                             color = "red", size = 1)}+
                                # {if(!sin_peso)
                                #   geom_vline(aes(xintercept =  pos_reg_gen_snp),
                                #              color = "blue", size = 1)}+
                                {if(!sin_peso &  linea_peso) geom_vline(aes(xintercept =  pos_gen),
                                                                        color = "red", linetype = "dashed", size = 1)}+
                                # {if(!sin_peso) geom_vline(aes(xintercept =  pos_reg_gen),
                                #                           color = "blue", linetype = "dashed", size = 1)}+
                                facet_wrap(as.formula(paste0("~", variable_region)))+
                                theme(legend.position = "bottom",
                                      axis.text.x = element_text(size = size_fech),
                                      legend.text = element_text(size = size_text_legend))
                              return(g)
                            }
                          ))

#'Esta es la clase de Modelo
#'@export
#'
Modelo <- R6::R6Class(classname = "Modelo",
                      public = list(
                        encuesta = NULL,
                        diseno = NULL,
                        diccionario = NULL,
                        tema = NULL,
                        graficadas = NULL,
                        initialize = function(encuesta = NULL, diseno = NULL, diccionario = NULL, tema, graficadas = NULL){
                          self$encuesta <- encuesta
                          self$diseno <- diseno
                          self$diccionario <- diccionario
                          self$tema <- tema
                          self$graficadas <- graficadas
                        },
                        correspondencia = function(var1, var2, legenda1 = NULL, legenda2 = NULL, colores = NULL){


                          analisis_correspondencia(var1 = var1, var2 = var2, legenda1 = legenda1, legenda2 = legenda2, diseno = self$diseno, colores = colores)
                        },
                        componentesPrincipales = function(variables){


                          pc <- survey::svyprcomp(survey::make.formula(variables),
                                                  design = self$diseno,
                                                  scale=TRUE, scores=TRUE)
                          factoextra::fviz_pca_biplot(pc, geom.ind = "point", labelsize = 2, repel = T)}
                      #   ,
                      #   blackBox = function(vars, stimuli){
                      #
                      #     if(is.null(self$diseno)) {
                      #
                      #       diseno <- self$encuesta$muestra$diseno
                      #
                      #     } else {
                      #
                      #       diseno <- self$diseno
                      #
                      #     }
                      #
                      #     diseno$variables %>%
                      #       as_tibble() |>
                      #       analizar_blackbox_1d(vars, stimuli) %>%
                      #       graficar_blackbox_1d()
                      #
                      #   }
                      # )
                      ))

#' Title
#'
#' @param encuesta
#' @param dir
#'
#' @return
#' @export
#'
#' @examples
Auditoria <- R6::R6Class("Auditoria",
                         public = list(
                           dir = NULL,
                           initialize = function(encuesta, tipo_encuesta, dir){
                             if(!file.exists(dir)){
                               dir.create(dir)
                             }
                             dir.create(glue::glue("{dir}/data"))
                             readr::write_rds(encuesta$Resultados, glue::glue("{dir}/data/clase_pregunta.rda"))
                             # readr::write_rds(encuesta$muestra$muestra, glue::glue("{dir}/data/diseno.rda"))
                             # readr::write_rds(encuesta$shp_completo, glue::glue("{dir}/data/shp.rda"))
                             # readr::write_excel_csv(encuesta$respuestas$base, glue::glue("{dir}/data/bd.csv"))
                             sf_use_s2(T)
                             mapa_base <- encuesta$shp_completo$shp$MUNICIPIO %>%
                               left_join(encuesta$muestra$muestra$poblacion$marco_muestral %>% distinct(MUNICIPIO,strata_1)) %>%
                               group_by(strata_1) %>% summarise(n()) %>%
                               sf::st_buffer(dist = 0)
                             readr::write_rds(mapa_base, glue::glue("{dir}/data/mapa_base.rda"))

                             enc_shp <- encuesta$respuestas$base %>%
                               filter(!is.na(Longitude) | !is.na(Latitude)) %>%
                               sf::st_as_sf(coords = c("Longitude","Latitude"), crs = "+init=epsg:4326")
                             readr::write_rds(enc_shp, glue::glue("{dir}/data/enc_shp.rda"))
                             # Calcular la relacióin entre cluster y seccion
                             encuesta$muestra$muestra$poblacion$marco_muestral |>
                               distinct(SECCION, cluster_2) |>
                               readr::write_rds(glue::glue("{dir}/data/catalogo_seccion_cluster.rda"))

                             # readr::write_excel_csv(encuesta$respuestas$eliminadas, glue::glue("{dir}/data/eliminadas.csv"))
                             if(tipo_encuesta == "inegi"){
                               file.copy(overwrite = T,
                                         from = system.file("app_inegi/app.R", package = "encuestar",
                                                            mustWork = TRUE),
                                         to = dir

                               )
                             }

                             if(tipo_encuesta == "ine"){
                               file.copy(overwrite = T,
                                         from = system.file("app_ine/app.R", package = "encuestar",
                                                            mustWork = TRUE),
                                         to = dir

                               )
                             }

                             self$dir <- dir
                           },
                           run_app = function(){
                             shiny::shinyAppDir(
                               self$dir
                             )
                           },
                           subir_app = function(...){
                             rsconnect::deployApp(self$dir,...)
                           }
                         ))
