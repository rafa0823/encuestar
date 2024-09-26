
# Preambulo -----------------------------------------------------------------------------------

library(dplyr)
# Cargar la version de desarrollo de la librería encuestar que deberá estar instalada en sistema
library(encuestar)

# Insumos -------------------------------------------------------------------------------------

#+ Se carga el archivo shp con la cartografia de la zona de la encuesta
shp_hermosillo_agosto <- readr::read_rds("./data-raw/shp.rda")

#+ Se carga el diseno de la encuesta generado con la paqueteria muestreaR
diseno_hermosillo_agosto <- readr::read_rds("./data-raw/diseno.rda")

#+ Se carga el diccionario de variables en la encuesta
diccionario_hermosillo_agosto <-
  readxl::read_xlsx(path = "./data-raw/diccionario_enc_demo.xlsx") |> #dicc_enc_hermosillo_agosto.xlsx
  dplyr::filter(!grepl(pattern = "Registro de ubicación|Filtros", x = bloque))

# data-raw ------------------------------------------------------------------------------------

#+ Se carga la base de datos de la encuesta en curso
bd_respuestas_hermosillo_agosto <-
  openxlsx2::read_xlsx(file = "./data-raw/bd_demo.xlsx", na.strings = "-1") |> #respuestas_campo_hermosillo_agosto.xlsx
  as_tibble() |>
  #+ Se eliminan variables de prueba
  dplyr::filter(!Srvyr %in% c("test", "Katheryn Hernandez")) |>
  dplyr::mutate(SECCION = as.character(as.numeric(cluster))) |>
  #+ Se crean las variables para definir grupos etareos
  dplyr::mutate(generacion = case_when(edad >= 18 & edad <= 25 ~ "Generación Z (18 a 25 años)",
                                       edad >= 26 & edad <= 40 ~ "Millenials (26 a 40 años)",
                                       edad >= 41 & edad <= 55 ~ "Generación X (41 a 55 años)",
                                       edad >= 56  ~ "Baby Boomers  (56 años o más)"),
                generacion = factor(generacion, levels = c("Generación Z (18 a 25 años)",
                                                           "Millenials (26 a 40 años)",
                                                           "Generación X (41 a 55 años)",
                                                           "Baby Boomers  (56 años o más)"))) |>
  #+ Se crean las variables para definir nivel de estudios
  dplyr::mutate(grado2 = case_when(grepl("Primaria", estudios) ~ "Educación básica",
                                   grepl("Secundaria", estudios) ~ "Educación básica",
                                   estudios == "No estudió" ~ "Educación básica",
                                   grepl("Preparatoria", estudios) ~ "Educación media superior",
                                   estudios %in% c("Licenciatura completa",
                                                   "Licenciatura incompleta",
                                                   "Doctorado",
                                                   "Diplomado o maestría") ~ "Educación superior o más",
                                   .default = "No contesta")) |>
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
                  (suma_amai>=0 & suma_amai<=47)~"E",
                  (suma_amai>=48 & suma_amai<=94)~"D",
                  (suma_amai>=95 & suma_amai<=115)~"D_mas",
                  (suma_amai>=116 & suma_amai<=140)~"C_menos",
                  (suma_amai>=141 & suma_amai<=167)~"C",
                  (suma_amai>=168 & suma_amai<=201)~"C_mas",
                  suma_amai>=202~"A_B",.default = NA)) |>
  #+ Se Hacen correcciones en alguna de las varibles, en caso de ser detectado
  mutate(voto_pr_24 = gsub('Xóchilt','Xóchitl', voto_pr_24))

#+ Se definen lo intentos efectivos, que son las enetrevistas que se realizaron correctamente y las que se rechazaron
intentos_efectivos <-
  bd_respuestas_hermosillo_agosto |>
  select(SbjNum, num_range("INT", 1:20)) |>
  mutate(across(.cols = !SbjNum, .fns = ~ as.character(.x))) |>
  tidyr::pivot_longer(cols = !SbjNum, names_to = "variable", values_to = "rechazo") |>
  filter(grepl(pattern = 'Iniciar entrevista', x = rechazo)) |>
  mutate(intento_efectivo = gsub(pattern = "INT", replacement = "", x = variable)) |>
  select(SbjNum, intento_efectivo)

# GEOLOCALIZACIONES EFECTIVAS
#+ Se cofirma que las localizaciones de las entrevistas consideradas como efectivas
geolocalizacion_efectiva <-
  purrr::pmap_df(.l = list(ids = intentos_efectivos %>% pull(SbjNum),
                           intento_efectivo = intentos_efectivos %>% pull(intento_efectivo)),
                 #+ Se ocupa la funcion de la libreria encuestar "obtener_ubicacionEfectiva_surveyToGo" para procesar las ubicaciones registradas para cada entrecista
                 .f = ~ encuestar::obtener_ubicacionEfectiva_surveyToGo(bd_respuestas = bd_respuestas_hermosillo_agosto,
                                                                        id = ..1,
                                                                        intento_efectivo = ..2))

#+ Se agregan los resultados de geolocalizacion y numero de intentos de entrevista a las bases
bd_respuestas_hermosillo_agosto <-
  bd_respuestas_hermosillo_agosto |>
  left_join(geolocalizacion_efectiva, by = "SbjNum") |>
  mutate(Latitude = GPS_INT_LA,
         Longitude = GPS_INT_LO)

# BASE DE CORRECCIONES
#+ En caso de que se necesiten hacer correcciones en alguna varible de la encuesta,
#+ se carga la base de datos con las correcciones correspondientes
bd_correcciones_hermosillo_agosto <-
  readxl::read_excel(path = "data-raw/bd_correcciones_hermosillo_agosto.xlsx") |>
  janitor::clean_names() |>
  #+ Se le da formato a la base de correcciones
  select(SbjNum = municipio,
         codigo_pregunta = codigo_survey,
         capturada = capturada,
         correccion = correcion) |>
  mutate(codigo_pregunta = gsub(pattern = "\n", replacement = "", x = codigo_pregunta),
         correccion = dplyr::if_else(condition = grepl(pattern = "Cuestiones", x = correccion),
                                     true = "Cuestiones de clima",
                                     false = correccion),
         correccion = dplyr::if_else(condition = grepl(pattern = "Mi voto dí cuenta o marca la diferencia", x = correccion),
                                     true = "Mi voto sí cuenta o marca la diferencia",
                                     false = correccion),
         codigo_pregunta = dplyr::if_else(condition = grepl(pattern = "voto_pr", x = codigo_pregunta),
                                          true = "influencia_voto_pr_24",
                                          false = codigo_pregunta)) |>
  mutate(correccion = gsub('Las caracterísitcas del candidato','Las características del candidato', correccion))|>
  filter(codigo_pregunta=='noparticipacion_24_O1') |>
  rename(llave = codigo_pregunta)


# BASE DE categorias
#+ En caso de existir preguntas abiertas categorizadas,
#+ se carga la base con las categorizaciones
categorias_path <- './data-raw/bd_demo_categorias.xlsx'
categorias <- readxl::read_xlsx(categorias_path)

#+ Se agregan los resultados de las categorias a la base de datos
bd_respuestas_hermosillo_agosto <-
  bd_respuestas_hermosillo_agosto |>
  left_join(categorias,
            by = 'SbjNum')

# Pegar datos necesarios para grafica de candidato opinion
bd_respuestasParciales_alvaroObregon_mayo <-
  readxl::read_xlsx(path = "./data-raw/respuestas_campo_avlaroobregon_mayo_2024.xlsx", na = "-1") |>
  select(starts_with("conoce_pm"),
         starts_with("partido_pm")) |>
  head(1230)

bd_respuestas_hermosillo_agosto <-
  bd_respuestas_hermosillo_agosto |>
  bind_cols(bd_respuestasParciales_alvaroObregon_mayo)

# BASE DE ELIMINADAS
#+ Se agrega la lista de encuestas eliminadas por auditoria
eliminadas <-
  readxl::read_excel(path = "./data-raw/bd_eliminadas_hermosillo_agosto.xlsx")

#+ En caso de existir varibales que no se vayan a utilizar, se agrega la lista de variables que no se vayan a considerar
# Omitir variables
quitar <- c()

# Clusters a los que forzar entrevistas
mantener <- ""

# Clase -------------------------------------------------------------------
#+ Se crea la clase encuesta, que contiene la base de datos, asi como el diseno muestral, y la app de auditoria
encuesta_demo <- Encuesta$new(respuestas = bd_respuestas_hermosillo_agosto,
                              # n_simulaciones = 200,
                              quitar_vars = quitar,
                              mantener = mantener,
                              bd_correcciones = bd_correcciones_hermosillo_agosto,
                              muestra = diseno_hermosillo_agosto,
                              auditoria_telefonica = eliminadas,
                              cuestionario = diccionario_hermosillo_agosto,
                              shp = shp_hermosillo_agosto,
                              sin_peso = F,
                              tipo_encuesta = "ine",
                              mantener_falta_coordenadas = F, # mantener entrevistas sin coordenadas
                              rake = T, ######### con postestratificacion
                              patron = "\\(No leer\\)| \\(No leer\\)|\\(ROTAR\\)|\\(No leer)|:",
                              auditar = c("")
)

# encuesta_demo$auditoria$run_app()

usethis::use_data(encuesta_demo, overwrite = TRUE)
