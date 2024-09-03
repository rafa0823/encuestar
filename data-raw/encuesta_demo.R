
# Preambulo -----------------------------------------------------------------------------------

library(dplyr)
library(encuestar)

# Insumos -------------------------------------------------------------------------------------

shp_hermosillo_agosto <- readr::read_rds("./data-raw/shp.rda")

diseno_hermosillo_agosto <- readr::read_rds("./data-raw/diseno.rda")

diccionario_hermosillo_agosto <-
  readxl::read_xlsx(path = "./data-raw/dicc_enc_hermosillo_agosto.xlsx") |>
  dplyr::filter(!grepl(pattern = "Registro de ubicación|Filtros", x = bloque))

# data-raw ------------------------------------------------------------------------------------

bd_respuestas_hermosillo_agosto <-
  openxlsx2::read_xlsx(file = "./data-raw/respuestas_campo_hermosillo_agosto.xlsx", na.strings = "-1") |>
  as_tibble() |>
  dplyr::filter(!Srvyr %in% c("test", "Katheryn Hernandez")) |>
  dplyr::mutate(SECCION = as.character(as.numeric(cluster))) |>
  dplyr::mutate(generacion = case_when(edad >= 18 & edad <= 25 ~ "Generación Z (18 a 25 años)",
                                       edad >= 26 & edad <= 40 ~ "Millenials (26 a 40 años)",
                                       edad >= 41 & edad <= 55 ~ "Generación X (41 a 55 años)",
                                       edad >= 56  ~ "Baby Boomers  (56 años o más)"),
                generacion = factor(generacion, levels = c("Generación Z (18 a 25 años)",
                                                           "Millenials (26 a 40 años)",
                                                           "Generación X (41 a 55 años)",
                                                           "Baby Boomers  (56 años o más)"))) |>
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
  dplyr::rename(otro_problema_principal = Q_39_S,
                otro_problema_secundario = Q_40_S,
                otro_problema_inseguridad = Q_41_S,
                otro_medios_com = S_Q_42_14,
                otro_voto_pm_21 = Q_54_S,
                otro_motivacion_voto_pm_24 = S_Q_56_11,
                otro_voto_pm_24 = Q_57_S,
                otro_influencia_voto_pm_24 = Q_58_S,
                otro_influencia_voto_pr_24 = Q_61_S,
                otro_noparticipacion_24 = S_Q_64_11) |>
  mutate(voto_pr_24 = gsub('Xóchilt','Xóchitl', voto_pr_24))

intentos_efectivos <-
  bd_respuestas_hermosillo_agosto |>
  select(SbjNum, num_range("INT", 1:20)) |>
  mutate(across(.cols = !SbjNum, .fns = ~ as.character(.x))) |>
  tidyr::pivot_longer(cols = !SbjNum, names_to = "variable", values_to = "rechazo") |>
  filter(grepl(pattern = 'Iniciar entrevista', x = rechazo)) |>
  mutate(intento_efectivo = gsub(pattern = "INT", replacement = "", x = variable)) |>
  select(SbjNum, intento_efectivo)

geolocalizacion_efectiva <-
  purrr::pmap_df(.l = list(ids = intentos_efectivos %>% pull(SbjNum),
                           intento_efectivo = intentos_efectivos %>% pull(intento_efectivo)),
                 .f = ~ encuestar::obtener_ubicacionEfectiva_surveyToGo(bd_respuestas = bd_respuestas_hermosillo_agosto,
                                                                        id = ..1,
                                                                        intento_efectivo = ..2))
bd_respuestas_hermosillo_agosto <-
  bd_respuestas_hermosillo_agosto |>
  left_join(geolocalizacion_efectiva, by = "SbjNum") |>
  mutate(Latitude = GPS_INT_LA,
         Longitude = GPS_INT_LO)

# BASE DE CORRECCIONES
bd_correcciones_hermosillo_agosto <-
  readxl::read_excel(path = "data-raw/bd_correcciones_hermosillo_agosto.xlsx") |>
  janitor::clean_names() |>
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
  mutate(correccion = gsub('Las caracterísitcas del candidato','Las características del candidato', correccion))

# BASE DE ELIMINADAS
eliminadas <-
  readxl::read_excel(path = "./data-raw/bd_eliminadas_hermosillo_agosto.xlsx")

# Omitir variables
quitar <- c()

# Clusters a los que forzar entrevistas
mantener <- ""

# Clase -------------------------------------------------------------------

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

usethis::use_data(encuesta_demo, encuesta_demo, internal = TRUE, overwrite = TRUE)
