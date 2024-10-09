
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
  filter(!llaves %in% c("gps", "intentos"))

# data-raw ------------------------------------------------------------------------------------

#+ Se carga la base de datos de la encuesta en curso
bd_respuestas_hermosillo_agosto <-
  openxlsx2::read_xlsx(file = "./data-raw/bd_demo.xlsx", na.strings = "-1") |> #respuestas_campo_hermosillo_agosto.xlsx
  as_tibble() |>
  #+ Se eliminan variables de prueba
  dplyr::filter(!Srvyr %in% c("test", "Katheryn Hernandez")) |>
  dplyr::mutate(SECCION = as.character(as.numeric(cluster))) |>
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
  #+ Se Hacen correcciones en alguna de las varibles, en caso de ser detectado
  mutate(voto_pr_24 = gsub('Xóchilt','Xóchitl', voto_pr_24))

set.seed(123)

bd_respuestas_hermosillo_agosto <-
  bd_respuestas_hermosillo_agosto |>
  mutate(ine = sample(c("Sí", "No"),
                        size = nrow(bd_respuestas_hermosillo_agosto),
                        replace = TRUE))

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

variables_interes <- c("voto_pm_24", "conoce_pm_astiazaran", "voto_pr_24")
variables_tendencias <- c("voto_pm_24", "conoce_pm_astiazaran", "conoce_pm_delrio")

# Clase -------------------------------------------------------------------
#+ Se crea la clase encuesta, que contiene la base de datos, asi como el diseno muestral, y la app de auditoria
encuesta_demo <- Encuesta$new(respuestas = bd_respuestas_hermosillo_agosto,
                              # n_simulaciones = 200,
                              quitar_vars = quitar,
                              bd_categorias = categorias,
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
                              auditar = variables_interes,
                              vars_tendencias = variables_tendencias,
)

# encuesta_demo$auditoria$run_app()

usethis::use_data(encuesta_demo, overwrite = TRUE)
