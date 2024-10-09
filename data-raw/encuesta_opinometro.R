# SCRIPT DE PRUEBAS PARA GENERAR UNA CLASE ENCUESTA USANDO COMO INSUMO EL OPINOMETRO
# Al generar la clase, se escriben los archivos R/constantes.R, R/funciones.R y un folder
# llamado auditoria/ los cuales no forman parte edl desarrollo salvo que se esté trabajando en la
# encuesta demo

# Preambulo -----------------------------------------------------------------------------------

library(dplyr)
library(encuestar)

# Insumos -------------------------------------------------------------------------------------

shp_hermosillo_agosto <-
  readr::read_rds("./data-raw/shp.rda")

diseno_hermosillo_agosto <-
  readr::read_rds("./data-raw/diseno.rda")

diccionario_sonora_agosto <-
  readxl::read_xlsx(path = "./data-raw/diccionario_sonora_20240802.xlsx") |>
  filter(!llaves %in% c("gps", "intentos"))

# Base de eliminadas --------------------------------------------------------------------------

eliminadas <-
  tibble(SbjNum = 000,
         razon = sample(c("razon_a", "razon_b", "razon_c"),
                        size = 1,
                        replace = TRUE))

quitar <- c()

mantener <- ""

diccionario_sonora_opinometro <-
  diccionario_sonora_agosto |>
  mutate(llaves = gsub(pattern = "prioridad_gobireno",
                       replacement = "prioridad_gobierno",
                       x = llaves),
         llaves = gsub(pattern = "01",
                       replacement = "O1",
                       x = llaves),
         llaves = gsub(pattern = "02",
                       replacement = "O2",
                       x = llaves),
         llaves = gsub(pattern = "03",
                       replacement = "O3",
                       x = llaves)) |>
  filter(!llaves %in% c("razon_calificacion_gobierno",
                        "otro_problema_principal",
                        "otro_problema_secundario",
                        "ine",
                        "otro_ocupacion",
                        "salario")) |>
  filter(!grepl(pattern = "medio", x = llaves))

# Clase -------------------------------------------------------------------

encuesta_opinometro <- Encuesta$new(respuestas = NA,
                                    # n_simulaciones = 200,
                                    opinometro_id = 167,
                                    quitar_vars = quitar,
                                    mantener = mantener,
                                    bd_correcciones = NULL,
                                    muestra = diseno_hermosillo_agosto,
                                    auditoria_telefonica = eliminadas,
                                    cuestionario = diccionario_sonora_opinometro,
                                    shp = shp_hermosillo_agosto,
                                    sin_peso = T,
                                    mantener_falta_coordenadas = F, # mantener entrevistas sin coordenadas
                                    rake = T, ######### con postestratificacion
                                    patron = "\\(No leer\\)| \\(No leer\\)|\\(ROTAR\\)|\\(No leer)|:",
                                    auditar = c("")
)

options(survey.lonely.psu = "remove")

# encuesta_opinometro$auditoria$run_app()
