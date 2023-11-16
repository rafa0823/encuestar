## code to prepare `encuesta` dataset goes here
# Preámbulo ---------------------------------------------------------------

library(dplyr)
library(encuestar)
library(tibble)
library(ggplot2)
library(readxl)
setwd("../enc_chis_oct23")
source("R/funcion_amai.R")

# Insumos -----------------------------------------------------------------

shp <- readr::read_rds("Insumos/shp.rda")
diseno_chiapas <- readr::read_rds("Insumos/diseño.rda")

diccionario <- readxl::read_excel(path = "Insumos/Diccionario_cuestionario_VAA.xlsx") |>
  filter(!bloque %in% c("Registro de ubicación", "Filtros")) |>
  mutate(respuestas = gsub(pattern = " (No leer)", replacement = "", x = respuestas),
         tema = gsub(pattern = "NA", replacement = NA, x = tema),
         respuestas = dplyr::if_else(condition = tipo_pregunta == "abiertas", true = "Abierta", false = respuestas)) |>
  mutate(respuestas = stringr::str_split(respuestas, "_"),
         tema = if_else(is.na(tema), NA_character_, tema)) |>
  filter(!llaves %in% c("preferencia_candidatosenado_otro",
                        "recibir_nombre_nombres", "recibir_nombre_apaterno", "recibir_nombre_amaterno")) |>
  tidyr::unnest(cols = respuestas) |>
  mutate(across(.cols = c(tema, respuestas), .fns = ~ gsub(pattern = "\"Doctor\"", replacement = "“Doctor”", x = .x)),
         across(.cols = c(tema, respuestas), .fns = ~ gsub(pattern = "Ns/Nc \\(No leer\\)", replacement = "Ns/Nc", x = .x)),
         across(.cols = c(tema, respuestas), .fns = ~ gsub(pattern = "Otro: \\(No leer\\)", replacement = "Otro", x = .x)),
         across(.cols = c(tema, respuestas), .fns = ~ gsub(pattern = "Otro:\\(No leer\\)", replacement = "Otro", x = .x)),
         across(.cols = c(tema, respuestas), .fns = ~ gsub(pattern = "Otro:", replacement = "Otro", x = .x)),
         across(.cols = c(tema, respuestas), .fns = ~ gsub(pattern = "Ninguno \\(No leer\\)", replacement = "Ninguno", x = .x)),
         across(.cols = c(tema, respuestas), .fns = ~ gsub(pattern = "Regular \\(No leer\\)", replacement = "Regular", x = .x)),
         across(.cols = c(tema, respuestas), .fns = ~ gsub(pattern = "Candidato independiente \\(No leer\\)", replacement = "Candidato independiente", x = .x)),
         across(.cols = c(tema, respuestas), .fns = ~ gsub(pattern = "No contesta \\(No leer\\)", replacement = "No contesta", x = .x)),
  ) |>
  group_by(bloque, pregunta, tipo_pregunta, llaves, tema) |>
  summarise("respuestas" = list(respuestas), .groups = "drop")

diseño <- diseno_chiapas$poblacion$marco_muestral |>
  distinct(SECCION, cluster_2) |>
  transmute(seccion_ine=as.numeric(SECCION),
            SECCION=as.character(cluster_2))

rurub <- read.csv("Insumos/seccion_tipo_rurub.csv") |>
  rename(seccion_ine=seccion) |>
  left_join(diseño,by = join_by(seccion_ine))

mujeres_senadoras <- diccionario |>
  filter(llaves == "pereferencia_candidatasenado") |>
  tidyr::unnest(respuestas) |>
  # filter(!respuestas %in% c("Otro", "Ns/Nc", "Ninguno")) |>
  pull(respuestas)

hombres_senadores <- diccionario |>
  filter(llaves == "preferencia_candidatosenado") |>
  tidyr::unnest(respuestas) |>
  # filter(!respuestas %in% c("Otro", "Ns/Nc", "Ninguno")) |>
  pull(respuestas)

respuestas <- readxl::read_excel(path = "data-raw/respuestas_campo.xlsx", na = "-1") |>
  filter(Srvyr != "test") |>
  rename(SECCION = cluster) |>
  mutate(SECCION = dplyr::if_else(condition = SECCION == "933",
                                  true = "746",
                                  false = SECCION),
         SECCION = dplyr::if_else(condition = SECCION == "745",
                                  true = "746",
                                  false = SECCION),
         SECCION = dplyr::if_else(condition = SECCION == "2078",
                                  true = "2038",
                                  false = SECCION)) |>
  rename("conocimiento_eduardo" = conocimiento_era) |>
  mutate(voto_candidato_presidencia = dplyr::if_else(condition = voto_candidato_presidencia == "Xóchilt Gálvez por PAN-PRI-PRD",
                                                     true = "Xóchitl Gálvez por PAN-PRI-PRD",
                                                     false = voto_candidato_presidencia),
         voto_candidato_presidencia_segunda = dplyr::if_else(condition = voto_candidato_presidencia_segunda == "Xóchilt Gálvez por PAN-PRI-PRD",
                                                             true = "Xóchitl Gálvez por PAN-PRI-PRD",
                                                             false = voto_candidato_presidencia_segunda)) |>
  group_by(SbjNum) |>
  mutate(AMAI=calcular_socioAMAI(
    jefe=amai_estudios,
    wc=amai_banos,
    autos=amai_automoviles,
    internet=amai_internet,
    trabajo=amai_personastrabajaron,
    cuartos=amai_dormitorios)) |>
  ungroup() |>
  left_join(rurub,by = join_by(SECCION)) |>
  mutate(AMAI_factor=factor(AMAI, levels=c("E","D","D_mas",
                                           "C_menos","C","C_mas",
                                           "A/B"))) |>
  mutate(grupo_edad=case_when(edad>=18 & edad<=29 ~ "Personas jóvenes\n(18 a 29 años)",
                              edad>=30 & edad<=59 ~ "Personas adultas\n(30 a 59 años)",
                              edad>=60 ~ "Personas adultas mayores\n(60 años o más)"),
         grupo_edad=factor(grupo_edad, levels=c("Personas jóvenes\n(18 a 29 años)",
                                                "Personas adultas\n(30 a 59 años)",
                                                "Personas adultas mayores\n(60 años o más)")),
         edad_grupo_calibrar=case_when(edad>=18 & edad<=24 ~ "de18a24",
                                       edad>=25 & edad<=39 ~ "de25a39",
                                       edad>=40 & edad<=59 ~ "de40a59",
                                       edad>=60 ~ "de60amas"))

# Eliminadas
eliminadas_filename <- "data-raw/Eliminadas.xlsx"

eliminadas <- read_excel(eliminadas_filename)


# Correccion de respuestas

corregir_filename <- "data-raw/correcciones.xlsx"

bd_corregir <- read_excel(corregir_filename)

respuestas_a_corregir <- function(bd_respuestas, id_entrevista, codigo_pregunta, respuesta_capturada, respuesta_correcta) {

  respuestas_corregidas <- bd_respuestas %>%
    mutate(!!rlang::sym(codigo_pregunta) := dplyr::if_else(condition = SbjNum == id_entrevista,
                                                           true = respuesta_correcta,
                                                           false = !!rlang::sym(codigo_pregunta)))

  return(respuestas_corregidas)

}

for(i in seq_along(bd_corregir$SbjNum)) {

  print(i)

  id_entrevista = bd_corregir$SbjNum[i]
  codigo_pregunta = bd_corregir$codigo_pregunta[i]
  respuesta_capturada = bd_corregir$capturada[i]
  respuesta_correcta = bd_corregir$correccion[i]

  a <- respuestas |>
    filter(SbjNum == id_entrevista) |>
    select(!!codigo_pregunta) |>
    pull()

  print(a)

  respuestas <- respuestas_a_corregir(bd_respuestas = respuestas,
                                      id_entrevista = id_entrevista,
                                      codigo_pregunta = codigo_pregunta,
                                      respuesta_capturada = respuesta_capturada,
                                      respuesta_correcta = respuesta_correcta)

  b <- respuestas |>
    filter(SbjNum == id_entrevista) |>
    select(!!codigo_pregunta) |>
    pull()

  print(b)

}

quitar <- c()
mantener <- ""

Sys.setlocale(locale = "es_ES.UTF-8")


# generar dicotomicas
candidatos_interes <- c("Carlos Morales","“Doctor” Pepe Cruz","Sasil de León","Eduardo Ramírez Aguilar","José Antonio Aguilar Castillejos, “Jaac”")

for (candidato in candidatos_interes) {
  variable_nombre <- paste0("preflocalcalibrar_", tolower(gsub(" ", "_", candidato)))
  respuestas <- respuestas %>%
    mutate({{variable_nombre}} := ifelse(candidato_preferencia == candidato, "Sí", "No"))
}

candidatos_interes <- select(respuestas, voto_candidato_presidencia) |>
  filter(!(voto_candidato_presidencia %in% c("Ns/Nc (No leer)","Ninguno (No leer)")))  |>
  pull() |>
  unique()

for (candidato in candidatos_interes) {
  variable_nombre <- paste0("votopresideciacalibrar_", tolower(gsub(" ", "_", candidato)))
  respuestas <- respuestas %>%
    mutate({{variable_nombre}} := ifelse(voto_candidato_presidencia == candidato, "Sí", "No"))
}

# Clase -------------------------------------------------------------------
encuesta <- Encuesta$new(respuestas = respuestas, #si es NA van a ser simulaciones
                                 # n_simulaciones = 50,
                                 quitar_vars = quitar,
                                 mantener = mantener,
                                 muestra = diseno_chiapas,
                                 auditoria_telefonica = eliminadas,
                                 cuestionario = diccionario,
                                 shp = shp,
                                 sin_peso = F,
                                 tipo_encuesta = "ine",
                                 mantener_falta_coordenadas = F, # mantener entrevistas sin coordenadas
                                 rake = T, ######### con postestratificacion
                                 patron = "\\(No leer\\)| \\(No leer\\)|\\(ROTAR\\)|\\(No leer)|:",
                                 auditar = c("conocimiento_zoe", "conocimiento_manuelita",
                                             "voto_partido", "preferencia_candidatopresidencia")
)

setwd("../encuestar")
usethis::use_data(encuesta, overwrite = TRUE)
