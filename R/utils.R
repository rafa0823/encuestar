#' Obtener telemetría de intento efectivo de encuesta
#'
#' @param bd_respuestas Base de datos con formato exportado de SurveyToGo
#' @param id Id único que identifica cada entrevista, generalmente se llama SbjNum
#' @param intento_efectivo Veces que se intenta levantar una entrevista hasta que esta comienza
#'
#' @return
#' @export
#'
#' @examples
obtener_ubicacionEfectiva_surveyToGo = function(bd_respuestas, id, intento_efectivo) {
  bd_respuestas |>
    filter(SbjNum == id) |>
    select(SbjNum,
           paste0("INT",
                  intento_efectivo),
           paste0("GPS_INT",
                  intento_efectivo,
                  "_",
                  c("LA", "LO", "ALT", "BEAR", "SPEED", "DATE"))) |>
    mutate(across(.cols = !c(SbjNum), .fns = ~ as.character(.x)),
           intento_efectivo = intento_efectivo) |>
    relocate(intento_efectivo, .after = SbjNum) |>
    rename_with(~ gsub(pattern = as.character(intento_efectivo), replacement = "", x = .),
                .cols = everything())
}

#' Title
#'
#' @param nombre
#' @param extension
#' @param tolerancia
#'
#' @return
#' @export
#'
#' @examples
formato_archivo = function(nombre, extension, tolerancia = 10) {
  paste0(nombre,
         "_",
         gsub(pattern = "-", replacement = "", x = Sys.Date()),
         "_",
         format(Sys.time(), "%H"),
         as.character(floor(as.integer(format(Sys.time(), "%M")) / tolerancia) * tolerancia),
         "h.",
         extension)
}
#' Estandarizar texto para imitar resultados de la categorizacion con el bot
#'
#' @param texto
#' @param quitar_acentos
#' @param quitar_caracteres_especiales
#' @param minusculas
#'
#' @return
#' @export
#'
#' @examples
estandarizar_texto = function(texto, quitar_acentos = TRUE, quitar_caracteres_especiales = TRUE, minusculas = TRUE) {
  # variable de texto
  texto_estandar <- texto

  texto_estandar <- as.character(texto)

  if (is.na(texto_estandar)) {
    return(NA)
  }

  # Quitar acentos si es necesario
  if (quitar_acentos) {
    texto_estandar <- stringi::stri_trans_general(texto_estandar, "Latin-ASCII")
  }

  # Quitar caracteres especiales si es necesario
  if (quitar_caracteres_especiales) {
    texto_estandar <- stringr::str_replace_all(texto_estandar, "(?<!>)\\W+?(?!>)",
                                               function(x) {if (stringr::str_detect(x, ">>>")) return(">>>") else return(" ")
                                               })
  }
  # Quitar espacios extras
  texto_estandar <- stringr::str_trim(stringr::str_replace_all(texto_estandar, "\\s+", " "))

  # Convertir a minúsculas si es necesario
  if (minusculas) {
    texto_estandar <- tolower(texto_estandar)
  }

  return(texto_estandar)
}
#' Title
#'
#' @param folder
#' @param prefijo
#' @param bd_categorias_raw
#' @param variable
#'
#' @return
#' @export
#'
#' @examples
generarGlosario_preguntaAbierta = function(folder = "./data/", prefijo = "glosario_", bd_categorias_raw, variable) {
  if(!file.exists(folder)) {
    dir.create(folder)
  }
  bd_categorias_raw |>
    select(!!rlang::sym(variable)) |>
    na.omit() |>
    rowwise() |>
    mutate(categoria = estandarizar_texto(!!rlang::sym(variable)),
           categoria_corregida = !!rlang::sym(variable)) %>%
    ungroup() %>%
    openxlsx2::write_xlsx(x = .,
                          file = paste0(folder,
                                        prefijo,
                                        variable,
                                        ".xlsx"))
}
