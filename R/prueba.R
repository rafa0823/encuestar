# #
# #
# # # Crear objeto-------------------------------------------------------------------------
#
#
# # # Leer base
# base_encuesta <- readr::read_csv(file = "datar/bd.csv")
# auditoria_telefonica <- readr::read_csv(file = "datar/eliminar.csv")
# info <- sf::st_read("~/Downloads/shp/mza_select9.shp") %>% tibble::as_tibble()
#
# encuesta <- Encuesta$new(respuestas = base_encuesta,
#                          auditoria_telefonica = auditoria_telefonica,
#                          muestra = info
#                          )
#
#
# # Leer cuestionario -------------------------------------------------------
cuestionario <- officer::read_docx("~/Downloads/Morant Consultores plantilla.docx")
docx_summary(cuestionario) %>%
  filter(!style_name %in% c("Morant_título","Morant_texto")) %>%
  mutate(bloque=ifelse(style_name=="Morant_Bloque", text, NA)) %>%
  fill(bloque,.direction = c("down")) %>%
  filter(!style_name=="Morant_Bloque") %>%
  mutate(pregunta=ifelse(style_name=="Morant_Pregunta", text, NA)) %>%
  fill(pregunta,.direction = c("down")) %>%
  filter(!style_name=="Morant_Pregunta")
#
# # # Leer diccionario
# # diccionario <- read_csv(file = "datar/codigos.csv")
# # # Diseño
# # var_diseño <- diccionario %>% filter(tipo=="diseño") %>% arrange(orden) %>% pull(codigo)
# # diseño <- rlang::new_formula(lhs = NULL,
# #                              rhs = rlang::parse_expr(stringr::str_c(var_diseño,
# #                                                                     collapse = " + ")))
# # diseño <- survey::svydesign(ids = diseño,
# #                             data = base_encuesta %>%
# #                               filter(!is.na(municipio),
# #                                      !is.na(localidad),
# #                                      !is.na(seccion),
# #                                      !is.na(manzana)))
# # # Leer base de datos auditoría telefónica
# # auditoria_telefonica <- read_csv(file = "datar/eliminar.csv")
# # # Diseño
# #
# #
# # encuesta <- list(base_encuesta=base_encuesta,
# #                  diccionario=diccionario,
# #                  auditoria_telefonica=auditoria_telefonica,
# #                  diseño=diseño,
# #                  calibracion=calibracion)
# #
# # # Limpiar -----------------------------------------------------------------
# #
# # # Eliminar
# # encuesta <- eliminar_auditoria_telefonica(encuesta)
# #
# #
# # # Analizar ----------------------------------------------------------------
# tabla <- analizar_frecuencias(encuesta , pregunta = P29)
# #
# #
# #
