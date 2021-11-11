

# Crear objeto-------------------------------------------------------------------------
# Leer base
base_encuesta <- read_csv(file = "datar/encuesta.csv")
# Leer diccionario
diccionario <- read_csv(file = "datar/codigos.csv")
# Diseño
var_diseño <- diccionario %>% filter(tipo=="diseño") %>% arrange(orden) %>% pull(codigo)
diseño <- rlang::new_formula(lhs = NULL,
                             rhs = rlang::parse_expr(stringr::str_c(var_diseño,
                                                                    collapse = " + ")))
diseño <- survey::svydesign(ids = diseño,
                            data = base_encuesta %>%
                              filter(!is.na(municipio),
                                     !is.na(localidad),
                                     !is.na(seccion),
                                     !is.na(manzana)))
# Leer base de datos auditoría telefónica
auditoria_telefonica <- read_csv(file = "datar/eliminar.csv")
# Diseño


encuesta <- list(base_encuesta=base_encuesta,
                 diccionario=diccionario,
                 auditoria_telefonica=auditoria_telefonica,
                 diseño=diseño,
                 calibracion=calibracion)

# Limpiar -----------------------------------------------------------------

# Eliminar
encuesta <- eliminar_auditoria_telefonica(encuesta)


# Analizar ----------------------------------------------------------------
tabla <- analizar_frecuencias(encuesta , pregunta = emocion)



