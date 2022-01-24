exportar_bd <- function(self, carpeta, agregar, quitar){
  compartir <-
    self$respuestas$base %>%
    # mutate(Date = Date - days(23)) %>%
    select(Fecha = Date, Latitud = Latitude,
           Longitud = Longitude,
           Municipio = Muni, Localidad = Loc,
           # PA:P25,
           any_of(na.omit(self$cuestionario$diccionario$llaves)),
           any_of(agregar)) %>%
    select(-any_of(quitar))

  eliminar <-
    names(self$respuestas$base)[is.na(match(names(self$respuestas$base),
                                            names(compartir)))]

  continuar <- yesno::yesno2(glue::glue("Desea eliminar las siguientes variables?: \n {paste(eliminar, collapse = ', ')}"),
                             yes = "Sí", no = "No")

  if(continuar){
    compartir %>% readr::write_excel_csv(glue::glue("{carpeta}/bd.csv"))

  } else{
    cat("Use el parámetro 'agregar' y haga un vector con las variables que desea agregar. O arregle las llaves del cuestionario")
  }
}
