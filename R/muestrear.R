Muestra <- R6::R6Class(
  classname = "Muestra",
  public = list(
    lista_nominal = NULL,
    shp_mun = NULL,
    shp_mza = NULL,
    shp_loc = NULL,
    initialize = function(lista_nominal = lista_nominal,
                          shp_mun = shp_mun,
                          shp_mza = shp_mza,
                          shp_loc = shp_loc,
                          Marco_Muestral){
      self$lista_nominal <- lista_nominal
      self$shp_mun <- shp_mun
      self$shp_mza <- shp_mza
      self$shp_loc <- shp_loc
      self$Marco_Muestral <- "A"
    }
  )#,
  # private = list(
  #   Marco_Muestral = NULL,
  #   asignar_marco = function(){
  #     private$Marco_Muestral <- "cdscs"
  #   }
  # )
)
