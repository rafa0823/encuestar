eliminar_auditoria_telefonica <- function(encuesta){
  if(("SbjNum" %in% names(encuesta$base_encuesta))
     & ("SbjNum" %in% names(auditoria_telefonica))){
    # Se eliminan por no pasar la auditoria telefónica
    encuesta$base <- encuesta$base %>%
      anti_join(encuesta$auditoria_telefonica, by="SbjNum")

  }
  else print("Identificador SbjNum no presente en alguna de las bases")
  return(encuesta)
}
