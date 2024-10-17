Opinometro <- R6::R6Class(classname = "Opinometro",
                          public = list(
                            id_cuestionarioOpinometro = NULL,
                            pool = NULL,
                            variables_cuestionario = NULL,
                            bd_respuestas_cuestionario = NULL,
                            diccionario = NULL,
                            initialize = function(id_cuestionarioOpinometro = NA,
                                                  pool = NA,
                                                  diccionario = NA) {
                              # self$inicar_conexion()
                              self$pool <- pool
                              self$id_cuestionarioOpinometro <- id_cuestionarioOpinometro
                              self$definir_variables()
                              self$construir_respuestas()
                              self$diccionario <- diccionario
                              self$verificar_variables()
                              self$terminar_conexion()
                            },
                            # inicar_conexion = function(){
                            #   private$pool
                            # },
                            definir_variables = function(){
                              self$variables_cuestionario <-
                                determinarVariables_cuestinoarioOpinometro(pool = self$pool,
                                                                           id_cuestionario = self$id_cuestionarioOpinometro)
                            },
                            construir_respuestas = function(){

                              bd_respuestas_opinometro_raw <-
                                consultar_respuestas(pool = self$pool,
                                                     codigos = self$variables_cuestionario,
                                                     encuesta_id = self$id_cuestionarioOpinometro)

                              self$bd_respuestas_cuestionario <-
                                bd_respuestas_opinometro_raw |>
                                rectificar_respuestasOpinometro(variables_cuestionario = self$variables_cuestionario) |>
                                left_join(bd_respuestas_opinometro_raw |>
                                            calcular_intentosEfectivos_opinometro(),
                                          by = "SbjNum")

                            },
                            verificar_variables = function(){
                              variables_sobrantes <-
                                self$bd_respuestas_cuestionario |>
                                select(all_of(self$variables_cuestionario)) |>
                                colnames() |>
                                as_tibble() |>
                                anti_join(self$diccionario |>
                                            pull(llaves) |>
                                            as_tibble(),
                                          by = "value")

                              variables_faltantes <-
                                self$diccionario |>
                                pull(llaves) |>
                                as_tibble() |>
                                anti_join(self$bd_respuestas_cuestionario |>
                                            select(all_of(self$variables_cuestionario)) |>
                                            colnames() |>
                                            as_tibble(),
                                          by = "value")
                              if(nrow(variables_sobrantes) != 0) {
                                print(glue::glue("Las siguientes variables están en el opinometro pero no estan en el diccionario: "))
                                print(variables_sobrantes |>
                                        rename(variable = value))
                              }

                              if(nrow(variables_faltantes) != 0) {
                                print(glue::glue("Las siguientes variables están en el diccionario pero no existen en los registros del opinometro:"))
                                print(variables_faltantes |>
                                        rename(variable = value))

                                crear_variables <-
                                  yesno::yesno2("¿Crear las variables faltantes a la base consultada?",
                                                yes = "Y",
                                                no = "N")
                                if(crear_variables){
                                  for(i in variables_faltantes$value) {
                                    self$bd_respuestas_cuestionario <-
                                      self$bd_respuestas_cuestionario |>
                                      mutate(!!rlang::sym(i) := NA_character_)
                                  }
                                }
                              }
                            },
                            terminar_conexion = function(){
                              pool::poolClose(pool = private$pool)
                            }
                          )
)

