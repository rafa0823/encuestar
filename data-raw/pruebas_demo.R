

diseno <- survey::svydesign(ids = ~1, data = datos_demo)

gr <- Grafica$new(diseno = diseno, diccionario = cuestionario_demo, tema = tema_default)

gr$gauge_numerica(codigo = evaluacion_amlo, color = "blue", escala = c(0, 9), size_text_pct = 18)

gr$gauge_categorica(codigo = persona_lenguaindigena, filtro = "respuesta == 'Sí'", color = "red", escala = c(0, 1), size_text_pct = 18)

gr$barras_categorica(codigo = lengua_indigena, salto = 20, porcentajes_fuera = F)



gr$barras_aspectos(codigo = opinion, aspectos = c("era", "sasil"), filtro = "respuesta == 'Buena'")

gr$barras_aspectos(codigo = opinion, aspectos = c("era", "sasil"), filtro = "respuesta == 'Buena'")

gr$barras_aspectos(codigo = evaluacion, aspectos = c("amlo", "rutilio"))

gr$barras_aspectos(patron = opinion, aspectos = c("era", "sasil"), filtro = "respuesta == 'Buena'")

gr$intervalo_numerica(patron = evaluacion, aspectos = c("amlo", "rutilio"))



# Barras numerica ---------------------------------------------------------

gr <- Grafica$new(diseno = diseno, diccionario = cuestionario_demo, tema = tema_default)

gr$barras_numerica(patron_inicial = conocimiento, aspectos = c("era", "sasil"), filtro = "respuesta == 'Sí'",
                   salto = 20, porcentajes_fuera = F)


# Sankey ------------------------------------------------------------------

gr <- Grafica$new(diseno = diseno, diccionario = cuestionario_demo, tema = tema_default)

gr$sankey_categorica(variables = c("sexo", "conocimiento_manuelita"))


# Nube de texto -----------------------------------------------------------

gr <- Grafica$new(diseno = diseno, diccionario = cuestionario_demo, tema = tema_default)

gr$nube_texto(codigo = "problema_masimportante_otro",
              palabrasVacias = c("muy", "anteriores", "mas", "todas", "falta"),
              total_palabras = 15)


# Barras multirespuesta ---------------------------------------------------

diseno$variables |>
  as_tibble() |>
  glimpse()

gr <- Grafica$new(diseno = diseno, diccionario = cuestionario_demo, tema = tema_default)

gr$barras_multirespuesta()

# Candiadato opinion ------------------------------------------------------

gr <- Grafica$new(diseno = diseno, diccionario = cuestionario_demo, tema = tema_default)

gr$candidato_opinion(patron_inicial = opinion,
                     aspectos = c("era", "sasil"), ns_nc = "Ns/Nc (No leer)", regular = "Regular (No leer)",
                     grupo_positivo= c("Buena"), grupo_negativo = c("Mala"),
                     orden_resp = c("Mala", "Regular (No leer)", "Buena"))


c("Mala" = "red", "Regular" = "yellow", "Buena" = "green", "Ns/Nc" = "gray70", "Burbuja" = "blue")

# Candidato partido -------------------------------------------------------

diseno <- survey::svydesign(ids = ~1, data = datos_demo)

nombres_candidatos <- cuestionario_demo %>%
  filter(grepl(pattern = "Bloque 5:", x = bloque)) |>
  filter(grepl("partido_",llaves)) %>%
  pull(tema)

color_morena <- "#A6032F"
color_morena_complemento <- "#F5CD5F"
color_panal <- "#03A6A6"
color_pt <- "#D91136"
color_pri <- "#038C33"
color_mc <- "#F27405"
color_pvem <- "#98BF5E"
color_pan <- "#0339a6"
color_prd <- "#F2B705"

color_pencsolchis <- "#AE95BF"
color_chisunido <- "#0396A6"
color_rspchis <-"#D9526B"
color_pmchis <- "#6B3A8C"

color_otro <- "gray30"
color_ninguno <- "black"
color_nsnc <- "gray60"

gr <- Grafica$new(diseno = diseno, diccionario = cuestionario_demo, tema = tema_default)

gr$candidatoPartido(llave_partido = "partido", llave_conocimiento = "conocimiento", respuesta_conoce = "Sí",
                     candidatos = c("era", "sasil"), corte_otro = 0.03, cliente = c("era", "sasil"),
                     colores_candidatos = rep("#5B0A1C", 6) %>% purrr::set_names(nombres_candidatos),
                     colores_partido = c("PAN" = color_pan,
                                         "PRI" = color_pri,
                                         "PRD" = color_prd,
                                         "PT" = color_pt,
                                         "Partido Verde (PVEM)" = color_pvem,
                                         "Movimiento Ciudadano (MC)" = color_mc,
                                         "MORENA" = color_morena,
                                         "Chiapas Unido" = color_chisunido,
                                         "Podemos Mover a Chiapas" = color_pmchis,
                                         "Partido Encuentro Solidario Chiapas" = color_pencsolchis,
                                         "Redes Sociales Progresistas Chiapas" = color_rspchis,
                                         "Candidato independiente" = color_otro,
                                         "Ninguno (No leer)" = color_ninguno,
                                         "Ns/Nc (No leer)" = color_nsnc,
                                         "Otro" = color_otro
                     ))

# Candidato saldo ---------------------------------------------------------

diseno <- survey::svydesign(ids = ~1, data = datos_demo)

gr <- Grafica$new(diseno = diseno, diccionario = cuestionario_demo, tema = tema_default)

gr$candidatoSaldo(llave_opinion = "opinion", candidatos = c("era", "sasil"),
                  positivos = c("Buena"), negativos = c("Mala"),
                  color_positivo = "orange", color_negativo = "brown")

# Método de MORENA --------------------------------------------------------

candidatos <- cuestionario_demo %>%
  filter(grepl("Bloque 5", bloque)) |>
  filter(grepl("opinion_",llaves)) |>
  pull(llaves) %>%
  gsub(pattern = "opinion_", replacement = "", x = .)

atributos <- tibble(atributo = c("honesto", "mujeres", "cercano", "estado", "cumple"),
                    puntos = c(1,.5,.25,.25,.25))

gr <- Grafica$new(diseno = diseno, diccionario = cuestionario_demo, tema = tema_default)

gr$metodo_morena(personajes = candidatos, atributos = atributos)

# Cruce puntos ------------------------------------------------------------

p3.1.1_personajes <- cuestionario_demo %>%
  filter(grepl(pattern = "Bloque 3", x = bloque)) |>
  filter(grepl(pattern = "conocimiento_", x = llaves)) |>
  pull(llaves)

gr <- Grafica$new(diseno = diseno, diccionario = cuestionario_demo, tema = tema_default)

gr$puntos(cruce = "sexo",
          variables = p3.1.1_personajes,
          vartype = "cv", valor_variables = "Sí")

# Brechas duales -------------------------------------------------------

gr <- Grafica$new(diseno = diseno, diccionario = cuestionario_demo, tema = tema_default)

gr$brechasDuales(var1 = "AMAI_factor",
                 var2_filtro = "candidato_preferencia",
                 filtro = c("Carlos Morales", "Sasil de León", "“Doctor” Pepe Cruz", "Eduardo Ramírez Aguilar"),
                 vartype = "cv",
                 line_rich = F,
                 line_linewidth = 2, line_hjust = 0.5, line_vjust = -0.5)

# Brechas multiples -------------------------------------------------------

gr <- Grafica$new(diseno = diseno, diccionario = cuestionario_demo, tema = tema_default)

gr$brechasMultiples(por_grupo = "AMAI_factor",
                    variables=c("conocimiento_manuelita",
                                "conocimiento_carlos",
                                "conocimiento_era",
                                "conocimiento_sasil"),
                    valor_variables = "Sí", vartype = "cv", line_rich = F,
                    line_linewidth = 2, line_hjust = "ymax", line_vjust = -0.5)

# Cruce barras ------------------------------------------------------------

gr <- Grafica$new(diseno = diseno, diccionario = cuestionario_demo, tema = tema_default)

gr$barrasMultiples(por_grupo = "AMAI_factor", variables = c("conocimiento_manuelita",
                                                   "conocimiento_carlos",
                                                   "conocimiento_era",
                                                   "conocimiento_sasil"), valor_variables = "Sí", vartype = "cv", color = "red")

# Cruce bloques -----------------------------------------------------------

gr <- Grafica$new(diseno = diseno, diccionario = cuestionario_demo, tema = tema_default)

gr$bloques(cruce = "seg_voto", filter = c("Ns/Nc", NA),
           variable = "voto_candidato",
           vartype = "cv")


# Correspondencia ---------------------------------------------------------

gr <- Grafica$new(diseno = diseno, diccionario = cuestionario_demo, tema = tema_default)

gr$correspondencia(var1 = "sexo", var2 = "preferencia_candidatopresidencia", legenda1 = "a", legenda2 = "b", colores = c("red", "blue"))

# Componentes principales -------------------------------------------------

gr <- Grafica$new(diseno = diseno, diccionario = cuestionario_demo, tema = tema_default)

gr$componentesPrincipales(variables = c("sexo", "conocimiento_era"))

# Blackbox ----------------------------------------------------------------

gr <- Grafica$new(diseno = diseno, diccionario = cuestionario_demo, tema = tema_default)

gr$blackBox(vars = "sexo", stimuli = "edad")

# REGIONES ----------------------------------------------------------------

diseno <- survey::svydesign(ids = ~1, data = datos_demo |> mutate(region = sample(x = seq.int(1:6), size  = n(), replace = T)))

Preguntas <- Pregunta2$new(encuesta = encuesta_demo, tema = tema_default)
# MAPA GANADOR
Preguntas$Regiones$mapa_ganador(variable = "voto_partido", lugar = 1)

# MAPA degradado
Preguntas$Regiones$mapa_degradadoNumerico(variable = "evaluacion_rutilio")

# Heatmap conocimiento
candidatos <- cuestionario_demo |>
  filter(grepl(pattern = "Bloque 3", x = bloque)) |>
  distinct(llaves) |>
  pull() |>
  gsub(pattern = "conocimiento_", replacement = "")

Preguntas$Regiones$heatmap_conocimiento(patron_llaveConocimiento = "conocimiento",
                                        candidatos = candidatos,
                                        respuesta = "Sí")

# heatmap saldoOpinion
Preguntas <- Pregunta2$new(encuesta = encuesta_demo, tema = tema_default)

candidatos <- cuestionario_demo |>
  filter(grepl(pattern = "Bloque 5", x = bloque)) |>
  filter(grepl(pattern = "opinion", x = llaves)) |>
  distinct(llaves) |>
  pull() |>
  gsub(pattern = "opinion_", replacement = "")

Preguntas$Regiones$heatmap_saldoOpinion(patron_llaveOpinion = "opinion",
                                        candidatos = candidatos,
                                        ns_nc = "Ns/Nc",
                                        cat_negativo = c("Muy mala", "Mala"),
                                        cat_regular = "Regular",
                                        cat_positivo = c("Buena", "Muy buena"))

