## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(dplyr)

## ----dicc_ejemp_1, echo = FALSE-----------------------------------------------
ejemp_dic <- readxl::read_excel('./Manual-diccionario/diccionario_ejemp_gen.xlsx')|>
  mutate(respuestas = ifelse(is.na(respuestas),'',respuestas),
         tema = ifelse(is.na(tema),'',tema))


ejemp_dic_1 <- ejemp_dic |>
  filter(llaves %in% c('edad','sexo','colonia_bien','colonia_mal','voto_pr_24','afirmacion_seguridad'))

knitr::kable(ejemp_dic_1, caption = 'Tabla 1. Ejemplo de un diccionario', align = 'c')


## ----dicc_ejemp_2,echo=FALSE--------------------------------------------------

ejemp_dic_2<- ejemp_dic |>
  select(tipo_pregunta,llaves,respuestas)|>
  filter(llaves %in% c('edad','sexo','colonia_bien','voto_pr_24','afirmacion_seguridad'))

knitr::kable(ejemp_dic_2,caption = 'Tabla 2. Estructura básica del diccionario de datos', align = 'c')


## ----dicc_ejemp_3,echo=FALSE--------------------------------------------------
ejemp_dic_3<- ejemp_dic |>
  select(bloque,llaves, pregunta,tema)|>
  filter(llaves %in% c('edad','sexo','colonia_bien','voto_pr_24','afirmacion_seguridad'))

knitr::kable(ejemp_dic_3, caption = 'Tabla 3. Estructura de las variables/columnas de apoyo en el diccionario', align = 'c')

## ----dicc_ejemp_4,echo=FALSE--------------------------------------------------
ejemp_dic_4<- ejemp_dic[c(5:13),]

knitr::kable(ejemp_dic_4,caption = 'Tabla 4. Llaves con diferentes niveles de gobierno', align = 'c')

## ----sufi_gob,echo=FALSE------------------------------------------------------
sufij_gob <- readxl::read_excel('./Manual-diccionario/niveles_gob.xlsx')

knitr::kable(sufij_gob,caption = 'Tabla 5. Sufijos para los diferentes niveles de gobierno', align = 'c')

## ----dicc_ejemp_5,echo=FALSE--------------------------------------------------
ejemp_dic_5 <-  ejemp_dic |>
  filter(llaves %in% c('voto_herm_pm_24','voto_can_pm_24','afirmacion_mex',
                       'afirmacion_son','accion_principal_son','accion_principal_mex'))

knitr::kable(ejemp_dic_5, caption = 'Tabla 6. Ejemplo de uso de lugares para nombramiento de las llaves', align = 'c')

## ----ejemp_lug,echo=FALSE-----------------------------------------------------
ejemp_lug <- readxl::read_excel('./Manual-diccionario/ejem_lugs.xlsx')

knitr::kable(ejemp_lug,caption = 'Tabla 7. Casos en controversia en la asignación de sufijos para lugares',align = 'c')

## ----dicc_ejemp_5_1,echo=FALSE------------------------------------------------
ejemp_dic_5_1 <-  ejemp_dic|>
  filter(llaves %in% c('voto_herm_pm_24','voto_can_pm_24','accion_principal_son','accion_principal_mex'))

knitr::kable(ejemp_dic_5_1,align = 'c')

## ----dicc_ejemp_6,echo=FALSE--------------------------------------------------
ejemp_dic_6 <-  ejemp_dic |>
  filter(llaves %in% c('aprueba_amlo'))

knitr::kable(ejemp_dic_6,caption = 'Tabla 8. Uso de nombres/apodos para referenciar personajes', align = 'c')

