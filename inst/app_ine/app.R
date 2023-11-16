library(shiny)
library(leaflet)
library(readr)
library(purrr)
library(tibble)
library(sf)
library(stringr)
library(dplyr)
library(tidyr)
library(muestreaR)
library(glue)
library(ggplot2)
library(shinyWidgets)
library(shinydashboard)
library(DT)
library(shinycssloaders)
library(colorRamps)
library(gt)
library(shinyjs)
library(encuestar)
library(highcharter)

options(survey.lonely.psu ="remove")
preguntas <- read_rds("data/clase_pregunta.rda")
diseno <- preguntas$encuesta$muestra$muestra
shp <- preguntas$encuesta$shp_completo
bd <- preguntas$encuesta$respuestas$base
enc_shp <- readr::read_rds("data/enc_shp.rda") %>%
  mutate(interior_cluster = dplyr::if_else(condition = (as.numeric(distancia) != 0),
                                           true = "Fuera",
                                           false = "Dentro"))
eliminadas <- preguntas$encuesta$respuestas$eliminadas
eliminadas_shp <- eliminadas %>% filter(!is.na(Longitude)) %>% st_as_sf(coords = c("Longitude", "Latitude"),crs = 4326)

corregidas_shp <- preguntas$encuesta$respuestas$cluster_corregido %>% st_as_sf(coords = c("Longitude", "Latitude"),crs = 4326)
mapa_base <- read_rds("data/mapa_base.rda")
bbox_qro <- st_bbox(shp$shp$MUN)

Sys.setlocale(locale = "es_ES.UTF-8")

# equipos <- readr::read_rds("data/clusters_por_equipo")

# funciones ---------------------------------------------------------------

unidades_app <- function(diseno, u_nivel) {
  unidades <- u_nivel %>% pull(variable)
  u_nivel_tipo <- u_nivel %>% transmute(paste(tipo,nivel,sep = "_")) %>% pull(1)

  aulr <- shp$shp[[unidades]] %>%
    inner_join(diseno$muestra[[diseno$ultimo_nivel]] %>%
                 unnest(data) %>%
                 distinct(!!rlang::sym(unidades), !!rlang::sym(u_nivel_tipo)))
}

entrevistas <- function(diseno, enc, u_nivel, u_nivel_tipo){
  cortes <- diseno$cuotas %>% count(parse_number(rango)-1) %>% pull(1) %>% append(Inf)
  texto_cortes <- diseno$cuotas %>% distinct(rango) %>% pull(1)

  hecho <- enc %>%
    mutate(edad = as.character(cut(as.integer(edad),cortes,
                                   texto_cortes)),
           sexo = if_else(sexo == "F", "Mujer", "Hombre"),
           cluster = as.numeric(!!rlang::sym(u_nivel$variable))) %>%
    count(cluster, edad, sexo, name = "hecho") %>%
    full_join(
      diseno$cuotas %>% mutate(sexo = if_else(sexo == "F", "Mujer", "Hombre")) %>%
        rename(cuota = n, cluster = !!rlang::sym(u_nivel_tipo), edad = rango)
    ) %>% replace_na(list(hecho = 0, faltan = 0)) %>%
    mutate(faltan = cuota - hecho) %>% filter(cluster %in% diseno$cuotas[[u_nivel_tipo]])

  por_hacer <- diseno$cuotas %>% mutate(sexo = if_else(sexo == "F", "Mujer", "Hombre")) %>%
    rename(cuota = n, cluster = u_nivel_tipo, edad = rango) %>%
    left_join(
      enc %>%
        mutate(edad = as.character(cut(as.integer(edad),cortes,
                                       texto_cortes)),
               sexo = if_else(sexo == "F", "Mujer", "Hombre"),
               cluster = as.numeric(!!rlang::sym(u_nivel$variable))) %>%
        count(cluster, edad, sexo, name = "hecho")
    ) %>% replace_na(list(hecho = 0)) %>% mutate(por_hacer = cuota-hecho,
                                                 por_hacer2 = if_else(por_hacer < 0, 0, por_hacer)
    )
  return(list(hecho = hecho, por_hacer = por_hacer))
}

graficar_entrevistas <- function(bd_efectivas, bd_eliminadas, bd_corregidas){

  bd_hechas <- bd_efectivas %>% as_tibble %>%
    count(fecha = lubridate::floor_date(Date, "day")) %>%
    rename("Hechas" = n)

  bd_eliminadas <- bd_eliminadas %>% as_tibble %>%
    count(fecha = lubridate::floor_date(Date, "day")) %>%
    rename("Eliminadas" = n)

  bd_corregidas <- bd_corregidas %>% as_tibble %>%
    count(fecha = lubridate::floor_date(Date, "day")) %>%
    rename("Corregidas" = n)

  bd_plot <- bd_hechas %>%
    left_join(bd_eliminadas, by = "fecha") %>%
    left_join(bd_corregidas, by = "fecha") %>%
    tidyr::pivot_longer(cols = !fecha, names_to = "tipo", values_to = "n") %>%
    mutate(color = case_when(tipo == "Hechas" ~ "#0EEB79",
                             tipo == "Eliminadas" ~ "red",
                             tipo == "Corregidas" ~ "orange"))

  g <- bd_plot %>%
    ggplot(aes(x = fecha, y = n, color = color)) +
    geom_point() +
    geom_line() +
    ggrepel::geom_text_repel(aes(label = n), show.legend = F, size = 8) +
    scale_color_identity(labels = c("#0EEB79" = "Hechas", "red" = "Eliminadas", "orange" = "Corregidas"),
                         guide = "legend") +
    scale_x_datetime(date_breaks = "1 day", date_labels = "%d %b") +
    labs(x = NULL, y = "Entrevistas") +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 16),
          axis.text = element_text(size = 14))

  return(g)

}

graficar_barras <- function(bd, color){

  g <- bd %>%
    ggplot(aes(x = reorder(str_to_title(Srvyr), n), y = n)) +
    ggchicklet::geom_chicklet(fill = color) +
    ggfittext::geom_bar_text(show.legend = F, contrast = T, size = 14) +
    scale_y_continuous(breaks = NULL) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "none", axis.text = element_text(size = 14))

  return(g)
}

# Parámetros --------------------------------------------------------------

PRINCIPAL <- "#A6032F"
gray70 <- "#B3B3B3"

# vars necesarios para app ------------------------------------------------

u_nivel <- diseno$niveles %>% filter(nivel == diseno$ultimo_nivel)
u_nivel_tipo <- u_nivel %>% transmute(paste(tipo,nivel,sep = "_")) %>% pull(1)
aulr <- unidades_app(diseno, u_nivel)

# cuotas ------------------------------------------------------------------

n_entrevista <- entrevistas(diseno, bd, u_nivel, u_nivel_tipo)
hecho <- n_entrevista$hecho
por_hacer <- n_entrevista$por_hacer

faltan_shp <- aulr %>%
  left_join(hecho %>% count(!!rlang::sym(paste("cluster",u_nivel$nivel, sep = "_")) := cluster,
                            wt =  faltan))

# UI ----------------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = diseno$poblacion$nombre),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Mapa", tabName = "mapa", icon = icon("map")
      ),
      menuItem(
        "Entrevistas", tabName = "entrevistas", icon = icon("poll")
      ),
      menuItem(
        "Encuestadores", tabName = "encuestadores", icon = icon("users")
      ),
      menuItem(
        "Auditoría", tabName = "auditoria", icon = icon("search")
      )
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(
        tabName = "mapa",
        fluidPage(
          title = "Mapa",
          fluidRow(
            column(
              width = 12,
              leafletOutput(outputId = "mapa_principal", height = 850),
              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                            width = 330, height = "auto",
                            HTML(text = "<button data-toggle='collapse' data-target='#demo'>Menú (mostrar/ocultar)</button>"),
                            tags$div(id = 'demo',
                                     class = "collapse",
                                     dateRangeInput(inputId = "mapa_fecha_input",
                                                    label = h3("Rango de fechas"),
                                                    language = "es",
                                                    separator = "a",
                                                    format = "MM-dd",
                                                    start = lubridate::as_date(min(enc_shp |> as_tibble() |> distinct(Date) |> pull())),
                                                    end = lubridate::as_date(max(enc_shp |> as_tibble() |> distinct(Date) |> pull())),
                                                    min = lubridate::as_date(min(enc_shp |> as_tibble() |> distinct(Date) |> pull())),
                                                    max = lubridate::as_date(max(enc_shp |> as_tibble() |> distinct(Date) |> pull()))),
                                     actionButton(inputId = "filtrar_fechas", label = "Filtrar fechas"),
                                     selectInput(inputId = "cluster", label = h3("Cluster"), choices = c("Seleccione..." = "", sort(unique(diseno$muestra[[diseno$ultimo_nivel]][[u_nivel_tipo]])))),
                                     fluidRow(
                                       column(
                                         width = 6,
                                         h3("Ubicación")
                                       )
                                     ),
                                     fluidRow(
                                       column(
                                         width = 12,
                                         textInput(inputId = "coord_input", label = h4("Coordenadas"), value = "", width = "75%"),
                                       )
                                     ),
                                     actionButton(inputId = "filtrar", label = "Buscar"),
                                     gt_output(outputId = "faltantes"),
                                     hr(),
                                     actionButton("regresar", "Regresar")
                            )
              )
            )
          )
        )
      ),
      tabItem("entrevistas",
              h2("Total de entrevistas realizadas"),
              fluidRow(
                column(12,
                       progressBar(id = "enc_hechas", value = nrow(bd), display_pct = T, striped = T,
                                   total = (diseno$niveles %>% filter(nivel == 0) %>% pull(unidades))*diseno$n_0,
                                   status = "success"
                       )
                )
              ),
              h2("Avance por región"),
              fluidRow(
                column(width = 3, offset = 9,
                       downloadButton(outputId = "descargar_region", "Descargar resumen por region")
                )
              ),
              hr(),
              fluidRow(
                column(width = 12,
                       shinycssloaders::withSpinner((plotOutput(outputId = "avance_region", height = 600)))
                )
              ),
              hr(),
              fluidRow(
                column(width = 3,
                       selectInput(inputId = "municipio", "Municipio",
                                   choices = c("Todos", sort(unique(preguntas$encuesta$muestra$muestra$cuotas$Municipio))), selected = "Todos")
                )
              ),
              h2("Histórico de entrevistas"),
              fluidRow(
                column(12,
                       shinycssloaders::withSpinner(plotOutput("hechas", height = 400))
                )
              ),
              hr(),
              fluidRow(
                column(3,
                       valueBoxOutput(outputId = "hecho_totales", width = NULL)
                ),
                column(3,
                       valueBoxOutput(outputId = "faltantes_totales", width = NULL)
                ),
                column(3,
                       valueBoxOutput(outputId = "excedentes_totales", width = NULL)
                ),
                column(3,
                       valueBoxOutput(outputId = "eliminadas_totales", width = NULL)
                )
              ),
              h2("Balance de entrevistas"),
              fluidRow(
                column(6, withSpinner(plotOutput("por_hacer", height = 1200))),
                column(6, withSpinner(plotOutput("por_hacer_cuotas", height = 1200)))
              ),
              h2("Distribución sexo vs rango de edad"),
              fluidRow(
                column(6,
                       withSpinner(plotOutput("sexo")),
                ),
                column(6,
                       withSpinner(plotOutput("rango_edad"))
                )
              )
      ),
      tabItem("auditoria",
              fluidRow(
                column(6, selectInput(inputId = "vars", "Variable", choices = sort(preguntas$encuesta$auditar)))
              ),
              fluidRow(
                plotOutput("grafica", height = 600)
              )
      ),
      tabItem("encuestadores",
              tabsetPanel(type = "tabs",
                          tabPanel("General",
                                   h2("Estadísticas generales de los encuestadores"),
                                   fluidRow(
                                     column(width = 3,
                                            selectInput(inputId = "municipio_encuestadores", "Municipio",
                                                        choices = c("Todos", sort(unique(preguntas$encuesta$muestra$muestra$cuotas$Municipio))), selected = "Todos")
                                     )
                                   ),
                                   fluidRow(
                                     fluidRow(
                                       column(6,
                                              shinycssloaders::withSpinner(plotOutput("eliminadas_encuestador", height = 450))
                                       ),
                                       column(6,
                                              shinycssloaders::withSpinner(plotOutput("corregidas_encuestador", height = 450))
                                       )
                                     ),
                                     fluidRow(
                                       column(6,
                                              actionButton(inputId = "siguiente_eliminadas", label = "Siguiente")
                                       ),
                                       column(6,
                                              actionButton(inputId = "siguiente_corregidas", label = "Siguiente")
                                       )
                                     )
                                   ),
                                   hr(),
                                   fluidRow(
                                     fluidRow(
                                       column(6,
                                              shinycssloaders::withSpinner(plotOutput("prom_tiempo_encuestador", height = 400))
                                       ),
                                       column(6,
                                              shinycssloaders::withSpinner(plotOutput("duracion_entrevistas", height = 400))
                                       )
                                     ),
                                     fluidRow(
                                       column(6,
                                              actionButton(inputId = "siguiente_promedio", label = "Siguiente")
                                       )
                                     )
                                   ),
                                   hr(),
                                   fluidRow(
                                     column(6,
                                            shinycssloaders::withSpinner(plotOutput("razon_el", height = 400))
                                     ),
                                     column(6,
                                            shinycssloaders::withSpinner(DTOutput("eliminadas"))
                                     )
                                   )
                          ),
                          tabPanel("Individual",
                                   h2("Estadísticas particulares por encuestador"),
                                   fluidRow(
                                     column(width = 3,
                                            selectInput(inputId = "encuestador", "Encuestador",
                                                        choices = c("Seleccionar", sort(unique(bd$Srvyr))), selected = "Seleccionar")
                                     )
                                   ),
                                   fluidRow(
                                     column(width = 4,
                                            valueBoxOutput(outputId = "eliminadas_individual", width = NULL)
                                     ),
                                     column(width = 4,
                                            valueBoxOutput(outputId = "corregidas_individual", width = NULL)
                                     ),
                                     column(width = 4,
                                            valueBoxOutput(outputId = "efectivas_individual", width = NULL)
                                     )
                                   ),
                                   hr(),
                                   fluidRow(
                                     leafletOutput(outputId = "mapa_auditoria", height = 850),
                                     absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                                   draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                                   width = 330, height = "auto"
                                                   # HTML("<button data-toggle='collapse' data-target='#demo'>Min/max</button>"),
                                                   # tags$div(id = 'demo',  class="collapse",
                                                   #          selectInput("cluster", "Cluster", c("Seleccione..."= "",sort(unique(diseno$muestra[[diseno$ultimo_nivel]][[u_nivel_tipo]])))
                                                   #          ),
                                                   #          actionButton("filtrar","Filtrar"),
                                                   #          gt_output("faltantes"),
                                                   #          hr(),
                                                   #          actionButton("regresar", "Regresar")
                                                   # )
                                     )
                                   )
                          )
              )
      )
    )
  )
)

# SERVER ------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # Pestaña "Mapa" ----------------------------------------------------------

  entrevistas_efectivas <- reactive({
    # input$filtrar
    input$filtrar_fechas

    if(enc_shp %>% filter(as.numeric(distancia) != 0) %>% nrow() > 0){
      ent_c <- enc_shp %>%
        mutate(label = paste(!!rlang::sym(u_nivel$variable), Srvyr, SbjNum, sep= "-"),
               color = dplyr::if_else(condition = as.numeric(distancia) == 0,
                                      true = "#7BF739",
                                      false = "purple")
               ) %>%
        arrange(distancia)
    } else {
      ent_c <- enc_shp %>%
        mutate(label = paste(!!rlang::sym(u_nivel$variable), Srvyr, SbjNum, sep= "-"),
               color = "#7BF739")
    }
    shp_efectivas <- ent_c #|>
      # mutate(fecha = lubridate::as_date(Date)) |>
      # filter(lubridate::as_date(isolate(input$mapa_fecha_input[1])) <= fecha) |>
      # filter(fecha <= lubridate::as_date(isolate(input$mapa_fecha_input[2])))

    return(list(shp_efectivas))

  })

  output$mapa_principal <- renderLeaflet({

    nombres_region <- diseno$poblacion$marco_muestral |>
      distinct(region, strata_1) |>
      arrange(region) |>
      mutate(nombre_region = paste("Región ", strata_1, sep = ""))

    avance_clusters <- hecho |>
      group_by(cluster) |>
      summarise(across(.cols = c(hecho, cuota), .fns = ~ sum(.x, na.rm = T))) |>
      mutate(pct = hecho/cuota,
             cuartil = dplyr::case_when(pct <= 0.25 ~ "<=50%",
                                        0.5 < pct & pct <= 1.0 ~ "50% <= 100%",
                                        T ~ "Excedida"))

    faltan_shp <- faltan_shp |>
      left_join(avance_clusters, by = c("cluster_2" = "cluster"))

    pal_region <- leaflet::colorFactor(palette = topo.colors(n_distinct(nombres_region$nombre_region)), domain = unique(nombres_region$nombre_region))

    pal_efectivas <- leaflet::colorFactor(palette = c("#7BF739", "purple"), domain = c("Dentro", "Fuera"))

    pal_faltantes <- leaflet::colorFactor(palette = c("red", "green", "orange"), levels = c("<=50%", "50% <= 100%", "Excedida"), domain = faltan_shp$cuartil, ordered = T)

    map <- mapa_base %>%
      left_join(nombres_region |> select(strata_1, nombre_region), by = "strata_1") |>
      leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(color = ~pal_region(nombre_region), opacity = 1, fill = T, fillOpacity = 0.1) %>%
      addLegend(pal = pal_region, values = ~nombre_region, position = "bottomleft", title = "Región") %>%
      shp$graficar_mapa(bd = diseno$muestra, nivel = u_nivel %>% pull(variable)) %>%
<<<<<<< HEAD
      addPolygons(data = faltan_shp, fillColor = ~pal_n(n), fillOpacity = 1,stroke = F,
                  label = ~glue::glue("Encuestas faltantes: {n}"), group = "Encuestas faltantes") %>%
      addLegend(pal = pal_n, values = faltan_shp$n, title = "Encuestas faltantes",
                group = "Encuestas faltantes",position = "bottomleft") %>%
      addCircleMarkers(data = ent_c,
                       color = ~color, stroke = F,
                       label = ~label, group = "Entrevistas")

    if(nrow(eliminadas_shp) != 0) {

      map <- map |> addCircleMarkers(data = eliminadas_shp, stroke = F, color = "#FF715B", fillOpacity = 1,
                                     label = ~glue::glue("{SbjNum} - {Srvyr}"), group = "Eliminadas", clusterOptions = markerClusterOptions())

    }

    map <- map |>
      addCircleMarkers(data = corregidas_shp, stroke = F, color = "yellow", fillOpacity = 1,
                       popup = ~glue::glue("{SbjNum} - {Srvyr} - {Date} ≤<br> cluster reportado: {anterior} <br> cluster corregido: {nueva}"), group = "Cluster corregido", clusterOptions = markerClusterOptions()) %>%
      addLegend(position = "bottomright", colors = "green", labels = "Dentro de cluster")
=======
      addPolygons(data = faltan_shp,
                  fillColor = ~ pal_faltantes(cuartil),
                  fillOpacity = 1,
                  stroke = F,
                  label = ~glue::glue("Cuota cubierta: {scales::percent(pct, accuracy = 1.)} Entrevistas faltantes: {n}"),
                  group = "Encuestas faltantes") %>%
      addLegend(pal = pal_faltantes,
                values = faltan_shp$cuartil,
                title = "Cuota cubierta",
                group = "Encuestas faltantes",
                position = "bottomleft") %>%
      addCircleMarkers(data = entrevistas_efectivas()[[1]],
                       color = ~color,
                       stroke = F,
                       label = ~label,
                       group = "Entrevistas") %>%
      addLegend(position = "bottomleft",
                pal = pal_efectivas,
                values = entrevistas_efectivas()[[1]]$interior_cluster,
                na.label = "Indefinido",
                title = "Dentro de cluster") %>%
      addCircleMarkers(data = corregidas_shp,
                       stroke = F,
                       color = "yellow",
                       fillOpacity = 1,
                       popup = ~glue::glue("{SbjNum} - {Srvyr} - {Date} ≤<br> cluster reportado: {anterior} <br> cluster corregido: {nueva}"),
                       group = "Cluster corregido",
                       clusterOptions = markerClusterOptions())
>>>>>>> dev

    if (nrow(eliminadas_shp) > 0) {

      map <- map |>
        addCircleMarkers(data = eliminadas_shp,
                         stroke = F,
                         color = "#FF715B",
                         fillOpacity = 1,
                         label = ~glue::glue("{SbjNum} - {Srvyr}"),
                         group = "Eliminadas",
                         clusterOptions = markerClusterOptions())
    }

    map <- map %>%
      addLayersControl(baseGroups = c("Entrevistas", "Eliminadas", "Cluster corregido"),
                       overlayGroups = c("Encuestas faltantes"),
                       options = layersControlOptions(), position = "topleft") %>%
      hideGroup("Encuestas faltantes")

    return(map)

  })

  cluster_actual <- reactiveVal(value = "")

  proxy_mapa_principal <- leafletProxy("mapa_principal")

  observeEvent(input$filtrar, {

    if(input$cluster != cluster_actual()) {

      if(input$cluster != "") {

        if(!(is.na(coordenadas()[1]$latitud) & is.na(coordenadas()[2]$longitud))) {

          bbox <- aulr %>%
            filter(!!rlang::sym(u_nivel_tipo) == !!input$cluster) %>%
            sf::st_bbox()

          proxy_mapa_principal %>%
            flyToBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]]) %>%
            addMarkers(lat = coordenadas()[1]$latitud, lng = coordenadas()[2]$longitud)

          cluster_actual(input$cluster)

        } else {

          bbox <- aulr %>%
            filter(!!rlang::sym(u_nivel_tipo) == !!input$cluster) %>%
            sf::st_bbox()

          proxy_mapa_principal %>%
            flyToBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]])

          cluster_actual(input$cluster)

        }

      } else {

        if(!(is.na(coordenadas()[1]$latitud) & is.na(coordenadas()[2]$longitud))) {

          proxy_mapa_principal %>%
            flyToBounds(bbox_qro[[1]], bbox_qro[[2]], bbox_qro[[3]], bbox_qro[[4]]) %>%
            addMarkers(lat = coordenadas()[1]$latitud, lng = coordenadas()[2]$longitud)

          cluster_actual(input$cluster)

        } else {

          proxy_mapa_principal %>%
            flyToBounds(bbox_qro[[1]], bbox_qro[[2]], bbox_qro[[3]], bbox_qro[[4]])

          cluster_actual(input$cluster)

        }

      }

    } else {

      if(input$cluster != "") {

        if(!(is.na(coordenadas()[1]$latitud) & is.na(coordenadas()[2]$longitud))) {

          proxy_mapa_principal %>%
            addMarkers(lat = coordenadas()[1]$latitud, lng = coordenadas()[2]$longitud)

          cluster_actual(input$cluster)


        } else {

          proxy_mapa_principal

          cluster_actual(input$cluster)

        }

      } else {

        if(!(is.na(coordenadas()[1]$latitud) & is.na(coordenadas()[2]$longitud))) {

          proxy_mapa_principal %>%
            addMarkers(lat = coordenadas()[1]$latitud, lng = coordenadas()[2]$longitud)

          cluster_actual(input$cluster)


        } else {

          proxy_mapa_principal

          cluster_actual(input$cluster)

        }

      }

    }

  })

  observeEvent(input$regresar, {
    proxy_mapa_principal %>%
      flyToBounds(bbox_qro[[1]],bbox_qro[[2]],bbox_qro[[3]],bbox_qro[[4]])
  })

  balance_cluster <- reactive({
    input$filtrar

    balance_cluster <- hecho %>%
      filter(cluster == !!isolate(input$cluster)) |>
      group_by(cluster) |>
      summarise(across(.cols = c(cuota, hecho, faltan), .fns = ~ sum(.x)))

    hecho %>%
      filter(cluster == !!isolate(input$cluster)) %>%
      select(sexo, edad, faltan) %>%
      mutate(edad = gsub(pattern = "([0-9])([A-Z])([0-9])", replacement = "\\1 a \\3", x = edad),
             edad = gsub(pattern = "Y", replacement = " y ", x = edad)) |>
      tidyr::pivot_wider(names_from = sexo, values_from = faltan) %>%
      tidyr::replace_na(list(Mujer = 0, Hombre = 0)) %>%
      rename(Edad = edad)
  })

  output$faltantes <- render_gt({
    req(nrow(balance_cluster()) > 0)
    balance_cluster() %>%
      gt() %>%
      tab_header(title = md(glue::glue("**Cluster {isolate(input$cluster)}**")),
                 subtitle = glue::glue("Cuota: {balance_cluster()$cuota}   Hecho: {balance_cluster()$hecho}   Faltan: {balance_cluster()$faltan}")) |>
      tab_spanner(label = "Entrevistas faltanes por edad y sexo", columns = c(Edad, Hombre, Mujer))
  })

  coordenadas <- reactive({
    input$filtrar

    coord <- tidyr::separate(data = tibble(coord = isolate(input$coord_input)), col = coord, into = c("lat", "lon"), sep = ", ")

    list(latitud = as.double(coord$lat),
         longitud = as.double(coord$lon))
  })

  # Pestaña "Entrevistas" ---------------------------------------------------

  output$tot_hechas <- renderPlot({

    bd_plot <- tibble("hechas" = bd |>  nrow(),
                      "totales" = (diseno$niveles  |> filter(nivel == 0) |> pull(unidades))*diseno$n_0) |>
      pivot_longer(cols = everything(), names_to = "tipo", values_to = "n") |>
      mutate(control = "Entrevistas")

    g <- bd_plot |>
      ggplot(aes(x = control, y = n, fill = reorder(tipo, -n))) +
      geom_col() +
      ggfittext::geom_bar_text(aes(label = n), position = "stack", contrast = T, size = 16) +
      coord_flip() +
      theme_minimal() +
      labs(title = "Total de entrevistas realizadas") +
      scale_fill_manual(values = c("hechas" = "blue", "totales" = "red"),
                        labels = c("hechas" = "Hechas", "totales" = "Totales"),
                        name = "") +
      theme(axis.title.x = element_blank(), legend.position = "bottom",
            axis.title.y = element_blank(), text = element_text(size = 16))

    return(g)

  })

  efectivas_filter <- eventReactive(c(bd, input$municipio),{

    bd %>%
      {
        if(input$municipio != "Todos"){
          filter(., MUNI == input$municipio)
        } else{
          .
        }}
  })

  eliminadas_filter <- eventReactive(c(eliminadas, input$municipio),{

    eliminadas %>%
      {
        if(input$municipio != "Todos"){
          filter(., MUNI == input$municipio)
        } else{
          .
        }}
  })

  corregidas_filter <- eventReactive(c(corregidas_shp, input$municipio),{

    corregidas_shp %>% as_tibble %>%
      left_join(bd %>% distinct(SbjNum, MUNI), by = "SbjNum") %>%
      {
        if(input$municipio != "Todos"){
          filter(., MUNI == input$municipio)
        } else{
          .
        }}
  })

  por_hacer_filter <- eventReactive(c(por_hacer, input$municipio),{

    por_hacer %>%
      {
        if(input$municipio != "Todos"){
          filter(., Municipio == input$municipio)
        } else{
          .
        }}
  })

  hecho_filter <- eventReactive(c(hecho, input$municipio),{

    hecho %>%
      {
        if(input$municipio != "Todos"){
          filter(., Municipio == input$municipio)
        } else{
          .
        }}
  })

  output$hechas <- renderPlot({

    graficar_entrevistas(bd_efectivas = efectivas_filter(), bd_eliminadas = eliminadas_filter(), bd_corregidas = corregidas_filter())

  })

  output$hecho_totales <- renderValueBox({

    res <- hecho_filter() %>%
      summarise(hecho = sum(hecho)) |>
      pull() |>
      scales::comma()

    valueBox(value = res, subtitle = glue::glue("Entrevistas efectivas en total"), color = "green")
  })

  output$faltantes_totales <- renderValueBox({

    res <- por_hacer_filter() %>%
      summarise(por_hacer = sum(por_hacer)) |>
      pull() |>
      scales::comma()

    res <- pmax(0, res)

    valueBox(value = res, subtitle = glue::glue("Entrevistas por hacer en total"), color = "yellow")
  })

  output$excedentes_totales <- renderValueBox({

    res <- hecho_filter() %>%
      summarise(excedentes = sum(faltan)) |>
      mutate(excedentes = abs(pmin(0, excedentes))) |>
      pull() |>
      scales::comma()

    valueBox(value = res, subtitle = glue::glue("Entrevistas hechas de más en total"), color = "orange")
  })

  output$eliminadas_totales <- renderValueBox({

    res <- eliminadas_filter() %>%
      nrow() |>
      scales::comma()

    valueBox(value = res, subtitle = glue::glue("Entrevistas eliminadas en total"), color = "red")
  })

  output$por_hacer <- renderPlot({
    aux <- por_hacer_filter() %>% count(cluster, wt = por_hacer, name = "encuestas") %>%
      mutate(color = if_else(encuestas > 0, "#5BC0EB", "#C3423F"))

    aux %>%
      ggplot(aes(y = forcats::fct_reorder(factor(cluster), encuestas), x = encuestas)) +
      geom_col(aes(fill = color)) +
      geom_label(aes(label = encuestas)) +
      scale_fill_identity() +
      # annotate("label", x = aux %>% filter(encuestas == max(encuestas)) %>% pull(encuestas),
      #          y = aux %>% filter(encuestas == min(encuestas)) %>% pull(cluster) %>% factor(),
      #          size = 9,
      #          label = glue::glue("{scales::comma(sum(aux$encuestas))} entrevistas por hacer"),
      #          hjust = "inward", vjust = "inward") +
      theme_minimal() + ylab("cluster") + xlab("entrevistas por hacer")
  })

  output$por_hacer_cuotas <- renderPlot({

    pal_p <- leaflet::colorNumeric("Blues", domain = hecho_filter() %>% filter(faltan>0) %>% pull(faltan) %>% unique %>% sort)
    pal_n <- leaflet::colorNumeric("Reds", domain = hecho_filter() %>% filter(faltan<0) %>% pull(faltan) %>% unique %>% sort)
    uno <- pal_p(hecho_filter() %>% filter(faltan>0) %>% pull(faltan) %>% unique %>% sort)
    dos <- pal_n(hecho_filter() %>% filter(faltan<0) %>% pull(faltan) %>% unique %>% sort %>% rev)

    hecho_filter() %>% mutate(grupo = glue::glue("{edad} {sexo}")) %>%
      group_by(cluster) %>% mutate(total = sum(faltan)) %>%
      ungroup %>% mutate(cluster = reorder(cluster, total)) %>%
      ggplot(aes(y = cluster, x = grupo,
                 fill = factor(faltan))) +
      geom_tile() +
      geom_text(aes(label = faltan ), alpha = .5) +
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, 6)) +
      scale_fill_manual(values = c(dos, "white", uno)) +
      labs(fill = "Entrevistas \n por hacer", y = NULL, x = NULL) + theme_minimal()
  })

  output$sexo <- renderPlot({
    preguntas$encuesta$muestra$revisar_sexo()

    # preguntas$encuesta$muestra$muestra$poblacion$marco_muestral

  })

  output$rango_edad <- renderPlot({
    preguntas$encuesta$muestra$revisar_rango_edad()
  })

  output$descargar_resumen <- downloadHandler(filename = function(){
    paste("cuotas_por_municipio_", format(Sys.time(), "%Y_%m_%d-%H_%M"), ".xlsx", sep = "")
  },
  content = function(file){

    df_mun <- hecho %>%
      group_by(Municipio) %>%
      summarise(across(c(hecho, cuota, faltan), ~ sum(.x, na.rm = T)))

    df_mun_cluster <- hecho %>%
      group_by(Municipio, cluster) %>%
      summarise(across(c(hecho, cuota, faltan), ~ sum(.x, na.rm = T))) |>
      ungroup()

    wb <- openxlsx::createWorkbook()

    openxlsx::addWorksheet(wb, sheetName = "municipios")
    openxlsx::writeData(wb, df_mun, sheet = "municipios")

    openxlsx::addWorksheet(wb, sheetName = "municipios_cluster")
    openxlsx::writeData(wb, df_mun_cluster, sheet = "municipios_cluster")

    openxlsx::saveWorkbook(wb, file = file)

  },
  contentType = "file/xlsx"
  )

  output$avance_region <- renderPlot({

    clusters_en_muestra <- diseno$poblacion$marco_muestral |>
      distinct(strata_1, region, cluster_2)

    datos_de_levantamiento <- por_hacer |>
      group_by(cluster) |>
      summarise(across(.cols = c(cuota, hecho, por_hacer), .fns = ~ sum(.x)))

    bd_plot <- clusters_en_muestra |>
      inner_join(datos_de_levantamiento, by = c("cluster_2" = "cluster")) |>
      group_by(region, strata_1) |>
      summarise(across(.cols = c(cuota, hecho, por_hacer), .fns = ~ sum(.x))) |>
      mutate(pct = hecho/cuota) |>
      arrange(desc(pct)) |>
      mutate(region = paste("Región ", strata_1, sep = ""))

    g <- bd_plot %>%
      ggplot() +
      ggchicklet::geom_chicklet(aes(x = reorder(region, pct), y = cuota, fill = "A"), show.legend = T) +
      geom_text(aes(x = reorder(region, pct), y = cuota, label = cuota), hjust = -0.5) +
      ggchicklet::geom_chicklet(aes(x = reorder(region, pct), y = hecho, fill = "B"), show.legend = T) +
      ggfittext::geom_bar_text(aes(x = reorder(region, pct), y = hecho,
                                   label = paste(hecho, " (", scales::percent(x = pct, accuracy = 1.), ")", sep = "")
                                   # label = hecho
      ), vjust = 2.5, contrast = T) +
      coord_flip() +
      labs(x = "", y = "Entrevistas", fill = "") +
      scale_fill_manual(values = c("A" = "gray70", "B" = PRINCIPAL),
                        labels = c("A" = "Cuota", "B" = "Hecho")) +
      theme_minimal() +
      theme(panel.grid = element_blank(), text = element_text(size = 24), legend.position = "bottom",
            axis.text.x = element_text(family = "Poppins", size = 18),
            axis.text.y = element_text(family = "Poppins", size = 18))

    return(g)

  })

  output$descargar_region <- downloadHandler(filename = function(){

    paste("avance_regional_", format(Sys.time(), "%Y_%m_%d-%H_%M"), ".xlsx", sep = "")
  },
  content = function(file){

    clusters_en_muestra <- diseno$poblacion$marco_muestral |>
      distinct(strata_1, region, cluster_2)

    datos_de_levantamiento <- por_hacer |>
      group_by(cluster) |>
      summarise(across(.cols = c(cuota, hecho, por_hacer), .fns = ~ sum(.x)))

    datos_de_levantamiento_mun <- por_hacer |>
      group_by(cluster, Municipio) |>
      summarise(across(.cols = c(cuota, hecho, por_hacer), .fns = ~ sum(.x)))

    bd_region <- clusters_en_muestra |>
      inner_join(datos_de_levantamiento, by = c("cluster_2" = "cluster")) |>
      group_by(region, strata_1) |>
      summarise(across(.cols = c(cuota, hecho, por_hacer), .fns = ~ sum(.x))) %>%
      mutate(region = paste("Región ", strata_1, sep = "")) |>
      select(!strata_1)

    bd_region_municipio <- clusters_en_muestra |>
      inner_join(datos_de_levantamiento_mun, by = c("cluster_2" = "cluster")) |>
      relocate(Municipio, .after = region) |>
      group_by(strata_1, region, Municipio) |>
      summarise(across(.cols = c(cuota, hecho, por_hacer), .fns = ~ sum(.x))) |>
      ungroup() %>%
      mutate(region = paste("Región ", strata_1, sep = "")) |>
      select(!strata_1)

    bd_region_municipio_cluster <- clusters_en_muestra |>
      inner_join(datos_de_levantamiento_mun, by = c("cluster_2" = "cluster")) |>
      relocate(Municipio, .after = region) |>
      group_by(strata_1, region, Municipio, cluster_2) |>
      summarise(across(.cols = c(cuota, hecho, por_hacer), .fns = ~ sum(.x))) |>
      ungroup() %>%
      mutate(region = paste("Región ", strata_1, sep = "")) |>
      select(!strata_1)

    wb <- openxlsx::createWorkbook()

    openxlsx::addWorksheet(wb, sheetName = "region")
    openxlsx::writeData(wb, bd_region, sheet = "region")

    openxlsx::addWorksheet(wb, sheetName = "municipio")
    openxlsx::writeData(wb, bd_region_municipio, sheet = "municipio")

    openxlsx::addWorksheet(wb, sheetName = "cluster")
    openxlsx::writeData(wb, bd_region_municipio_cluster, sheet = "cluster")

    openxlsx::saveWorkbook(wb, file = file)

  },
  contentType = "file/xlsx"
  )

  # Pestaña "Auditoría" -----------------------------------------------------

  output$grafica <- renderPlot({

    preguntas$graficar(llave = !!rlang::sym(input$vars), "frecuencia", parametros = list(salto = 10, tit = "", porcentajes_afuera = F))

  })

  # Pestaña "Encuestadores" -------------------------------------------------

  ## Estadisticas colectivas ------------------------------------------------

  efectivas_filter_encuestadores <- eventReactive(c(bd, input$municipio_encuestadores),{

    bd %>%
      {
        if(input$municipio_encuestadores != "Todos"){
          filter(., MUNI == input$municipio_encuestadores)
        } else{
          .
        }}
  })

  eliminadas_filter_encuestadores <- eventReactive(c(eliminadas, input$municipio_encuestadores),{

    eliminadas %>%
      {
        if(input$municipio_encuestadores != "Todos"){
          filter(., MUNI == input$municipio_encuestadores)
        } else{
          .
        }}
  })

  corregidas_filter_encuestadores <- eventReactive(c(corregidas_shp, input$municipio_encuestadores),{

    corregidas_shp %>% as_tibble %>%
      left_join(bd %>% distinct(SbjNum, MUNI), by = "SbjNum") %>%
      {
        if(input$municipio_encuestadores != "Todos"){
          filter(., MUNI == input$municipio_encuestadores)
        } else{
          .
        }}
  })

  por_hacer_filter_encuestadores <- eventReactive(c(por_hacer, input$municipio_encuestadores),{

    por_hacer %>%
      {
        if(input$municipio_encuestadores != "Todos"){
          filter(., Municipio == input$municipio_encuestadores)
        } else{
          .
        }}
  })

  hecho_filter_encuestadores <- eventReactive(c(hecho, input$municipio_encuestadores),{

    hecho %>%
      {
        if(input$municipio_encuestadores != "Todos"){
          filter(., Municipio == input$municipio_encuestadores)
        } else{
          .
        }}
  })

  indice_eliminadas <- reactiveVal(1)
  indice_corregidas <- reactiveVal(1)
  indice_promedio <- reactiveVal(1)

  observeEvent(input$siguiente_eliminadas,{
    indice_eliminadas(indice_eliminadas() + 1)
  })

  observeEvent(input$siguiente_corregidas,{
    indice_corregidas(indice_corregidas() + 1)
  })

  observeEvent(input$siguiente_promedio,{
    indice_promedio(indice_promedio() + 1)
  })

  output$eliminadas_encuestador <- renderPlot({

    lista <- eliminadas_filter_encuestadores() %>%
      count(Srvyr) %>%
      arrange(desc(n)) %>%
      tibble::rownames_to_column("id") %>%
      mutate(grupo = ((as.numeric(id)-1) %/% 7)+1) %>%
      split(.$grupo)

    pag <- indice_eliminadas() %% (length(lista)+1)

    if(pag == 0) {
      indice_eliminadas(indice_eliminadas() + 1)
      pag <- indice_eliminadas() %% (length(lista)+1)
    }

    aux <- lista %>% purrr::pluck(pag) %>% select(Srvyr, n)

    g <- graficar_barras(bd = aux, color = "red") +
      labs(x = NULL, y = "Eliminadas", title = "Entrevistas eliminadas por encuestador")

    return(g)

  })

  output$corregidas_encuestador <- renderPlot({

    lista <- corregidas_filter_encuestadores() %>%
      count(Srvyr) %>%
      arrange(desc(n)) %>%
      tibble::rownames_to_column("id") %>%
      mutate(grupo = ((as.numeric(id)-1) %/% 7)+1) %>%
      split(.$grupo)

    pag <- indice_corregidas() %% (length(lista)+1)

    if(pag == 0) {
      indice_corregidas(indice_corregidas() + 1)
      pag <- indice_corregidas() %% (length(lista)+1)
    }

    aux <- lista %>% purrr::pluck(pag) %>% select(Srvyr, n)

    g <- graficar_barras(bd = aux, color = "orange") +
      labs(x = NULL, y = "Corregidas", title = "Entrevistas corregidas por encuestador")

    return(g)

  })

  output$duracion_entrevistas <- renderPlot({

    g <- efectivas_filter_encuestadores() %>%
      transmute(duracion = as.double(VEnd - VStart)) %>%
      # filter(duracion <= lubridate::as.duration("60 mins")) %>%
      filter(duracion <= 60) %>%
      ggplot(aes(x = duracion)) +
      geom_histogram(bins = 120, fill = "blue") +
      labs(x = "Duración (minutos)", y = "Entrevistas", title = "Duración de las entrevistas") +
      theme_minimal()

    return(g)

  })

  output$prom_tiempo_encuestador <- renderPlot({

    duracion_promedio <- efectivas_filter_encuestadores() %>% transmute(Srvyr,
                                                                        duracion = as.double(VEnd - VStart)) %>%
      filter(duracion <= 60) %>%
      summarise(duracion_promedio = mean(duracion)) %>% pull(duracion_promedio)

    lista <- efectivas_filter_encuestadores() %>%
      transmute(Srvyr,
                duracion = as.double(VEnd - VStart)) %>%
      filter(duracion <= 60) %>%
      group_by(Srvyr) %>%
      mutate(promedio = mean(duracion),
             min = min(duracion),
             max = max(duracion),
             sd = sd(duracion),
             mediana = median(duracion)) %>%
      select(!duracion) %>%
      ungroup() %>%
      distinct(.keep_all = T) %>%
      arrange(promedio) %>%
      tibble::rownames_to_column("id") %>%
      mutate(grupo = ((as.numeric(id)-1) %/% 7)+1) %>%
      split(.$grupo)

    pag <- indice_promedio() %% (length(lista)+1)

    if(pag == 0) {
      indice_promedio(indice_promedio() + 1)
      pag <- indice_promedio() %% (length(lista)+1)
    }

    aux <- lista %>% purrr::pluck(pag)

    g <- aux %>%
      ggplot(aes(x = reorder(str_to_title(Srvyr), -promedio), y = promedio)) +
      geom_point(size = 2) +
      geom_errorbar(aes(ymin = min, ymax = max)) +
      geom_hline(yintercept = duracion_promedio, show.legend = "Duración promedio", na.rm = T, color = "brown") +
      coord_flip() +
      scale_y_continuous(breaks = seq.int(from = 0, to = 60, by = 5)) +
      labs(x = NULL, y = "Minutos", title = "Duración promedio de las entrevistas") +
      theme_minimal() +
      theme(legend.position = "none", axis.text = element_text(size = 14))

    return(g)

  })

  output$razon_el <- renderPlot({

    bd_razones <- preguntas$encuesta$respuestas$eliminadas %>%

      {
        if(input$municipio_encuestadores != "Todos"){

          filter(., Muni == input$municipio_encuestadores)

        } else{
          .
        }
      } %>%
      count(razon) %>%
      mutate(pct = n/sum(n)) %>%
      rename(Srvyr = razon) %>%
      select(Srvyr, n)

    g <- graficar_barras(bd = bd_razones, color = "blue") +
      labs(x = NULL, y = NULL, title = "Razones para eliminar entrevistas")

    return(g)

  })

  output$eliminadas <- renderDT({

    eliminadas_filter_encuestadores() %>% select(SbjNum, Fecha= Date, Encuestador = Srvyr) %>%
      # bind_rows(
      #   bd %>% filter(is.na(Longitude)) %>% select(SbjNum, Fecha = Date, Encuestador = Srvyr) %>%
      #     mutate(Razón = "GPS apagado")
      # ) %>%
      arrange(desc(Fecha))
  }, options = list(dom = "ltpi",
                    language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json")))

  ## Estadísticas individuales ----------------------------------------------

  output$efectivas_individual <- renderValueBox({

    req(input$encuestador != "Seleccionar")

    res <- bd %>%
      count(Srvyr) %>%
      filter(Srvyr == input$encuestador) %>%
      pull(n)

    res <- pmax(0, res)

    valueBox(value = res, subtitle = glue::glue('Entrevistas efectivas'), color = "green")

  })

  output$corregidas_individual <- renderValueBox({

    req(input$encuestador != "Seleccionar")

    res <- corregidas_shp %>%
      as_tibble %>%
      left_join(bd %>% distinct(SbjNum, MUNI), by = "SbjNum") %>%
      count(Srvyr) %>%
      tidyr::complete(Srvyr = preguntas$encuesta$muestra$diseno$variables |> distinct(Srvyr) |> pull(),
                      fill = list(n = 0)) |>
      filter(Srvyr == input$encuestador) %>%
      pull(n)

    res <- pmax(0, res)

    valueBox(value = res, subtitle = glue::glue('Entrevistas corregidas'), color = "orange")

  })

  output$eliminadas_individual <- renderValueBox({

    req(input$encuestador != "Seleccionar")

    aux <- eliminadas %>%
      count(Srvyr) %>%
      filter(Srvyr == input$encuestador)

    if(nrow(aux) != 0) {

      res <- aux %>% pull(n)

    }

    if(nrow(aux) == 0) {

      res <- nrow(aux)

    }

    res <- pmax(0, res)

    valueBox(value = res, subtitle = glue::glue('Entrevistas eliminadas'), color = "red")

  })

  output$duracion_individual <- renderPlot({

    g <- efectivas_filter_encuestadores() %>%
      transmute(duracion = as.double(VEnd - VStart)) %>%
      filter(duracion <= 60) %>%
      ggplot(aes(x = duracion)) +
      geom_histogram(bins = 120, fill = "blue") +
      labs(x = "Duración (minutos)", y = "Entrevistas", title = "Duración de las entrevistas") +
      theme_minimal()

    return(g)

  })

  output$mapa_auditoria <- renderLeaflet({

    req(input$encuestador != "Seleccionar")

    nombres_region <- diseno$poblacion$marco_muestral |>
      distinct(region, strata_1) |>
      arrange(region) |>
      mutate(nombre_region = paste(strata_1, region, sep = " "))

    pal <- colorFactor(topo.colors(n_distinct(nombres_region$nombre_region)),
                       domain = unique(nombres_region$nombre_region))

    if(enc_shp %>% filter(as.numeric(distancia) != 0) %>% nrow() > 0){

      efectivas_encuestador <- preguntas$encuesta$muestra$diseno$variables |>
        as_tibble() |>
        filter(Srvyr == input$encuestador) |>
        pull(SbjNum)

      ent_c <- enc_shp %>%
        filter(Srvyr == input$encuestador) |>
        mutate(label = paste(!!rlang::sym(u_nivel$variable), Srvyr, SbjNum, sep= " - "),
               color = case_when(SbjNum %in% efectivas_encuestador ~ "green",
                                 T ~ "sin categoria")
        ) %>%
        arrange(distancia)

    } else {

      ent_c <- enc_shp %>%
        mutate(label = paste(!!rlang::sym(u_nivel$variable), Srvyr, SbjNum,sep= "-"),
               color = "green")

    }

    pal_n <- leaflet::colorNumeric(colorRampPalette(c("red", "white", "blue"))(3),
                                   domain = faltan_shp$n)

    mapa_auditoria <- mapa_base %>%
      left_join(nombres_region |>
                  select(strata_1, nombre_region), by = "strata_1") |>
      leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(color = ~pal(strata_1), opacity = 1, fill = T, fillOpacity = 0.1) %>%
      addPolygons(color = ~pal(nombre_region), opacity = 1, fill = T, fillOpacity = 0.1) %>%
      addLegend(pal = pal, values = ~nombre_region, position = "bottomleft", title = "Región") %>%
      shp$graficar_mapa(bd = diseno$muestra, nivel = u_nivel %>% pull(variable)) %>%
      addCircleMarkers(data = ent_c,
                       color = ~color,
                       stroke = F,
                       label = ~label,
                       group = "Entrevistas") %>%
      addCircleMarkers(data = corregidas_shp %>%
                         filter(Srvyr == input$encuestador),
                       stroke = F,
                       color = "yellow",
                       fillOpacity = 1,
                       popup = ~glue::glue("{SbjNum} - {Srvyr} - {Date} ≤<br> cluster reportado: {anterior} <br> cluster corregido: {nueva}"),
                       group = "Cluster corregido")

    if(nrow(eliminadas_shp %>% filter(Srvyr == input$encuestador)) != 0) {

      mapa_auditoria <- mapa_auditoria %>%
        addCircleMarkers(data = eliminadas_shp %>%
                           filter(Srvyr == input$encuestador),
                         stroke = F, color = "red", fillOpacity = 1,
                         label = ~glue::glue("{SbjNum} - {Srvyr}"),
                         group = "Eliminadas")

    }

    return(mapa_auditoria)

  })

  proxy <- leafletProxy("mapa_auditoria")

}

# Run the application
shinyApp(ui = ui, server = server)
