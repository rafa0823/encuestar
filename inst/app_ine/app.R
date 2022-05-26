library(shiny)
library(leaflet)
library(readr)
library(purrr)
library(tibble)
library(sf)
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

options(survey.lonely.psu="remove")
preguntas <- read_rds("data/clase_pregunta.rda")
diseno <- preguntas$encuesta$muestra$muestra
shp <- preguntas$encuesta$shp_completo
bd <- preguntas$encuesta$respuestas$base
enc_shp <- readr::read_rds("data/enc_shp.rda")
eliminadas <- preguntas$encuesta$respuestas$eliminadas
eliminadas_shp <- eliminadas %>% filter(!is.na(Longitude)) %>% st_as_sf(coords = c("Longitude", "Latitude"),crs = 4326)
mapa_base <- read_rds("data/mapa_base.rda")
bbox_qro <- st_bbox(shp$shp$MUN)

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

# vars necesarios para app ------------------------------------------------

u_nivel <- diseno$niveles %>% filter(nivel == diseno$ultimo_nivel)
u_nivel_tipo <- u_nivel %>% transmute(paste(tipo,nivel,sep = "_")) %>% pull(1)

aulr <- unidades_app(diseno, u_nivel)

# cuotas ------------------------------------------------------------------
n_entrevista <- entrevistas(diseno, bd, u_nivel, u_nivel_tipo)
hecho <- n_entrevista$hecho
por_hacer <- n_entrevista$por_hacer

ui <-dashboardPage(
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
        "Auditoría", tabName = "auditoria", icon = icon("search")
      )
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem("mapa",
              leafletOutput(outputId = "map", height = 600),

              # Shiny versions prior to 0.11 should use class = "modal" instead.
              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                            width = 330, height = "auto",
                            HTML("<button data-toggle='collapse' data-target='#demo'>Min/max</button>"),
                            tags$div(id = 'demo',  class="collapse",
                                     selectInput("cluster", "Cluster", c("Seleccione..."= "",sort(unique(diseno$muestra[[diseno$ultimo_nivel]][[u_nivel_tipo]])))
                                     ),
                                     actionButton("filtrar","Filtrar"),
                                     gt_output("faltantes"),
                                     hr(),
                                     actionButton("regresar", "Regresar")
                            )
              )
      ),
      tabItem("entrevistas",
              h2("Entrevistas realizadas"),

              fluidRow(
                column(12,
                       progressBar(id = "enc_hechas", value = nrow(bd),display_pct = T,striped = T,
                                   total = (diseno$niveles %>% filter(nivel == 0) %>% pull(unidades))*diseno$n_0,
                                   status = "success"
                       )
                )
              ),
              fluidRow(
                valueBox(width = 6,
                         value = por_hacer %>% filter(por_hacer < 0) %>% summarise(sum(por_hacer)) %>% pull(1) %>% abs(),
                         subtitle = "Entrevistas hechas de más según la cuota",color = "yellow",
                         icon = icon("plus")
                ),
                valueBox(width = 6,
                         value = nrow(eliminadas),
                         subtitle = "Entrevistas eliminadas",
                         color = "red",
                         icon = icon("times")
                )
              ),
              h2("Entrevitas por hacer"),
              fluidRow(
                column(6,
                       withSpinner(plotOutput("por_hacer",height = 600)),
                ),
                column(6,
                       withSpinner(plotOutput("por_hacer_cuotas",height = 600))
                )
              ),
              h2("Eliminadas"),
              fluidRow(
                column(6,
                       plotOutput("razon_el",height = 400)
                ),
                column(6,
                       DTOutput("eliminadas")
                )
              )

      ),
      tabItem("auditoria",
              fluidRow(
                column(6, passwordInput(inputId = "psw",label = "Contraseña")),
                column(6, hidden(selectInput("vars", "Variable", choices = c("Cargando..." = ""))))
              ),
              # uiOutput("au")
              fluidRow(
                plotOutput("grafica",height = 600)
              )
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {

  output$map <- renderLeaflet({
    pal <- colorFactor(topo.colors(n_distinct(mapa_base$strata_1)), domain = unique(mapa_base$strata_1))
    pal2 <- leaflet::colorBin(palette = c("blue", "yellow", "orange"),
                              domain = unique(enc_shp %>% filter(as.numeric(distancia) != 0) %>% pull(distancia)), bins = 5)


    mapa_base %>% leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
      addPolygons(color = ~pal(strata_1), opacity = 1, fill = F) %>%
      addLegend(pal = pal, values = ~strata_1, position = "bottomleft") %>%
      shp$graficar_mapa(bd = diseno$muestra, nivel = u_nivel %>% pull(variable)) %>%
      shp$graficar_mapa(bd = diseno$muestra, nivel = "MANZANA") %>%
      addCircleMarkers(data = enc_shp %>%
                         mutate(label = paste(!!rlang::sym(u_nivel$variable), Srvyr, SbjNum, sep= "-"),
                                color = if_else(as.numeric(distancia) == 0, "green", pal2(distancia))) %>%
                         arrange(distancia),
                       color = ~color, stroke = F,
                       label = ~label, group = "Entrevistas")  %>%
      addCircleMarkers(data = eliminadas_shp, stroke = F, color = "#FF715B", fillOpacity = 1,
                       label = ~glue::glue("{SbjNum} - {Srvyr}"), group = "Eliminadas", clusterOptions = markerClusterOptions()) %>%
      addLegend(position = "bottomright", colors = "green", labels = "Dentro de cluster") %>%
      addLegend(data = enc_shp %>% filter(as.numeric(distancia) != 0),
                position = "bottomright", pal = pal2, values = ~distancia,
                title = "Distancia (m)") %>% addLayersControl(baseGroups = c("Entrevistas", "Eliminadas"))

  })

  proxy <- leafletProxy("map")

  observeEvent(input$filtrar,{
    bbox <-  aulr %>% filter(!!rlang::sym(u_nivel_tipo) == !!input$cluster) %>% sf::st_bbox()

    proxy %>% flyToBounds(bbox[[1]],bbox[[2]],bbox[[3]],bbox[[4]])
  })

  observeEvent(input$regresar,{
    proxy %>% flyToBounds(bbox_qro[[1]],bbox_qro[[2]],bbox_qro[[3]],bbox_qro[[4]])
  })

  output$faltantes <- render_gt({
    req(input$filtrar)
    validate(need(input$cluster != "",message =  "Escoja un cluster"))

    hecho %>% filter(cluster == !!input$cluster) %>% select(sexo,edad,faltan) %>%
      pivot_wider(names_from = sexo,values_from = faltan) %>%
      replace_na(list(Mujer = 0, Hombre = 0)) %>% gt() %>%
      tab_header(title = "Faltantes",subtitle = glue("Cluster: {input$cluster}"))
  })

  output$por_hacer <- renderPlot({
    aux <- por_hacer %>% count(cluster, wt = por_hacer, name = "encuestas") %>%# filter(encuestas != 0) %>%
      mutate(color = if_else(encuestas > 0, "#5BC0EB", "#C3423F"))

    aux %>%
      ggplot(aes(y = forcats::fct_reorder(factor(cluster), encuestas), x = encuestas)) +
      geom_col(aes(fill = color)) +
      geom_label(aes(label = encuestas)) +
      scale_fill_identity() +
      annotate("label", x = aux %>% filter(encuestas == max(encuestas)) %>% pull(encuestas),
               y = aux %>% filter(encuestas == min(encuestas)) %>% pull(cluster) %>% factor(),
               size = 9,
               label = glue::glue("{scales::comma(sum(aux$encuestas))} entrevistas por hacer"),
               hjust = "inward", vjust = "inward") +
      theme_minimal() + ylab("cluster") + xlab("entrevistas por hacer")
  })

  output$por_hacer_cuotas <- renderPlot({

    pal_p <- leaflet::colorNumeric("Blues", domain = hecho %>% filter(faltan>0) %>% pull(faltan) %>% unique %>% sort)
    pal_n <- leaflet::colorNumeric("Reds", domain = hecho %>% filter(faltan<0) %>% pull(faltan) %>% unique %>% sort)
    uno <- pal_p(hecho %>% filter(faltan>0) %>% pull(faltan) %>% unique %>% sort)
    dos <- pal_n(hecho %>% filter(faltan<0) %>% pull(faltan) %>% unique %>% sort %>% rev)

    hecho %>% mutate(grupo = glue::glue("{edad} {sexo}")) %>%
      group_by(cluster) %>% mutate(total = sum(faltan)) %>%
      ungroup %>% mutate(cluster = reorder(cluster, total)) %>%
      ggplot(aes(y = cluster, x = grupo,
                 fill = factor(faltan))) +
      geom_tile() +
      geom_text(aes(label = faltan ), alpha = .5) +
      scale_fill_manual(values = c(dos, "white", uno)) +
      labs(fill = "Entrevistas \n por hacer", y = NULL, x = NULL) + theme_minimal()
  })

  output$razon_el <- renderPlot({
    preguntas$encuesta$respuestas$eliminadas %>% count(razon) %>% mutate(pct = n/sum(n)) %>%
      ggplot(aes(y = reorder(razon,n), x = n)) + geom_col(fill = "#FF715B") +
      geom_text(aes(label = scales::percent(pct,1)), size = 3, hjust = 1, color = "black") + theme_minimal() +
      labs(y = "Razón", x = "Entervistas eliminadas")
  })

  output$eliminadas <- renderDT({
    eliminadas %>% select(SbjNum, Fecha= Date, Encuestador = Srvyr) %>%
      # bind_rows(
      #   bd %>% filter(is.na(Longitude)) %>% select(SbjNum, Fecha = Date, Encuestador = Srvyr) %>%
      #     mutate(Razón = "GPS apagado")
      # ) %>%
      arrange(desc(Fecha))
  }, options = list(dom = "ltpi",
                    language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json")))

  observeEvent(input$psw,{

    if(input$psw == "hola"){
      updateSelectInput(session, "vars", choices = preguntas$encuesta$auditar)
      shinyjs::show(id = "vars")
    } else{
      shinyjs::hide(id = "vars")
    }

  })

  output$grafica <- renderPlot({
    validate(
      need(input$psw == "hola" & (input$vars != ""), message = "Escriba la contraseña")
    )

    preguntas$graficar(llave = !!rlang::sym(input$vars), "frecuencia", parametros = list(salto = 10, tit = ""))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
