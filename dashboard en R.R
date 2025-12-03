# -----------------------------------------------------------------------------
# SCRIPT: 05_Dashboard_Interactivo_Shiny.R
# AUTOR: Matías Ogaz
# DESCRIPCIÓN: Dashboard interactivo para visualización de KPIs comerciales.
#              Demuestra capacidad de despliegue y creación de herramientas de usuario.
# -----------------------------------------------------------------------------

# 1. LIBRERÍAS
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, shinydashboard, tidyverse, plotly, DT, scales)

# 2. GENERACIÓN DE DATOS SIMULADOS (Para que el script sea autónomo)
set.seed(123)
fechas <- seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day")
n <- length(fechas)

datos_dash <- data.frame(
  Fecha = fechas,
  Sucursal = sample(c("Santiago Centro", "Las Condes", "Providencia", "Maipú"), n, replace = TRUE),
  Categoria = sample(c("Tecnología", "Hogar", "Vestuario"), n, replace = TRUE),
  Ventas = rlnorm(n, meanlog = 10, sdlog = 1), # Distribución log-normal
  Clientes = rpois(n, lambda = 30)
) %>%
  mutate(Mes = floor_date(Fecha, "month"))

# 3. DEFINICIÓN DE LA INTERFAZ DE USUARIO (UI)
ui <- dashboardPage(
  skin = "blue",
  
  # A. Encabezado
  dashboardHeader(title = "Dashboard Comercial"),
  
  # B. Barra Lateral (Filtros)
  dashboardSidebar(
    sidebarMenu(
      menuItem("Panel General", tabName = "dashboard", icon = icon("chart-line")),
      menuItem("Datos Crudos", tabName = "datos", icon = icon("table"))
    ),
    hr(),
    h5("Filtros de Control", align = "center"),
    selectInput("filtro_sucursal", "Selecciona Sucursal:", 
                choices = c("Todas", unique(as.character(datos_dash$Sucursal)))),
    dateRangeInput("filtro_fecha", "Rango de Fechas:",
                   start = min(datos_dash$Fecha), end = max(datos_dash$Fecha))
  ),
  
  # C. Cuerpo Principal
  dashboardBody(
    tabItems(
      # Pestaña 1: Gráficos
      tabItem(tabName = "dashboard",
              # Fila de KPIs (Value Boxes)
              fluidRow(
                valueBoxOutput("kpi_venta_total", width = 4),
                valueBoxOutput("kpi_clientes", width = 4),
                valueBoxOutput("kpi_ticket_promedio", width = 4)
              ),
              # Fila de Gráficos
              fluidRow(
                box(title = "Evolución de Ventas (Interactivo)", status = "primary", solidHeader = TRUE,
                    plotlyOutput("plot_tendencia", height = "300px"), width = 8),
                box(title = "Ventas por Categoría", status = "warning", solidHeader = TRUE,
                    plotlyOutput("plot_categoria", height = "300px"), width = 4)
              )
      ),
      # Pestaña 2: Tabla de Datos
      tabItem(tabName = "datos",
              h3("Detalle Transaccional"),
              DTOutput("tabla_detalle")
      )
    )
  )
)

# 4. LÓGICA DEL SERVIDOR (SERVER)
server <- function(input, output) {
  
  # A. Filtrado Reactivo (El corazón de la app)
  datos_filtrados <- reactive({
    data <- datos_dash %>%
      filter(Fecha >= input$filtro_fecha[1] & Fecha <= input$filtro_fecha[2])
    
    if (input$filtro_sucursal != "Todas") {
      data <- data %>% filter(Sucursal == input$filtro_sucursal)
    }
    return(data)
  })
  
  # B. Cálculo de KPIs
  output$kpi_venta_total <- renderValueBox({
    total <- sum(datos_filtrados()$Ventas)
    valueBox(paste0("$", comma(total, scale = 1e-6, suffix = " M")), 
             "Venta Total", icon = icon("dollar-sign"), color = "green")
  })
  
  output$kpi_clientes <- renderValueBox({
    total <- sum(datos_filtrados()$Clientes)
    valueBox(comma(total), "Total Clientes", icon = icon("users"), color = "purple")
  })
  
  output$kpi_ticket_promedio <- renderValueBox({
    promedio <- mean(datos_filtrados()$Ventas)
    valueBox(paste0("$", comma(round(promedio, 0))), "Ticket Promedio", icon = icon("shopping-cart"), color = "aqua")
  })
  
  # C. Gráficos con Plotly (Interactivos)
  output$plot_tendencia <- renderPlotly({
    g <- datos_filtrados() %>%
      group_by(Fecha) %>%
      summarise(Venta_Diaria = sum(Ventas)) %>%
      ggplot(aes(x = Fecha, y = Venta_Diaria)) +
      geom_line(color = "#0073b7", size = 1) +
      geom_smooth(method = "loess", se = FALSE, color = "red", linetype = "dashed") +
      theme_minimal() +
      labs(x = "", y = "Ventas ($)")
    
    ggplotly(g) # Convierte ggplot estático a interactivo HTML
  })
  
  output$plot_categoria <- renderPlotly({
    g <- datos_filtrados() %>%
      group_by(Categoria) %>%
      summarise(Total = sum(Ventas)) %>%
      ggplot(aes(x = reorder(Categoria, Total), y = Total, fill = Categoria)) +
      geom_col() +
      coord_flip() +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(x = "", y = "")
    
    ggplotly(g)
  })
  
  # D. Tabla de Datos
  output$tabla_detalle <- renderDT({
    datatable(datos_filtrados(), 
              options = list(pageLength = 10, scrollX = TRUE))
  })
}

# 5. EJECUTAR APLICACIÓN
shinyApp(ui = ui, server = server)