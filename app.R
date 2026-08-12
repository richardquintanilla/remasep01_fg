# app.R - REMASEP01 Secciones F y G - Richard Quintanilla

library(shiny)
library(shinydashboard)
library(dplyr)
library(reactable)
library(htmltools)
library(tidyr)
library(openxlsx)
library(ggplot2)
library(plotly)
library(fst)

# ============================================================
# FUNCIÓN PARA ENCONTRAR ARCHIVOS
# ============================================================
encontrar_archivo <- function(nombres_posibles) {
     for(ruta in nombres_posibles) {
          if(file.exists(ruta)) {
               return(ruta)
          }
     }
     return(NULL)
}

# ============================================================
# CARGA DE DATOS DESDE .fst
# ============================================================

# Definir rutas posibles para los archivos .fst
rutas_remasep25 <- c(
     "remasep/listados/data/remasep25.fst",
     "../remasep/listados/data/remasep25.fst",
     "/srv/shiny-server/data/remasep25.fst"
)

rutas_f1 <- c(
     "remasep/listados/data/remasep01_f1.fst",
     "../remasep/listados/data/remasep01_f1.fst",
     "/srv/shiny-server/data/remasep01_f1.fst"
)

rutas_f2 <- c(
     "remasep/listados/data/remasep01_f2.fst",
     "../remasep/listados/data/remasep01_f2.fst",
     "/srv/shiny-server/data/remasep01_f2.fst"
)

rutas_g1 <- c(
     "remasep/listados/data/remasep01_g1.fst",
     "../remasep/listados/data/remasep01_g1.fst",
     "/srv/shiny-server/data/remasep01_g1.fst"
)

rutas_g2 <- c(
     "remasep/listados/data/remasep01_g2.fst",
     "../remasep/listados/data/remasep01_g2.fst",
     "/srv/shiny-server/data/remasep01_g2.fst"
)

rutas_g3 <- c(
     "remasep/listados/data/remasep01_g3.fst",
     "../remasep/listados/data/remasep01_g3.fst",
     "/srv/shiny-server/data/remasep01_g3.fst"
)

rutas_config <- c(
     "remasep/listados/data/config.rds",
     "../remasep/listados/data/config.rds",
     "/srv/shiny-server/data/config.rds"
)

# Encontrar rutas válidas
ruta_remasep25 <- encontrar_archivo(rutas_remasep25)
ruta_f1 <- encontrar_archivo(rutas_f1)
ruta_f2 <- encontrar_archivo(rutas_f2)
ruta_g1 <- encontrar_archivo(rutas_g1)
ruta_g2 <- encontrar_archivo(rutas_g2)
ruta_g3 <- encontrar_archivo(rutas_g3)
ruta_config <- encontrar_archivo(rutas_config)

# Verificar que se encontraron los archivos
if(is.null(ruta_remasep25)) stop("❌ No se encontró remasep25.fst")
if(is.null(ruta_f1)) stop("❌ No se encontró remasep01_f1.fst")
if(is.null(ruta_f2)) stop("❌ No se encontró remasep01_f2.fst")
if(is.null(ruta_g1)) stop("❌ No se encontró remasep01_g1.fst")
if(is.null(ruta_g2)) stop("❌ No se encontró remasep01_g2.fst")
if(is.null(ruta_g3)) stop("❌ No se encontró remasep01_g3.fst")
if(is.null(ruta_config)) stop("❌ No se encontró config.rds")

# Cargar datos desde .fst
cat("📂 Cargando datos desde .fst...\n")
cat("  - remasep25:", ruta_remasep25, "\n")
cat("  - remasep01_f1:", ruta_f1, "\n")
cat("  - remasep01_f2:", ruta_f2, "\n")
cat("  - remasep01_g1:", ruta_g1, "\n")
cat("  - remasep01_g2:", ruta_g2, "\n")
cat("  - remasep01_g3:", ruta_g3, "\n")
cat("  - config:", ruta_config, "\n")

# ============================================================
# CARGAR Y CONVERTIR CÓDIGOS A CHARACTER
# ============================================================
remasep25 <- read.fst(ruta_remasep25, as.data.table = FALSE) %>%
     mutate(codigopres = as.character(codigopres))

remasep01_f1 <- read.fst(ruta_f1, as.data.table = FALSE) %>%
     mutate(codigopres = as.character(codigopres))

remasep01_f2 <- read.fst(ruta_f2, as.data.table = FALSE) %>%
     mutate(codigopres = as.character(codigopres))

remasep01_g1 <- read.fst(ruta_g1, as.data.table = FALSE) %>%
     mutate(codigopres = as.character(codigopres))

remasep01_g2 <- read.fst(ruta_g2, as.data.table = FALSE) %>%
     mutate(codigopres = as.character(codigopres))

remasep01_g3 <- read.fst(ruta_g3, as.data.table = FALSE) %>%
     mutate(codigopres = as.character(codigopres))

config <- readRDS(ruta_config)

# Extraer configuraciones
opciones_establecimientos <- config$opciones_establecimientos
opciones_establecimientos <- c("Todos" = "all", opciones_establecimientos)

orden_tabla_f1 <- config$orden_tabla_f1
orden_tabla_f2 <- config$orden_tabla_f2
orden_tabla_g1 <- config$orden_tabla_g1
orden_tabla_g2 <- config$orden_tabla_g2
orden_tabla_g3 <- config$orden_tabla_g3
categorias_con_prematuros <- config$categorias_con_prematuros
categorias_para_total <- config$categorias_para_total
meses_nombres <- config$meses_nombres

cat("✅ Datos cargados exitosamente\n")
cat("📊 REMASEP25:", nrow(remasep25), "filas\n")
cat("📊 Opciones de establecimientos:", length(opciones_establecimientos), "\n")

# ============================================================
# RESTO DE CONFIGURACIONES
# ============================================================
meses_choices <- setNames(1:12, meses_nombres)

fmt_export <- function(x) {
     if (is.na(x) || is.null(x)) return("0")
     if (!is.numeric(x)) return(as.character(x))
     format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
}

tramos_edad_ive <- c("0-14", "14-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55+")

# ============================================================
# UI - COMPLETO
# ============================================================
ui <- dashboardPage(
     dashboardHeader(title = "REMASEP 2025"),
     dashboardSidebar(
          width = 300,
          tags$style(HTML("
      .skin-blue .main-header { position: fixed; width: 100%; z-index: 1030; top: 0; }
      .main-sidebar { position: fixed; top: 50px; bottom: 0; left: 0; z-index: 1020; overflow-y: auto; }
      .content-wrapper, .right-side { margin-left: 300px; padding-top: 50px; overflow-x: hidden; }
      @media (max-width: 767px) { .content-wrapper, .right-side { margin-left: 0; } }
      .main-sidebar { background-color: #191970 !important; }
      .sidebar-menu > li > a { color: #ecf0f1 !important; background-color: #191970 !important; }
      .sidebar-menu > li > a:hover { background-color: #2c2c8a !important; }
      .skin-blue .main-header .navbar { background-color: #191970 !important; }
      .skin-blue .main-header .logo { background-color: #191970 !important; }
      .content-wrapper, .right-side { background-color: #f4f4f4; }
      .box, .portlet { border: none !important; box-shadow: none !important; }
      .box.box-primary, .box.box-info { border: none !important; }
      .box-body { border: none !important; }
      .box.box-solid.box-primary > .box-header { border-bottom: 1px solid #e0e0e0 !important; }
      .box.box-solid.box-info > .box-header { border-bottom: 1px solid #e0e0e0 !important; }
      .control-label { font-weight: normal !important; color: white !important; }
      .sidebar .selectize-control, .sidebar .shiny-input-container:not(.shiny-input-container-inline) { width: 100% !important; }
      .sidebar .checkbox, .sidebar .action-button { width: 100%; margin-left: 0; margin-right: 0; }
      .sidebar .action-button { margin-top: 5px; }
      
      #limpiar_filtros {
        background-color: #EEE9E9 !important;
        color: #191970 !important;
        width: 100% !important;
        border: none !important;
        padding: 8px 15px !important;
        border-radius: 4px !important;
        font-weight: normal !important;
        margin-top: 5px !important;
        margin-left: 0 !important;
        margin-right: 0 !important;
        transition: background-color 0.2s !important;
      }
      #limpiar_filtros:hover {
        background-color: #d3d3d3 !important;
        color: #191970 !important;
      }
      
      .sidebar-menu {margin-top: 0 !important; padding-top: 10px !important;}
      .main-sidebar, .sidebar {padding-top: 0 !important; margin-top: 0 !important; background-color: #191970 !important;}
      .wrapper {background-color: #191970 !important;}
      
      .sidebar-menu > li.active > a {border-left-color: #ff0000 !important;}
      .sidebar-menu > li > a:hover {border-left-color: transparent !important; background-color: #EEE9E9 !important; color: #191970 !important;}
      .skin-blue .main-header .sidebar-toggle:hover {background-color: #EEE9E9 !important;}
      
      .box.box-solid.box-primary > .box-header {
        background-color: #191970 !important;
        color: white !important;
        font-size: 16px !important;
        font-weight: bold !important;
      }
      .box.box-solid.box-primary {
        border: 2px solid #191970 !important;
        border-radius: 5px !important;
      }
      
      .value-box-container {
        padding: 0 5px;
        margin-bottom: 15px;
      }
      
      .custom-value-box {
        border-radius: 10px;
        padding: 15px 20px;
        position: relative;
        box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
        transition: all 0.3s cubic-bezier(.25,.8,.25,1);
        min-height: 100px;
        display: flex;
        align-items: center;
        justify-content: space-between;
      }
      
      .custom-value-box:hover {
        box-shadow: 0 14px 28px rgba(0,0,0,0.25), 0 10px 10px rgba(0,0,0,0.22);
        transform: translateY(-2px);
      }
      
      .custom-value-box .box-content {
        display: flex;
        flex-direction: column;
        flex: 1;
        align-items: center;
        text-align: center;
        width: 100%;
      }
      
      .custom-value-box .box-content .box-number {
        font-size: 32px;
        font-weight: 900 !important;
        color: #ffffff !important;
        margin: 0;
        line-height: 1.2;
        text-align: center;
      }
      
      .custom-value-box .box-content .box-title {
        font-size: 15px;
        font-weight: 900 !important;
        color: #ffffff !important;
        margin: 0;
        opacity: 1;
        text-align: center;
        letter-spacing: 0.3px;
      }
      
      .custom-value-box .box-icon {
        font-size: 45px;
        margin-left: 10px;
        flex-shrink: 0;
        align-self: center;
        opacity: 0.75 !important;
      }
      
      .custom-value-box .box-icon i,
      .custom-value-box .box-icon svg {
        opacity: 0.75 !important;
      }
      
      /* Clases específicas para cada tarjeta */
      .card-partos { background-color: #6C3483; }
      .card-cesareas { background-color: #1E8449; }
      .card-total { background-color: #5D6D7E; }
      
      .custom-value-box,
      .custom-value-box *,
      .custom-value-box .box-content,
      .custom-value-box .box-content *,
      .custom-value-box .box-content .box-number,
      .custom-value-box .box-content .box-title {
        color: #ffffff !important;
        font-weight: 900 !important;
      }
      
      .card-partos .box-icon i,
      .card-partos .box-icon svg,
      .card-cesareas .box-icon i,
      .card-cesareas .box-icon svg,
      .card-total .box-icon i,
      .card-total .box-icon svg {
        color: #ffffff !important;
        opacity: 0.25 !important;
      }
      
      @media (max-width: 768px) {
        .custom-value-box .box-content .box-number {
          font-size: 28px;
        }
        .custom-value-box .box-content .box-title {
          font-size: 13px;
        }
        .custom-value-box .box-icon {
          font-size: 30px;
        }
        .custom-value-box {
          padding: 10px 15px;
          min-height: 80px;
        }
      }
    ")),
          
          div(style = "display: flex; justify-content: center; align-items: center; gap: 15px; padding: 0 0 0 0; margin: 0; margin-top: 10px;",
              tags$img(src = "https://raw.githubusercontent.com/richardquintanilla/remasep01_fg/main/www/logo_seremi.png", 
                       height = "90px", style = "display: block;"),
              tags$img(src = "https://raw.githubusercontent.com/richardquintanilla/remasep01_fg/main/www/logo_uaid_blanco.png", 
                       height = "100px", style = "display: block;")
          ),
          
          sidebarMenu(
               menuItem("📊 REMASEP01 - Secciones F y G", tabName = "remasep01"),
               menuItem("🚨 Alertas y Validaciones", tabName = "alertas"),
               menuItem("💾 Descarga de Datos", tabName = "descarga")
          ),
          
          br(),
          hr(),
          h4("Filtros", style = "padding-left: 15px; color: #ecf0f1; font-weight: normal; margin-bottom: 10px;"),
          
          selectInput("establecimiento", "🏨 Establecimiento",
                      choices = NULL,
                      selected = NULL),
          selectInput("mes", "📅 Mes (múltiple o Todos)",
                      choices = c("Todos" = "all", meses_choices),
                      selected = "all",
                      multiple = TRUE),
          actionButton("limpiar_filtros", "Limpiar filtros", icon = icon("eraser"))
     ),
     dashboardBody(
          tags$head(
               tags$style(HTML("
        .box { border: none !important; box-shadow: none !important; border-radius: 0 !important; }
        .box.box-primary > .box-header { background-color: #191970 !important; color: white !important; border-radius: 0 !important; }
        .cell-no-aplica { background-color: #555555 !important; color: white !important; text-align: center; }
        .content-wrapper, .right-side { background-color: #ffffff; }
        .main-header { position: fixed; width: 100%; z-index: 1030; }
        .main-sidebar { position: fixed; height: 100vh; overflow-y: auto; z-index: 1020; }
        .content-wrapper { margin-top: 50px; margin-left: 300px; padding: 15px; }
      "))
          ),
          tabItems(
               tabItem(tabName = "remasep01",
                       fluidRow(
                            column(12,
                                   div(style = "padding: 15px; margin-bottom: 20px; background-color: #f8f9fa;",
                                       h3("SECCIÓN F: PARTOS Y ABORTOS", style = "color: #191970; font-weight: bold; margin-top: 0; 
                              margin-bottom: 15px; border-bottom: 2px solid #191970; padding-bottom: 10px;"),
                                       
                                       fluidRow(
                                            div(class = "col-sm-4 value-box-container",
                                                uiOutput("porcentaje_partos_box_ui")
                                            ),
                                            div(class = "col-sm-4 value-box-container",
                                                uiOutput("porcentaje_cesareas_box_ui")
                                            ),
                                            div(class = "col-sm-4 value-box-container",
                                                uiOutput("porcentaje_total_box_ui")
                                            )
                                       ),
                                       
                                       box(
                                            title = "F.1: PARTOS Y ABORTOS ATENDIDOS", 
                                            status = "primary", solidHeader = TRUE, width = 12,
                                            reactableOutput("tabla_resumen_f1"),
                                            div(style = "font-size: 12px; color: #555; margin-top: 10px; text-align: left;",
                                                "(**) Están incluidos cualquier tipo de parto")
                                       ),
                                       
                                       h4("PARTOS Y CESÁREAS", style = "color: #191970; font-weight: bold; margin-top: 20px; 
                              margin-bottom: 15px; border-bottom: 2px solid #191970; padding-bottom: 10px;"),
                                       
                                       fluidRow(
                                            column(6,
                                                   box(
                                                        title = "Gráfico por Establecimiento - Partos",
                                                        status = "primary", 
                                                        solidHeader = TRUE, 
                                                        width = 12,
                                                        plotlyOutput("grafico_alertas_partos", height = "400px")
                                                   )
                                            ),
                                            column(6,
                                                   box(
                                                        title = "Gráfico por Establecimiento - Cesáreas",
                                                        status = "primary", 
                                                        solidHeader = TRUE, 
                                                        width = 12,
                                                        plotlyOutput("grafico_alertas_cesareas", height = "400px")
                                                   )
                                            )
                                       ),
                                       
                                       fluidRow(
                                            column(6,
                                                   box(
                                                        title = "Detalle de Partos",
                                                        status = "primary", 
                                                        solidHeader = TRUE, 
                                                        width = 12,
                                                        reactableOutput("tabla_detalle_partos"),
                                                        div(style = "font-size: 11px; color: #555; margin-top: 5px; text-align: left;",
                                                            "*Parto presentación cefálica o podálica: c/s episiotomía, c/s sutura, c/s fórceps, 
                                          c/s inducción, c/s versión interna, c/s revisión, c/s extracción manual de placenta, 
                                          c/s monitorización. (Único o Múltiple)")
                                                   )
                                            ),
                                            column(6,
                                                   box(
                                                        title = "Detalle de Cesáreas",
                                                        status = "primary", 
                                                        solidHeader = TRUE, 
                                                        width = 12,
                                                        reactableOutput("tabla_detalle_cesareas")
                                                   )
                                            )
                                       ),
                                       
                                       h4("ABORTOS Y OTROS PROCEDIMIENTOS OBSTÉTRICOS", 
                                          style = "color: #191970; font-weight: bold; margin-top: 20px; margin-bottom: 15px; 
                              border-bottom: 2px solid #191970; padding-bottom: 10px;"),
                                       
                                       fluidRow(
                                            column(6,
                                                   box(
                                                        title = "Gráfico por Establecimiento",
                                                        status = "primary", 
                                                        solidHeader = TRUE, 
                                                        width = 12,
                                                        plotlyOutput("grafico_alertas_abortos", height = "450px")
                                                   )
                                            ),
                                            column(6,
                                                   box(
                                                        title = "Detalle por Establecimiento",
                                                        status = "primary", 
                                                        solidHeader = TRUE, 
                                                        width = 12,
                                                        reactableOutput("tabla_detalle_abortos")
                                                   )
                                            )
                                       ),
                                       
                                       box(
                                            title = "F.2: INTERRUPCIÓN VOLUNTARIA DEL EMBARAZO (IVE)", 
                                            status = "primary", solidHeader = TRUE, width = 12,
                                            reactableOutput("tabla_resumen_f2")
                                       )
                                   )
                            )
                       ),
                       
                       fluidRow(
                            column(12,
                                   div(style = "padding: 15px; margin-bottom: 20px; background-color: #f8f9fa;",
                                       h3("SECCIÓN G: INFORMACIÓN RECIÉN NACIDOS", style = "color: #191970; font-weight: bold; 
                              margin-top: 0; margin-bottom: 15px; border-bottom: 2px solid #191970; padding-bottom: 10px;"),
                                       
                                       box(
                                            title = "G.1: RECIÉN NACIDOS VIVOS Y FALLECIDOS SEGÚN PESO AL NACER", 
                                            status = "primary", solidHeader = TRUE, width = 12,
                                            reactableOutput("tabla_resumen_g1"),
                                            div(style = "font-size: 12px; color: #555; margin-top: 10px; text-align: left;",
                                                "*PKU e HC: FENILQUETONURIA e HIPOTIROIDISMO CONGÉNITO")
                                       ),
                                       
                                       box(
                                            title = "G.2: RECIÉN NACIDO CON MALFORMACIÓN CONGÉNITA", 
                                            status = "primary", solidHeader = TRUE, width = 12,
                                            reactableOutput("tabla_resumen_g2")
                                       ),
                                       
                                       box(
                                            title = "G.3: APGAR MENOR O IGUAL A 3 AL MINUTO Y APGAR MENOR O IGUAL A 6 A LOS 5 MINUTOS", 
                                            status = "primary", solidHeader = TRUE, width = 12,
                                            reactableOutput("tabla_resumen_g3")
                                       )
                                   )
                            )
                       )
               ),
               
               tabItem(tabName = "alertas",
                       fluidRow(
                            column(12,
                                   div(style = "padding: 15px; margin-bottom: 20px; background-color: #f8f9fa;",
                                       h3("⚕️ INTERRUPCIÓN VOLUNTARIA DEL EMBARAZO (IVE)", 
                                          style = "color: #191970; font-weight: bold; margin-top: 0; margin-bottom: 15px; 
                              border-bottom: 2px solid #191970; padding-bottom: 10px;"),
                                       
                                       fluidRow(
                                            column(12,
                                                   box(
                                                        title = "Detalle de IVE por Establecimiento, Causal y Tramo de Edad",
                                                        status = "primary", 
                                                        solidHeader = TRUE, 
                                                        width = 12,
                                                        reactableOutput("tabla_detalle_ive"),
                                                        div(style = "font-size: 11px; color: #555; margin-top: 5px; text-align: left;",
                                                            "Tramos de edad: 0-14, 14-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50-54, 55+")
                                                   )
                                            )
                                       )
                                   )
                            )
                       ),
                       
                       fluidRow(
                            column(12,
                                   div(style = "padding: 15px; margin-bottom: 20px; background-color: #f8f9fa;",
                                       h3("🩺 APGAR", 
                                          style = "color: #191970; font-weight: bold; margin-top: 0; margin-bottom: 15px; 
                              border-bottom: 2px solid #191970; padding-bottom: 10px;"),
                                       
                                       fluidRow(
                                            column(12,
                                                   box(
                                                        title = "Detalle de APGAR por Establecimiento y Mes",
                                                        status = "primary", 
                                                        solidHeader = TRUE, 
                                                        width = 12,
                                                        reactableOutput("tabla_alertas_apgar")
                                                   )
                                            )
                                       )
                                   )
                            )
                       ),
                       
                       fluidRow(
                            column(12,
                                   div(style = "padding: 15px; margin-bottom: 20px; background-color: #f8f9fa;",
                                       h3("🔍 VALIDACIÓN DE CONSISTENCIA: SECCIÓN F vs SECCIÓN G", 
                                          style = "color: #191970; font-weight: bold; margin-top: 0; margin-bottom: 15px; 
                              border-bottom: 2px solid #191970; padding-bottom: 10px;"),
                                       
                                       div(style = "margin-bottom: 15px;",
                                           p(strong("Regla de validación:"), 
                                             "Partos + Cesáreas (Sección F) debe ser igual a Nacidos Vivos + Nacidos Fallecidos (Sección G)"),
                                           p(style = "color: #d9534f; font-size: 14px;",
                                             icon("exclamation-triangle"), 
                                             " Solo se muestran los establecimientos/meses con inconsistencias (Diferencia ≠ 0)")
                                       ),
                                       
                                       h4("Resumen de Errores por Establecimiento", style = "color: #191970; margin-top: 15px;"),
                                       reactableOutput("tabla_resumen_errores"),
                                       br(),
                                       
                                       h4("Detalle por Establecimiento y Mes (Solo Errores)", style = "color: #191970; margin-top: 15px;"),
                                       reactableOutput("tabla_validacion_consistencia")
                                   )
                            )
                       )
               ),
               
               tabItem(tabName = "descarga",
                       fluidRow(
                            box(title = "Descarga de Datos REMASEP", status = "primary", solidHeader = TRUE, width = 12,
                                div(style = "text-align: center;",
                                    icon("database", class = "fa-4x", style = "color: #191970;"),
                                    br(),
                                    p("Este apartado permite descargar un archivo Excel con todas las tablas del dashboard:"),
                                    p(strong("• Resumen Porcentajes"), br(),
                                      strong("• F.1: Partos y Abortos Atendidos"), br(),
                                      strong("• Detalle de Partos"), br(),
                                      strong("• Detalle de Cesáreas"), br(),
                                      strong("• Abortos y otros procedimientos Obstétricos"), br(),
                                      strong("• F.2: Interrupción Voluntaria del Embarazo (IVE)"), br(),
                                      strong("• Detalle IVE por Establecimiento y Edad"), br(),
                                      strong("• G.1: Recién Nacidos Vivos y Fallecidos según Peso al Nacer"), br(),
                                      strong("• G.2: Recién Nacido con Malformación Congénita"), br(),
                                      strong("• G.3: Apgar"), br(),
                                      strong("• Apgar Detalle por Establecimiento"), br(),
                                      strong("• Validación de Consistencia (Detalle)"), br(),
                                      strong("• Validación de Consistencia (Resumen)")),
                                    br(),
                                    p("Los filtros seleccionados (establecimiento y mes) se aplican a la descarga."),
                                    br(),
                                    downloadButton("download_data", "Descargar Datos", 
                                                   class = "btn-primary", 
                                                   style = "font-size: 14px; padding: 8px 25px; background-color: #191970; border-color: #191970; color: white;")
                                )
                            )
                       )
               )
          )
     )
)

# ============================================================
# SERVER - COMPLETO
# ============================================================
server <- function(input, output, session) {

observe({
          req(opciones_establecimientos)
          updateSelectInput(session, "establecimiento", 
                           choices = opciones_establecimientos, 
                           selected = "all")
     })
     
     observeEvent(input$limpiar_filtros, {
          updateSelectInput(session, "establecimiento", selected = "all")
          updateSelectInput(session, "mes", selected = "all")  # ← Este ya funciona, pero se mantiene
     })
     
     observeEvent(input$limpiar_filtros, {
          updateSelectInput(session, "establecimiento", selected = "all")
          updateSelectInput(session, "mes", selected = "all")
     })
     
     datos_filtrados <- reactive({
          data <- remasep25
          
          if (!is.null(input$establecimiento) && input$establecimiento != "all") {
               estab_num <- as.numeric(input$establecimiento)
               if (!is.na(estab_num)) data <- data %>% filter(idestablec == estab_num)
          }
          if (!is.null(input$mes) && length(input$mes) > 0 && !("all" %in% input$mes)) {
               meses_num <- as.numeric(input$mes)
               meses_num <- meses_num[!is.na(meses_num)]
               if (length(meses_num) > 0) data <- data %>% filter(mes %in% meses_num)
          }
          
          data
     })
     
     # ---- TARJETA DE PORCENTAJE DE PARTOS ----
     output$porcentaje_partos_box_ui <- renderUI({
          data <- datos_filtrados()
          
          codigos_partos <- remasep01_f1 %>%
               filter(tabla == "Parto") %>%
               pull(codigopres)
          
          codigos_cesareas <- remasep01_f1 %>%
               filter(tabla == "Cesárea") %>%
               pull(codigopres)
          
          total_partos <- data %>%
               filter(codigopres %in% codigos_partos) %>%
               summarise(total = sum(col01, na.rm = TRUE)) %>%
               pull(total)
          
          total_cesareas <- data %>%
               filter(codigopres %in% codigos_cesareas) %>%
               summarise(total = sum(col01, na.rm = TRUE)) %>%
               pull(total)
          
          if (length(total_partos) == 0) total_partos <- 0
          if (length(total_cesareas) == 0) total_cesareas <- 0
          
          total_general <- total_partos + total_cesareas
          
          porcentaje <- ifelse(total_general > 0, 
                               round((total_partos / total_general) * 100, 1), 
                               0)
          
          div(class = "custom-value-box card-partos",
              div(class = "box-content",
                  div(class = "box-number", paste0(format(porcentaje, decimal.mark = ",", nsmall = 1), "%")),
                  div(class = "box-title", paste0("% Partos (", format(total_partos, big.mark = ".", decimal.mark = ",", scientific = FALSE), ")"))
              ),
              div(class = "box-icon", icon("baby"))
          )
     })
     
     # ---- TARJETA DE PORCENTAJE DE CESÁREAS ----
     output$porcentaje_cesareas_box_ui <- renderUI({
          data <- datos_filtrados()
          
          codigos_partos <- remasep01_f1 %>%
               filter(tabla == "Parto") %>%
               pull(codigopres)
          
          codigos_cesareas <- remasep01_f1 %>%
               filter(tabla == "Cesárea") %>%
               pull(codigopres)
          
          total_partos <- data %>%
               filter(codigopres %in% codigos_partos) %>%
               summarise(total = sum(col01, na.rm = TRUE)) %>%
               pull(total)
          
          total_cesareas <- data %>%
               filter(codigopres %in% codigos_cesareas) %>%
               summarise(total = sum(col01, na.rm = TRUE)) %>%
               pull(total)
          
          if (length(total_partos) == 0) total_partos <- 0
          if (length(total_cesareas) == 0) total_cesareas <- 0
          
          total_general <- total_partos + total_cesareas
          
          porcentaje <- ifelse(total_general > 0, 
                               round((total_cesareas / total_general) * 100, 1), 
                               0)
          
          div(class = "custom-value-box card-cesareas",
              div(class = "box-content",
                  div(class = "box-number", paste0(format(porcentaje, decimal.mark = ",", nsmall = 1), "%")),
                  div(class = "box-title", paste0("% Cesáreas (", format(total_cesareas, big.mark = ".", decimal.mark = ",", scientific = FALSE), ")"))
              ),
              div(class = "box-icon", icon("hospital-user"))
          )
     })
     
     # ---- TARJETA DE TOTAL (100%) ----
     output$porcentaje_total_box_ui <- renderUI({
          data <- datos_filtrados()
          
          codigos_partos <- remasep01_f1 %>%
               filter(tabla == "Parto") %>%
               pull(codigopres)
          
          codigos_cesareas <- remasep01_f1 %>%
               filter(tabla == "Cesárea") %>%
               pull(codigopres)
          
          total_partos <- data %>%
               filter(codigopres %in% codigos_partos) %>%
               summarise(total = sum(col01, na.rm = TRUE)) %>%
               pull(total)
          
          total_cesareas <- data %>%
               filter(codigopres %in% codigos_cesareas) %>%
               summarise(total = sum(col01, na.rm = TRUE)) %>%
               pull(total)
          
          if (length(total_partos) == 0) total_partos <- 0
          if (length(total_cesareas) == 0) total_cesareas <- 0
          
          total_general <- total_partos + total_cesareas
          
          div(class = "custom-value-box card-total",
              div(class = "box-content",
                  div(class = "box-number", "100%"),
                  div(class = "box-title", paste0("Total (", format(total_general, big.mark = ".", decimal.mark = ",", scientific = FALSE), ")"))
              ),
              div(class = "box-icon", icon("calculator"))
          )
     })
     
     # ---- REACTIVE F1 ----
     datos_aggregados_f1 <- reactive({
          data <- datos_filtrados()
          
          res <- data %>%
               left_join(remasep01_f1, by = "codigopres") %>%
               filter(!is.na(tabla)) %>%
               group_by(tabla) %>%
               summarise(
                    total_partos = sum(col01, na.rm = TRUE),
                    prematuro_00a23 = sum(col02, na.rm = TRUE),
                    prematuro_24a28 = sum(col03, na.rm = TRUE),
                    prematuro_29a32 = sum(col04, na.rm = TRUE),
                    prematuro_33a36 = sum(col05, na.rm = TRUE),
                    pueblos_originarios = sum(col06, na.rm = TRUE),
                    inmigrantes = sum(col07, na.rm = TRUE),
                    .groups = "drop"
               )
          
          res <- res %>%
               complete(tabla = orden_tabla_f1,
                        fill = list(total_partos = 0, prematuro_00a23 = 0,
                                    prematuro_24a28 = 0, prematuro_29a32 = 0,
                                    prematuro_33a36 = 0, pueblos_originarios = 0,
                                    inmigrantes = 0))
          
          res <- res %>%
               mutate(tabla = factor(tabla, levels = orden_tabla_f1)) %>%
               arrange(tabla)
          
          total_partos <- res %>%
               filter(tabla %in% categorias_para_total) %>%
               summarise(total = sum(total_partos, na.rm = TRUE)) %>% pull(total)
          if (length(total_partos) == 0) total_partos <- 0
          total_pueblos <- sum(res$pueblos_originarios, na.rm = TRUE)
          total_inmigrantes <- sum(res$inmigrantes, na.rm = TRUE)
          
          total_row <- data.frame(
               tabla = "Total",
               total_partos = total_partos,
               prematuro_00a23 = NA,
               prematuro_24a28 = NA,
               prematuro_29a32 = NA,
               prematuro_33a36 = NA,
               pueblos_originarios = total_pueblos,
               inmigrantes = total_inmigrantes,
               stringsAsFactors = FALSE
          )
          res <- bind_rows(res, total_row)
          res
     })
     
     # ---- REACTIVE F2 ----
     datos_aggregados_f2 <- reactive({
          data <- datos_filtrados()
          
          res <- data %>%
               left_join(remasep01_f2, by = "codigopres") %>%
               filter(!is.na(tabla)) %>%
               group_by(tabla) %>%
               summarise(
                    total = sum(col01, na.rm = TRUE),
                    menor_14 = sum(col02, na.rm = TRUE),
                    de_14a19 = sum(col03, na.rm = TRUE),
                    de_20a24 = sum(col04, na.rm = TRUE),
                    de_25a29 = sum(col05, na.rm = TRUE),
                    de_30a34 = sum(col06, na.rm = TRUE),
                    de_35a39 = sum(col07, na.rm = TRUE),
                    de_40a44 = sum(col08, na.rm = TRUE),
                    de_45a49 = sum(col09, na.rm = TRUE),
                    de_50a54 = sum(col10, na.rm = TRUE),
                    mas_55 = sum(col11, na.rm = TRUE),
                    aborto_hasta_12sem = sum(col12, na.rm = TRUE),
                    aborto_de_12a14sem = sum(col13, na.rm = TRUE),
                    aborto_de_14a21sem6d = sum(col14, na.rm = TRUE),
                    partos_vag_inducidos = sum(col15, na.rm = TRUE),
                    cesareas = sum(col16, na.rm = TRUE),
                    fonasa = sum(col17, na.rm = TRUE),
                    pueblos_originarios = sum(col18, na.rm = TRUE),
                    migrantes = sum(col19, na.rm = TRUE),
                    .groups = "drop"
               )
          
          res <- res %>%
               complete(tabla = orden_tabla_f2,
                        fill = list(total = 0, menor_14 = 0, de_14a19 = 0,
                                    de_20a24 = 0, de_25a29 = 0, de_30a34 = 0,
                                    de_35a39 = 0, de_40a44 = 0, de_45a49 = 0,
                                    de_50a54 = 0, mas_55 = 0, aborto_hasta_12sem = 0,
                                    aborto_de_12a14sem = 0, aborto_de_14a21sem6d = 0,
                                    partos_vag_inducidos = 0, cesareas = 0,
                                    fonasa = 0, pueblos_originarios = 0, migrantes = 0))
          
          res <- res %>%
               mutate(tabla = factor(tabla, levels = orden_tabla_f2)) %>%
               arrange(tabla)
          
          total_fila <- res %>%
               summarise(across(-tabla, ~ sum(.x, na.rm = TRUE))) %>%
               mutate(tabla = "Total", .before = 1)
          
          res <- bind_rows(total_fila, res)
          res
     })
     
     # ---- REACTIVE G1 ----
     datos_aggregados_g1 <- reactive({
          data <- datos_filtrados()
          
          res <- data %>%
               left_join(remasep01_g1, by = "codigopres") %>%
               filter(!is.na(tabla)) %>%
               group_by(tabla) %>%
               summarise(
                    total = sum(col01, na.rm = TRUE),
                    menos_500 = sum(col02, na.rm = TRUE),
                    de_500_a_999 = sum(col03, na.rm = TRUE),
                    de_1000_a_1499 = sum(col04, na.rm = TRUE),
                    de_1500_a_1999 = sum(col05, na.rm = TRUE),
                    de_2000_a_2499 = sum(col06, na.rm = TRUE),
                    de_2500_a_2999 = sum(col07, na.rm = TRUE),
                    de_3000_a_3499 = sum(col08, na.rm = TRUE),
                    de_3500_a_3999 = sum(col09, na.rm = TRUE),
                    de_4000_y_mas = sum(col10, na.rm = TRUE),
                    primeras_muestras = sum(col11, na.rm = TRUE),
                    muestras_repetidas = sum(col12, na.rm = TRUE),
                    .groups = "drop"
               )
          
          res <- res %>%
               complete(tabla = orden_tabla_g1,
                        fill = list(total = 0, menos_500 = 0, de_500_a_999 = 0,
                                    de_1000_a_1499 = 0, de_1500_a_1999 = 0,
                                    de_2000_a_2499 = 0, de_2500_a_2999 = 0,
                                    de_3000_a_3499 = 0, de_3500_a_3999 = 0,
                                    de_4000_y_mas = 0, primeras_muestras = 0,
                                    muestras_repetidas = 0))
          
          res <- res %>%
               mutate(tabla = factor(tabla, levels = orden_tabla_g1)) %>%
               arrange(tabla)
          
          res
     })
     
     # ---- REACTIVE G2 ----
     datos_aggregados_g2 <- reactive({
          data <- datos_filtrados()
          
          res <- data %>%
               left_join(remasep01_g2, by = "codigopres") %>%
               filter(!is.na(tabla)) %>%
               group_by(tabla) %>%
               summarise(
                    total = sum(col01, na.rm = TRUE),
                    .groups = "drop"
               )
          
          res <- res %>%
               complete(tabla = orden_tabla_g2,
                        fill = list(total = 0))
          
          res <- res %>%
               mutate(tabla = factor(tabla, levels = orden_tabla_g2)) %>%
               arrange(tabla)
          
          res
     })
     
     # ---- REACTIVE G3 ----
     datos_aggregados_g3 <- reactive({
          data <- datos_filtrados()
          
          res <- data %>%
               left_join(remasep01_g3, by = "codigopres") %>%
               filter(!is.na(tabla)) %>%
               group_by(tabla) %>%
               summarise(
                    apgar_3 = sum(col01, na.rm = TRUE),
                    apgar_6 = sum(col02, na.rm = TRUE),
                    .groups = "drop"
               )
          
          res <- res %>%
               complete(tabla = orden_tabla_g3,
                        fill = list(apgar_3 = 0, apgar_6 = 0))
          
          res <- res %>%
               mutate(tabla = factor(tabla, levels = orden_tabla_g3)) %>%
               arrange(tabla)
          
          res
     })
     
     # ---- DATOS PARA GRÁFICO DE PARTOS ----
     datos_alertas_partos <- reactive({
          data <- datos_filtrados()
          
          codigos_partos <- remasep01_f1 %>%
               filter(tabla == "Parto") %>%
               pull(codigopres)
          
          data_partos <- data %>%
               filter(codigopres %in% codigos_partos) %>%
               group_by(idestablec, nombre_completo) %>%
               summarise(
                    total = sum(col01, na.rm = TRUE),
                    .groups = "drop"
               ) %>%
               arrange(desc(total))
          
          if (nrow(data_partos) == 0) {
               return(data.frame(idestablec = character(0), total = numeric(0), nombre_completo = character(0)))
          }
          
          data_partos
     })
     
     # ---- DATOS PARA TABLA DE DETALLE PARTOS ----
     datos_detalle_partos <- reactive({
          data <- datos_filtrados()
          
          codigos_partos <- remasep01_f1 %>%
               filter(tabla == "Parto") %>%
               pull(codigopres)
          
          data_detalle <- data %>%
               filter(codigopres %in% codigos_partos) %>%
               left_join(remasep01_f1 %>% select(codigopres, descripcion), by = "codigopres") %>%
               group_by(nombre_completo, descripcion) %>%
               summarise(
                    total = sum(col01, na.rm = TRUE),
                    .groups = "drop"
               ) %>%
               tidyr::pivot_wider(
                    id_cols = nombre_completo,
                    names_from = descripcion,
                    values_from = total,
                    values_fill = 0
               ) %>%
               mutate(
                    Total = rowSums(across(-nombre_completo), na.rm = TRUE)
               ) %>%
               select(nombre_completo, Total, everything()) %>%
               arrange(desc(Total))
          
          idx <- grep("Parto presentación", names(data_detalle))
          if (length(idx) > 0) {
               names(data_detalle)[idx] <- "*Parto presentación cefálica o podálica"
          }
          
          if (nrow(data_detalle) == 0) {
               return(data.frame(nombre_completo = character(0)))
          }
          
          data_detalle
     })
     
     # ---- DATOS PARA GRÁFICO DE CESÁREAS ----
     datos_alertas_cesareas <- reactive({
          data <- datos_filtrados()
          
          codigos_cesareas <- remasep01_f1 %>%
               filter(tabla == "Cesárea") %>%
               pull(codigopres)
          
          data_cesareas <- data %>%
               filter(codigopres %in% codigos_cesareas) %>%
               group_by(idestablec, nombre_completo) %>%
               summarise(
                    total = sum(col01, na.rm = TRUE),
                    .groups = "drop"
               ) %>%
               arrange(desc(total))
          
          if (nrow(data_cesareas) == 0) {
               return(data.frame(idestablec = character(0), total = numeric(0), nombre_completo = character(0)))
          }
          
          data_cesareas
     })
     
     # ---- DATOS PARA TABLA DE DETALLE CESÁREAS ----
     datos_detalle_cesareas <- reactive({
          data <- datos_filtrados()
          
          codigos_cesareas <- remasep01_f1 %>%
               filter(tabla == "Cesárea") %>%
               pull(codigopres)
          
          data_detalle <- data %>%
               filter(codigopres %in% codigos_cesareas) %>%
               left_join(remasep01_f1 %>% select(codigopres, descripcion), by = "codigopres") %>%
               group_by(nombre_completo, descripcion) %>%
               summarise(
                    total = sum(col01, na.rm = TRUE),
                    .groups = "drop"
               ) %>%
               tidyr::pivot_wider(
                    id_cols = nombre_completo,
                    names_from = descripcion,
                    values_from = total,
                    values_fill = 0
               ) %>%
               mutate(
                    Total = rowSums(across(-nombre_completo), na.rm = TRUE)
               ) %>%
               select(nombre_completo, Total, everything()) %>%
               arrange(desc(Total))
          
          if (nrow(data_detalle) == 0) {
               return(data.frame(nombre_completo = character(0)))
          }
          
          data_detalle
     })
     
     # ---- DATOS PARA GRÁFICO DE ALERTAS ABORTOS ----
     datos_alertas_abortos <- reactive({
          data <- datos_filtrados()
          
          codigos_abortos <- remasep01_f1 %>%
               filter(tabla == "Abortos y otros procedimientos Obstétricos") %>%
               pull(codigopres)
          
          data_abortos <- data %>%
               filter(codigopres %in% codigos_abortos) %>%
               group_by(idestablec, nombre_completo) %>%
               summarise(
                    total_abortos = sum(col01, na.rm = TRUE),
                    .groups = "drop"
               ) %>%
               arrange(desc(total_abortos))
          
          if (nrow(data_abortos) == 0) {
               return(data.frame(idestablec = character(0), total_abortos = numeric(0), nombre_completo = character(0)))
          }
          
          data_abortos
     })
     
     # ---- DATOS PARA TABLA DE DETALLE ABORTOS ----
     datos_detalle_abortos <- reactive({
          data <- datos_filtrados()
          
          codigos_abortos <- remasep01_f1 %>%
               filter(tabla == "Abortos y otros procedimientos Obstétricos") %>%
               pull(codigopres)
          
          data_detalle <- data %>%
               filter(codigopres %in% codigos_abortos) %>%
               left_join(remasep01_f1 %>% select(codigopres, descripcion), by = "codigopres") %>%
               group_by(nombre_completo, descripcion) %>%
               summarise(
                    total = sum(col01, na.rm = TRUE),
                    .groups = "drop"
               ) %>%
               tidyr::pivot_wider(
                    id_cols = nombre_completo,
                    names_from = descripcion,
                    values_from = total,
                    values_fill = 0
               ) %>%
               mutate(
                    Total = rowSums(across(-nombre_completo), na.rm = TRUE)
               ) %>%
               select(nombre_completo, Total, everything()) %>%
               arrange(desc(Total))
          
          if (nrow(data_detalle) == 0) {
               return(data.frame(nombre_completo = character(0)))
          }
          
          data_detalle
     })
     
     # ---- DATOS PARA TABLA DETALLADA DE IVE (CON MES Y ORDEN: Establecimiento, Causal, Mes, Total, Tramo Edad) ----
     datos_detalle_ive <- reactive({
          data <- datos_filtrados()
          
          codigos_ive <- remasep01_f2 %>%
               filter(tabla %in% c("Causal N° 1: Por riesgo de la vida de la mujer", 
                                   "Causal N° 2: Por riesgo de inviabilidad fetal", 
                                   "Causal N° 3: Por violación")) %>%
               pull(codigopres)
          
          if (length(codigos_ive) == 0) {
               return(NULL)
          }
          
          data_ive <- data %>%
               filter(codigopres %in% codigos_ive) %>%
               left_join(remasep01_f2 %>% select(codigopres, tabla), by = "codigopres")
          
          if (nrow(data_ive) == 0) {
               return(NULL)
          }
          
          # Transformar datos: primero por establecimiento, causal, mes y tramo de edad
          data_ive_long <- data_ive %>%
               tidyr::pivot_longer(
                    cols = c(menor_14, de_14a19, de_20a24, de_25a29, de_30a34, 
                             de_35a39, de_40a44, de_45a49, de_50a54, mas_55),
                    names_to = "tramo_edad",
                    values_to = "cantidad"
               ) %>%
               filter(cantidad > 0) %>%
               mutate(
                    tramo_edad_label = case_when(
                         tramo_edad == "menor_14" ~ "0-14",
                         tramo_edad == "de_14a19" ~ "14-19",
                         tramo_edad == "de_20a24" ~ "20-24",
                         tramo_edad == "de_25a29" ~ "25-29",
                         tramo_edad == "de_30a34" ~ "30-34",
                         tramo_edad == "de_35a39" ~ "35-39",
                         tramo_edad == "de_40a44" ~ "40-44",
                         tramo_edad == "de_45a49" ~ "45-49",
                         tramo_edad == "de_50a54" ~ "50-54",
                         tramo_edad == "mas_55" ~ "55+",
                         TRUE ~ tramo_edad
                    ),
                    mes_nombre = factor(mes, levels = 1:12, labels = meses_nombres)
               )
          
          # Agrupar por establecimiento, causal, mes y tramo de edad
          data_ive_agrupada <- data_ive_long %>%
               group_by(nombre_completo, tabla, mes_nombre, tramo_edad_label) %>%
               summarise(
                    total = sum(cantidad, na.rm = TRUE),
                    .groups = "drop"
               )
          
          # Verificar que hay datos
          if (nrow(data_ive_agrupada) == 0) {
               return(NULL)
          }
          
          # Reorganizar para tener el formato deseado
          data_ive_final <- data_ive_agrupada %>%
               rename(
                    Establecimiento = nombre_completo,
                    Causal = tabla,
                    Mes = mes_nombre,
                    `Tramo Edad` = tramo_edad_label
               ) %>%
               select(Establecimiento, Causal, Mes, Total = total, `Tramo Edad`) %>%
               arrange(Establecimiento, Causal, Mes, `Tramo Edad`)
          
          data_ive_final
     })
     
     # ---- DATOS PARA EXPORTACIÓN DE IVE ----
     datos_export_ive <- reactive({
          df <- datos_detalle_ive()
          
          if (is.null(df) || nrow(df) == 0) {
               return(data.frame(
                    Establecimiento = character(0),
                    Causal = character(0),
                    Mes = character(0),
                    Total = character(0),
                    `Tramo Edad` = character(0),
                    stringsAsFactors = FALSE,
                    check.names = FALSE
               ))
          }
          
          df_export <- df
          
          for(col in names(df_export)) {
               if (is.numeric(df_export[[col]])) {
                    df_export[[col]] <- sapply(df_export[[col]], function(x) {
                         if (is.na(x) || is.null(x)) return("0")
                         return(format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))
                    })
               }
          }
          
          df_export
     })
     
     # ---- DATOS PARA TABLA DE APGAR ----
     datos_tabla_apgar <- reactive({
          data <- datos_filtrados()
          
          data_apgar <- data %>%
               left_join(remasep01_g3, by = "codigopres") %>%
               filter(!is.na(tabla)) %>%
               group_by(nombre_completo, mes) %>%
               summarise(
                    apgar_3 = sum(col01, na.rm = TRUE),
                    apgar_6 = sum(col02, na.rm = TRUE),
                    .groups = "drop"
               ) %>%
               mutate(
                    mes_nombre = factor(mes, levels = 1:12, labels = meses_nombres)
               ) %>%
               arrange(nombre_completo, mes) %>%
               select(
                    Establecimiento = nombre_completo,
                    Mes = mes_nombre,
                    `APGAR ≤ 3 (al minuto)` = apgar_3,
                    `APGAR ≤ 6 (a los 5 minutos)` = apgar_6
               )
          
          if (nrow(data_apgar) == 0) {
               return(data.frame(
                    Establecimiento = character(0),
                    Mes = character(0),
                    `APGAR ≤ 3 (al minuto)` = numeric(0),
                    `APGAR ≤ 6 (a los 5 minutos)` = numeric(0)
               ))
          }
          
          data_apgar
     })
     
     # ---- DATOS PARA EXPORTACIÓN DE APGAR DETALLE ----
     datos_export_apgar_detalle <- reactive({
          data <- datos_filtrados()
          
          data_apgar <- data %>%
               left_join(remasep01_g3, by = "codigopres") %>%
               filter(!is.na(tabla)) %>%
               group_by(idestablec, nombre_completo, mes) %>%
               summarise(
                    apgar_3 = sum(col01, na.rm = TRUE),
                    apgar_6 = sum(col02, na.rm = TRUE),
                    .groups = "drop"
               ) %>%
               mutate(
                    mes_nombre = factor(mes, levels = 1:12, labels = meses_nombres)
               ) %>%
               arrange(idestablec, mes)
          
          if (nrow(data_apgar) == 0) {
               return(data.frame(
                    Establecimiento = character(0),
                    Mes = character(0),
                    `APGAR ≤ 3 (al minuto)` = character(0),
                    `APGAR ≤ 6 (a los 5 minutos)` = character(0)
               ))
          }
          
          df_export <- data_apgar %>%
               select(Establecimiento = nombre_completo, Mes = mes_nombre, apgar_3, apgar_6)
          
          for(col in names(df_export)) {
               if (is.numeric(df_export[[col]])) {
                    df_export[[col]] <- sapply(df_export[[col]], function(x) {
                         if (is.na(x) || is.null(x)) return("0")
                         return(format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))
                    })
               }
          }
          
          df_export <- df_export %>%
               rename(
                    `APGAR ≤ 3 (al minuto)` = apgar_3,
                    `APGAR ≤ 6 (a los 5 minutos)` = apgar_6
               )
          
          df_export
     })
     
     # ---- DATOS PARA EXPORTACIÓN DE PORCENTAJES ----
     datos_export_porcentajes <- reactive({
          data <- datos_filtrados()
          
          codigos_partos <- remasep01_f1 %>%
               filter(tabla == "Parto") %>%
               pull(codigopres)
          
          codigos_cesareas <- remasep01_f1 %>%
               filter(tabla == "Cesárea") %>%
               pull(codigopres)
          
          total_partos <- data %>%
               filter(codigopres %in% codigos_partos) %>%
               summarise(total = sum(col01, na.rm = TRUE)) %>%
               pull(total)
          
          total_cesareas <- data %>%
               filter(codigopres %in% codigos_cesareas) %>%
               summarise(total = sum(col01, na.rm = TRUE)) %>%
               pull(total)
          
          if (length(total_partos) == 0) total_partos <- 0
          if (length(total_cesareas) == 0) total_cesareas <- 0
          
          total_general <- total_partos + total_cesareas
          
          porcentaje_partos <- ifelse(total_general > 0, 
                                      round((total_partos / total_general) * 100, 1), 
                                      0)
          
          porcentaje_cesareas <- ifelse(total_general > 0, 
                                        round((total_cesareas / total_general) * 100, 1), 
                                        0)
          
          nombre_establecimiento <- ifelse(
               input$establecimiento == "all", 
               "Todos los establecimientos",
               names(opciones_establecimientos)[opciones_establecimientos == input$establecimiento][1]
          )
          
          if (is.na(nombre_establecimiento) || is.null(nombre_establecimiento)) {
               nombre_establecimiento <- input$establecimiento
          }
          
          meses_texto <- ifelse("all" %in% input$mes || is.null(input$mes) || length(input$mes) == 0, 
                                "Todos los meses", 
                                paste(meses_nombres[as.numeric(input$mes)], collapse = ", "))
          
          df_resultado <- data.frame(
               `Métrica` = c("Total Partos", "Total Cesáreas", "Total General", "% Partos", "% Cesáreas"),
               `Valor` = c(
                    fmt_export(total_partos),
                    fmt_export(total_cesareas),
                    fmt_export(total_general),
                    paste0(format(porcentaje_partos, decimal.mark = ",", nsmall = 1), "%"),
                    paste0(format(porcentaje_cesareas, decimal.mark = ",", nsmall = 1), "%")
               ),
               stringsAsFactors = FALSE
          )
          
          df_info <- data.frame(
               `Métrica` = c("Establecimiento", "Meses", "Fecha de exportación"),
               `Valor` = c(
                    nombre_establecimiento,
                    meses_texto,
                    format(Sys.Date(), "%d-%m-%Y %H:%M")
               ),
               stringsAsFactors = FALSE
          )
          
          df_resultado <- rbind(df_resultado, df_info)
          
          df_resultado
     })
     
     # ---- DATOS PARA VALIDACIÓN DE CONSISTENCIA F vs G ----
     datos_validacion_consistencia <- reactive({
          data <- datos_filtrados()
          
          codigos_partos <- remasep01_f1 %>%
               filter(tabla == "Parto") %>%
               pull(codigopres)
          
          codigos_cesareas <- remasep01_f1 %>%
               filter(tabla == "Cesárea") %>%
               pull(codigopres)
          
          codigos_nacidos_vivos <- remasep01_g1 %>%
               filter(tabla == "Nacidos vivos") %>%
               pull(codigopres)
          
          codigos_nacidos_fallecidos <- remasep01_g1 %>%
               filter(tabla == "Nacidos fallecidos") %>%
               pull(codigopres)
          
          validacion <- data %>%
               group_by(idestablec, nombre_completo, mes) %>%
               summarise(
                    total_partos = sum(ifelse(codigopres %in% codigos_partos, col01, 0), na.rm = TRUE),
                    total_cesareas = sum(ifelse(codigopres %in% codigos_cesareas, col01, 0), na.rm = TRUE),
                    total_f = total_partos + total_cesareas,
                    nacidos_vivos = sum(ifelse(codigopres %in% codigos_nacidos_vivos, col01, 0), na.rm = TRUE),
                    nacidos_fallecidos = sum(ifelse(codigopres %in% codigos_nacidos_fallecidos, col01, 0), na.rm = TRUE),
                    total_g = nacidos_vivos + nacidos_fallecidos,
                    .groups = "drop"
               ) %>%
               mutate(
                    diferencia = total_f - total_g,
                    mes_nombre = factor(mes, levels = 1:12, labels = meses_nombres)
               ) %>%
               arrange(idestablec, mes) %>%
               select(
                    Establecimiento = nombre_completo,
                    Mes = mes_nombre,
                    Partos = total_partos,
                    Cesareas = total_cesareas,
                    `Nacidos Vivos` = nacidos_vivos,
                    `Nacidos Fallecidos` = nacidos_fallecidos,
                    `Partos+Cesareas` = total_f,
                    `Total Nacidos` = total_g,
                    Diferencia = diferencia
               ) %>%
               filter(!is.na(Diferencia) & Diferencia != 0)
          
          if (nrow(validacion) == 0) {
               return(data.frame(
                    Establecimiento = character(0),
                    Mes = character(0),
                    Partos = numeric(0),
                    Cesareas = numeric(0),
                    `Nacidos Vivos` = numeric(0),
                    `Nacidos Fallecidos` = numeric(0),
                    `Partos+Cesareas` = numeric(0),
                    `Total Nacidos` = numeric(0),
                    Diferencia = numeric(0),
                    stringsAsFactors = FALSE
               ))
          }
          
          validacion
     })
     
     # ---- RESUMEN DE ERRORES POR ESTABLECIMIENTO ----
     datos_resumen_errores <- reactive({
          df <- datos_validacion_consistencia()
          
          if (nrow(df) == 0) {
               return(data.frame(
                    Establecimiento = character(0),
                    `Total Errores` = numeric(0),
                    `Total Partos+Cesareas` = numeric(0),
                    `Total Nacidos` = numeric(0),
                    `Diferencia Acumulada` = numeric(0),
                    stringsAsFactors = FALSE
               ))
          }
          
          df %>%
               group_by(Establecimiento) %>%
               summarise(
                    `Total Errores` = n(),
                    `Total Partos+Cesareas` = sum(`Partos+Cesareas`, na.rm = TRUE),
                    `Total Nacidos` = sum(`Total Nacidos`, na.rm = TRUE),
                    `Diferencia Acumulada` = sum(Diferencia, na.rm = TRUE),
                    .groups = "drop"
               ) %>%
               arrange(desc(`Total Errores`))
     })
     
     # ---- RENDER F1 ----
     output$tabla_resumen_f1 <- renderReactable({
          df <- datos_aggregados_f1()
          req(df)
          
          df_present <- df %>%
               rename(
                    Partos = tabla,
                    Total = total_partos,
                    `Prematuro <24 sem` = prematuro_00a23,
                    `Prematuro 24-28 sem` = prematuro_24a28,
                    `Prematuro 29-32 sem` = prematuro_29a32,
                    `Prematuro 33-36 sem` = prematuro_33a36,
                    `Pueblos Originarios` = pueblos_originarios,
                    `Inmigrantes` = inmigrantes
               )
          
          cols_prematuros <- c("Prematuro <24 sem", "Prematuro 24-28 sem", 
                               "Prematuro 29-32 sem", "Prematuro 33-36 sem")
          
          fmt <- function(x) {
               if (is.na(x)) return("0")
               format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
          }
          
          es_destacada <- function(categoria) {
               categoria %in% categorias_para_total | categoria == "Total"
          }
          
          col_defs <- list()
          col_defs[["Partos"]] <- reactable::colDef(
               name = "Partos", 
               sticky = "left",
               align = "left",
               style = function(value, index) {
                    categoria <- df_present$Partos[index]
                    if (es_destacada(categoria)) {
                         list(backgroundColor = "khaki", color = "black", fontWeight = "bold")
                    } else {
                         list(backgroundColor = "#191970", color = "white", fontWeight = "bold")
                    }
               },
               headerStyle = list(backgroundColor = "#191970", color = "white")
          )
          
          for (col in cols_prematuros) {
               col_defs[[col]] <- reactable::colDef(
                    cell = function(value, index) {
                         categoria <- df_present$Partos[index]
                         if (categoria == "Total") {
                              return(htmltools::div("-", class = "cell-no-aplica"))
                         } else if (categoria %in% categorias_con_prematuros) {
                              return(fmt(value))
                         } else {
                              return(htmltools::div("-", class = "cell-no-aplica"))
                         }
                    },
                    align = "center"
               )
          }
          
          col_defs[["Total"]] <- reactable::colDef(cell = function(value) fmt(value), align = "center")
          col_defs[["Pueblos Originarios"]] <- reactable::colDef(cell = function(value) fmt(value), align = "center")
          col_defs[["Inmigrantes"]] <- reactable::colDef(cell = function(value) fmt(value), align = "center")
          
          row_style <- function(index) {
               categoria <- df_present$Partos[index]
               if (es_destacada(categoria)) return(list(backgroundColor = "khaki"))
               return(NULL)
          }
          
          reactable(
               df_present, columns = col_defs,
               defaultColDef = reactable::colDef(align = "center", headerStyle = list(backgroundColor = "#191970", color = "white", fontWeight = "bold")),
               rowStyle = row_style, striped = TRUE, bordered = TRUE, highlight = TRUE, pagination = FALSE
          )
     })
     
     # ---- RENDER F2 ----
     output$tabla_resumen_f2 <- renderReactable({
          df <- datos_aggregados_f2()
          req(df)
          
          df_present <- df %>%
               rename(
                    CAUSALES = tabla,
                    Total = total,
                    `Edad en años menor a 14` = menor_14,
                    `Edad en años 14-19` = de_14a19,
                    `Edad en años 20-24` = de_20a24,
                    `Edad en años 25-29` = de_25a29,
                    `Edad en años 30-34` = de_30a34,
                    `Edad en años 35-39` = de_35a39,
                    `Edad en años 40-44` = de_40a44,
                    `Edad en años 45-49` = de_45a49,
                    `Edad en años 50-54` = de_50a54,
                    `Edad en años 55 y más` = mas_55,
                    `Aborto hasta 12sem 0dias` = aborto_hasta_12sem,
                    `Aborto de 12sem 1dia hasta 14sem 0dias` = aborto_de_12a14sem,
                    `Aborto de 14sem 1dia hasta 21sem 6dias` = aborto_de_14a21sem6d,
                    `Partos vaginales inducidos` = partos_vag_inducidos,
                    `Cesáreas` = cesareas,
                    `Beneficiarias FONASA` = fonasa,
                    `Pueblos Originarios` = pueblos_originarios,
                    `Migrantes` = migrantes
               )
          
          fmt <- function(x) {
               if (is.na(x)) return("0")
               format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
          }
          
          cols_no_aplican_violacion <- c("Aborto de 14sem 1dia hasta 21sem 6dias", 
                                         "Partos vaginales inducidos", 
                                         "Cesáreas")
          
          todas_las_cols <- names(df_present)[-1]
          
          col_defs <- list()
          
          col_defs[["CAUSALES"]] <- reactable::colDef(
               name = "CAUSALES",
               sticky = "left",
               align = "left",
               style = function(value, index) {
                    if (value == "Total") {
                         list(backgroundColor = "khaki", fontWeight = "bold")
                    } else {
                         list(backgroundColor = "#191970", color = "white", fontWeight = "bold")
                    }
               },
               headerStyle = list(backgroundColor = "#191970", color = "white")
          )
          
          nuevas_cols <- lapply(todas_las_cols, function(col_name) {
               reactable::colDef(
                    cell = function(value, index) {
                         categoria <- df_present[["CAUSALES"]][index]
                         if (categoria == "Causal N° 3: Por violación" && col_name %in% cols_no_aplican_violacion) {
                              return("-")
                         } else {
                              return(fmt(value))
                         }
                    },
                    align = "center",
                    style = function(value, index) {
                         categoria <- df_present[["CAUSALES"]][index]
                         if (categoria == "Causal N° 3: Por violación" && col_name %in% cols_no_aplican_violacion) {
                              return(list(backgroundColor = "#555555", color = "white", textAlign = "center"))
                         }
                         return(NULL)
                    }
               )
          })
          names(nuevas_cols) <- todas_las_cols
          col_defs <- c(col_defs, nuevas_cols)
          
          row_style <- function(index) {
               categoria <- df_present[["CAUSALES"]][index]
               if (categoria == "Total") return(list(backgroundColor = "khaki"))
               return(NULL)
          }
          
          reactable(
               df_present,
               columns = col_defs,
               defaultColDef = reactable::colDef(
                    headerStyle = list(backgroundColor = "#191970", color = "white", fontWeight = "bold")
               ),
               rowStyle = row_style,
               striped = TRUE,
               bordered = TRUE,
               highlight = TRUE,
               pagination = FALSE
          )
     })
     
     # ---- RENDER G1 ----
     output$tabla_resumen_g1 <- renderReactable({
          df <- datos_aggregados_g1()
          req(df)
          
          df_present <- df %>%
               rename(
                    TIPO = tabla,
                    Total = total,
                    `Peso al nacer menor a 500 gr` = menos_500,
                    `Peso al nacer de 500 a 999 gr` = de_500_a_999,
                    `Peso al nacer 1000 a 1499 gr` = de_1000_a_1499,
                    `Peso al nacer 1500 a 1999 gr` = de_1500_a_1999,
                    `Peso al nacer 2000 a 2499 gr` = de_2000_a_2499,
                    `Peso al nacer 2500 a 2999 gr` = de_2500_a_2999,
                    `Peso al nacer 3000 a 3499 gr` = de_3000_a_3499,
                    `Peso al nacer 3500 a 3999 gr` = de_3500_a_3999,
                    `Peso al nacer 4000 gr y más` = de_4000_y_mas,
                    `*PKU e HC - Primeras Muestras` = primeras_muestras,
                    `*PKU e HC - Muestras Repetidas` = muestras_repetidas
               )
          
          fmt <- function(x) {
               if (is.na(x)) return("0")
               format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
          }
          
          todas_las_cols <- names(df_present)[-1]
          
          col_defs <- list()
          
          col_defs[["TIPO"]] <- reactable::colDef(
               name = "TIPO",
               sticky = "left",
               align = "left",
               style = function(value, index) {
                    list(backgroundColor = "#191970", color = "white", fontWeight = "bold")
               },
               headerStyle = list(backgroundColor = "#191970", color = "white")
          )
          
          nuevas_cols <- lapply(todas_las_cols, function(col_name) {
               reactable::colDef(
                    cell = function(value, index) {
                         categoria <- df_present[["TIPO"]][index]
                         if (categoria == "Nacidos fallecidos" && 
                             (col_name == "*PKU e HC - Primeras Muestras" || 
                              col_name == "*PKU e HC - Muestras Repetidas")) {
                              return(htmltools::div("-", class = "cell-no-aplica"))
                         } else {
                              return(fmt(value))
                         }
                    },
                    align = "center"
               )
          })
          names(nuevas_cols) <- todas_las_cols
          col_defs <- c(col_defs, nuevas_cols)
          
          reactable(
               df_present,
               columns = col_defs,
               defaultColDef = reactable::colDef(
                    headerStyle = list(backgroundColor = "#191970", color = "white", fontWeight = "bold")
               ),
               striped = TRUE,
               bordered = TRUE,
               highlight = TRUE,
               pagination = FALSE
          )
     })
     
     # ---- RENDER G2 ----
     output$tabla_resumen_g2 <- renderReactable({
          df <- datos_aggregados_g2()
          req(df)
          
          df_present <- df %>%
               rename(
                    TIPO = tabla,
                    Total = total
               )
          
          fmt <- function(x) {
               if (is.na(x)) return("0")
               format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
          }
          
          col_defs <- list()
          
          col_defs[["TIPO"]] <- reactable::colDef(
               name = "TIPO",
               sticky = "left",
               align = "left",
               style = function(value, index) {
                    list(backgroundColor = "#191970", color = "white", fontWeight = "bold")
               },
               headerStyle = list(backgroundColor = "#191970", color = "white")
          )
          
          col_defs[["Total"]] <- reactable::colDef(
               cell = function(value) fmt(value),
               align = "center"
          )
          
          reactable(
               df_present,
               columns = col_defs,
               defaultColDef = reactable::colDef(
                    headerStyle = list(backgroundColor = "#191970", color = "white", fontWeight = "bold")
               ),
               striped = TRUE,
               bordered = TRUE,
               highlight = TRUE,
               pagination = FALSE
          )
     })
     
     # ---- RENDER G3 ----
     output$tabla_resumen_g3 <- renderReactable({
          df <- datos_aggregados_g3()
          req(df)
          
          df_present <- df %>%
               rename(
                    TIPO = tabla,
                    `APGAR ≤ 3 (al minuto)` = apgar_3,
                    `APGAR ≤ 6 (a los 5 minutos)` = apgar_6
               )
          
          fmt <- function(x) {
               if (is.na(x)) return("0")
               format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
          }
          
          col_defs <- list()
          
          col_defs[["TIPO"]] <- reactable::colDef(
               name = "TIPO",
               sticky = "left",
               align = "left",
               style = function(value, index) {
                    list(backgroundColor = "#191970", color = "white", fontWeight = "bold")
               },
               headerStyle = list(backgroundColor = "#191970", color = "white")
          )
          
          col_defs[["APGAR ≤ 3 (al minuto)"]] <- reactable::colDef(
               cell = function(value) fmt(value),
               align = "center"
          )
          
          col_defs[["APGAR ≤ 6 (a los 5 minutos)"]] <- reactable::colDef(
               cell = function(value) fmt(value),
               align = "center"
          )
          
          reactable(
               df_present,
               columns = col_defs,
               defaultColDef = reactable::colDef(
                    headerStyle = list(backgroundColor = "#191970", color = "white", fontWeight = "bold")
               ),
               striped = TRUE,
               bordered = TRUE,
               highlight = TRUE,
               pagination = FALSE
          )
     })
     
     # ---- RENDER TABLA DE APGAR EN ALERTAS ----
     output$tabla_alertas_apgar <- renderReactable({
          df <- datos_tabla_apgar()
          
          if (nrow(df) == 0) {
               return(reactable(
                    data.frame(Mensaje = "No hay datos disponibles para los filtros seleccionados"),
                    columns = list(Mensaje = reactable::colDef(name = "", align = "center")),
                    defaultColDef = colDef(
                         headerStyle = list(backgroundColor = "#191970", color = "white", fontWeight = "bold")
                    )
               ))
          }
          
          df_formateado <- df %>%
               mutate(
                    `APGAR ≤ 3 (al minuto)` = format(`APGAR ≤ 3 (al minuto)`, big.mark = ".", decimal.mark = ",", scientific = FALSE),
                    `APGAR ≤ 6 (a los 5 minutos)` = format(`APGAR ≤ 6 (a los 5 minutos)`, big.mark = ".", decimal.mark = ",", scientific = FALSE)
               )
          
          n_filas <- nrow(df_formateado)
          altura_dinamica <- min(max(n_filas * 35 + 50, 100), 500)
          
          reactable(
               df_formateado,
               columns = list(
                    Establecimiento = colDef(
                         name = "Establecimiento",
                         style = list(backgroundColor = "#191970", color = "white", fontWeight = "bold"),
                         minWidth = 200
                    ),
                    Mes = colDef(
                         name = "Mes",
                         align = "center",
                         minWidth = 100
                    ),
                    `APGAR ≤ 3 (al minuto)` = colDef(
                         name = "APGAR ≤ 3 (al minuto)",
                         align = "center",
                         minWidth = 150
                    ),
                    `APGAR ≤ 6 (a los 5 minutos)` = colDef(
                         name = "APGAR ≤ 6 (a los 5 minutos)",
                         align = "center",
                         minWidth = 150
                    )
               ),
               defaultColDef = colDef(
                    headerStyle = list(backgroundColor = "#191970", color = "white", fontWeight = "bold")
               ),
               striped = TRUE,
               bordered = TRUE,
               highlight = TRUE,
               pagination = FALSE,
               height = altura_dinamica,
               defaultSorted = list(Establecimiento = "asc", Mes = "asc")
          )
     })
     
     # ---- RENDER TABLA DETALLADA DE IVE (ORDEN: Establecimiento, Causal, Mes, Total, Tramo Edad) ----
     output$tabla_detalle_ive <- renderReactable({
          df <- datos_detalle_ive()
          
          # Definir las columnas fijas
          columnas_fijas <- c("Establecimiento", "Causal", "Mes", "Total", "Tramo Edad")
          
          if (is.null(df) || nrow(df) == 0) {
               # Crear dataframe vacío con estructura
               df_vacio <- data.frame(
                    Establecimiento = "No hay registros de IVE para los filtros seleccionados",
                    Causal = "",
                    Mes = "",
                    Total = "",
                    `Tramo Edad` = "",
                    stringsAsFactors = FALSE,
                    check.names = FALSE
               )
               
               # Altura dinámica para el mensaje
               altura_dinamica <- 100
               
               return(reactable(
                    df_vacio,
                    columns = list(
                         Establecimiento = colDef(
                              name = "Establecimiento",
                              style = function(value) {
                                   if (value == "No hay registros de IVE para los filtros seleccionados") {
                                        list(color = "#191970", fontStyle = "italic", textAlign = "center")
                                   } else {
                                        list(backgroundColor = "#191970", color = "white", fontWeight = "bold")
                                   }
                              },
                              minWidth = 250
                         ),
                         Causal = colDef(
                              name = "Causal",
                              align = "left",
                              minWidth = 200
                         ),
                         Mes = colDef(
                              name = "Mes",
                              align = "center",
                              minWidth = 100
                         ),
                         Total = colDef(
                              name = "Total",
                              align = "center",
                              style = list(backgroundColor = "khaki", fontWeight = "bold"),
                              minWidth = 80
                         ),
                         `Tramo Edad` = colDef(
                              name = "Tramo Edad",
                              align = "center",
                              minWidth = 100
                         )
                    ),
                    defaultColDef = colDef(
                         headerStyle = list(backgroundColor = "#191970", color = "white", fontWeight = "bold")
                    ),
                    striped = TRUE,
                    bordered = TRUE,
                    highlight = TRUE,
                    pagination = FALSE,
                    height = altura_dinamica
               ))
          }
          
          # Formatear números (Total es numérico)
          df_formateado <- df %>%
               mutate(
                    Total = format(Total, big.mark = ".", decimal.mark = ",", scientific = FALSE)
               )
          
          # Calcular altura dinámica basada en el número de filas
          n_filas <- nrow(df_formateado)
          
          altura_dinamica <- min(max(n_filas * 35 + 50, 100), 500)
          
          reactable(
               df_formateado,
               columns = list(
                    Establecimiento = colDef(
                         name = "Establecimiento",
                         style = list(backgroundColor = "#191970", color = "white", fontWeight = "bold"),
                         minWidth = 250
                    ),
                    Causal = colDef(
                         name = "Causal",
                         align = "left",
                         minWidth = 200
                    ),
                    Mes = colDef(
                         name = "Mes",
                         align = "center",
                         minWidth = 100
                    ),
                    Total = colDef(
                         name = "Total",
                         align = "center",
                         style = list(backgroundColor = "khaki", fontWeight = "bold"),
                         minWidth = 80
                    ),
                    `Tramo Edad` = colDef(
                         name = "Tramo Edad",
                         align = "center",
                         minWidth = 100
                    )
               ),
               defaultColDef = colDef(
                    headerStyle = list(backgroundColor = "#191970", color = "white", fontWeight = "bold")
               ),
               striped = TRUE,
               bordered = TRUE,
               highlight = TRUE,
               pagination = FALSE,
               height = altura_dinamica,
               defaultSorted = list(Establecimiento = "asc", Causal = "asc", Mes = "asc", `Tramo Edad` = "asc")
          )
     })
     
     # ---- RENDER TABLA DE VALIDACIÓN ----
     output$tabla_validacion_consistencia <- renderReactable({
          df <- datos_validacion_consistencia()
          
          if (nrow(df) == 0) {
               return(reactable(
                    data.frame(Mensaje = "✅ No se encontraron inconsistencias en los datos"),
                    columns = list(Mensaje = reactable::colDef(name = "", align = "center")),
                    defaultColDef = colDef(
                         headerStyle = list(backgroundColor = "#191970", color = "white", fontWeight = "bold")
                    )
               ))
          }
          
          df_formateado <- df %>%
               mutate(
                    Partos = format(Partos, big.mark = ".", decimal.mark = ","),
                    Cesareas = format(Cesareas, big.mark = ".", decimal.mark = ","),
                    `Nacidos Vivos` = format(`Nacidos Vivos`, big.mark = ".", decimal.mark = ","),
                    `Nacidos Fallecidos` = format(`Nacidos Fallecidos`, big.mark = ".", decimal.mark = ","),
                    `Partos+Cesareas` = format(`Partos+Cesareas`, big.mark = ".", decimal.mark = ","),
                    `Total Nacidos` = format(`Total Nacidos`, big.mark = ".", decimal.mark = ","),
                    Diferencia = format(Diferencia, big.mark = ".", decimal.mark = ",")
               )
          
          reactable(
               df_formateado,
               columns = list(
                    Establecimiento = colDef(
                         name = "Establecimiento",
                         style = list(backgroundColor = "#191970", color = "white", fontWeight = "bold"),
                         minWidth = 200
                    ),
                    Mes = colDef(
                         name = "Mes",
                         align = "center"
                    ),
                    Partos = colDef(
                         name = "Partos",
                         align = "center"
                    ),
                    Cesareas = colDef(
                         name = "Cesáreas",
                         align = "center"
                    ),
                    `Nacidos Vivos` = colDef(
                         name = "Nacidos Vivos",
                         align = "center"
                    ),
                    `Nacidos Fallecidos` = colDef(
                         name = "Nacidos Fallecidos",
                         align = "center"
                    ),
                    `Partos+Cesareas` = colDef(
                         name = "Partos+Cesáreas",
                         align = "center"
                    ),
                    `Total Nacidos` = colDef(
                         name = "Total Nacidos",
                         align = "center"
                    ),
                    Diferencia = colDef(
                         name = "Diferencia",
                         align = "center"
                    )
               ),
               defaultColDef = colDef(
                    headerStyle = list(backgroundColor = "#191970", color = "white", fontWeight = "bold")
               ),
               striped = TRUE,
               bordered = TRUE,
               highlight = TRUE,
               pagination = FALSE,
               height = 500
          )
     })
     
     # ---- RENDER TABLA RESUMEN DE ERRORES ----
     output$tabla_resumen_errores <- renderReactable({
          df <- datos_resumen_errores()
          
          if (nrow(df) == 0) {
               return(reactable(
                    data.frame(Mensaje = "✅ Todos los establecimientos tienen datos consistentes"),
                    columns = list(Mensaje = reactable::colDef(name = "", align = "center")),
                    defaultColDef = colDef(
                         headerStyle = list(backgroundColor = "#191970", color = "white", fontWeight = "bold")
                    )
               ))
          }
          
          reactable(
               df,
               columns = list(
                    Establecimiento = colDef(
                         name = "Establecimiento",
                         style = list(backgroundColor = "#191970", color = "white", fontWeight = "bold"),
                         minWidth = 200
                    ),
                    `Total Errores` = colDef(
                         name = "Total Errores",
                         align = "center"
                    ),
                    `Total Partos+Cesareas` = colDef(
                         name = "Total Partos+Cesáreas",
                         align = "center"
                    ),
                    `Total Nacidos` = colDef(
                         name = "Total Nacidos",
                         align = "center"
                    ),
                    `Diferencia Acumulada` = colDef(
                         name = "Diferencia Acumulada",
                         align = "center"
                    )
               ),
               defaultColDef = colDef(
                    headerStyle = list(backgroundColor = "#191970", color = "white", fontWeight = "bold")
               ),
               striped = TRUE,
               bordered = TRUE,
               highlight = TRUE,
               pagination = FALSE
          )
     })
     
     # ---- GRÁFICO DE ALERTAS PARTOS ----
     output$grafico_alertas_partos <- renderPlotly({
          df <- datos_alertas_partos()
          
          if (is.null(df) || nrow(df) == 0 || !"total" %in% names(df)) {
               plot_ly() %>%
                    layout(
                         annotations = list(
                              text = "No hay datos disponibles para los filtros seleccionados",
                              x = 0.5, y = 0.5,
                              xref = "paper", yref = "paper",
                              showarrow = FALSE,
                              font = list(size = 16, color = "#191970")
                         )
                    )
          } else {
               etiquetas <- format(df$total, big.mark = ".", decimal.mark = ",", scientific = FALSE)
               
               nombre_establecimiento_seleccionado <- ifelse(
                    input$establecimiento == "all", 
                    "Todos los establecimientos",
                    names(opciones_establecimientos)[opciones_establecimientos == input$establecimiento][1]
               )
               
               if (is.na(nombre_establecimiento_seleccionado) || is.null(nombre_establecimiento_seleccionado)) {
                    nombre_establecimiento_seleccionado <- input$establecimiento
               }
               
               max_valor <- max(df$total, na.rm = TRUE)
               if (max_valor == 0 || is.na(max_valor)) max_valor <- 1
               
               plot_ly(
                    df,
                    x = ~total,
                    y = ~reorder(nombre_completo, total),
                    type = "bar",
                    orientation = "h",
                    marker = list(
                         color = "#191970",
                         line = list(color = "#191970", width = 1)
                    ),
                    text = etiquetas,
                    textposition = "outside",
                    textfont = list(
                         color = "#191970",
                         size = 14,
                         family = "Arial, sans-serif"
                    ),
                    hoverinfo = "text",
                    hovertext = ~paste0(
                         "Establecimiento: ", nombre_completo, "<br>",
                         "Partos: ", format(total, big.mark = ".", decimal.mark = ",", scientific = FALSE)
                    ),
                    hovertemplate = "%{hovertext}<extra></extra>"
               ) %>%
                    layout(
                         title = list(
                              text = paste0(
                                   "Partos por establecimiento",
                                   "<br><sup>Filtros aplicados: ",
                                   nombre_establecimiento_seleccionado,
                                   " | ",
                                   ifelse("all" %in% input$mes || is.null(input$mes) || length(input$mes) == 0, 
                                          "Todos los meses", 
                                          paste(meses_nombres[as.numeric(input$mes)], collapse = ", ")),
                                   "</sup>"
                              ),
                              font = list(color = "#191970", size = 14)
                         ),
                         xaxis = list(
                              title = "Cantidad",
                              gridcolor = "#e0e0e0",
                              tickformat = ",.0f",
                              range = c(0, max_valor * 1.3)
                         ),
                         yaxis = list(
                              title = "",
                              tickfont = list(size = 9),
                              gridcolor = "#e0e0e0"
                         ),
                         plot_bgcolor = "white",
                         paper_bgcolor = "white",
                         hoverlabel = list(
                              bgcolor = "#191970",
                              font = list(color = "white")
                         ),
                         margin = list(b = 60, l = 150, r = 40, t = 80),
                         showlegend = FALSE
                    ) %>%
                    config(displayModeBar = TRUE, modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d"))
          }
     })
     
     # ---- TABLA DE DETALLE PARTOS ----
     output$tabla_detalle_partos <- renderReactable({
          df <- datos_detalle_partos()
          
          if (nrow(df) == 0) {
               return(reactable(
                    data.frame(Mensaje = "No hay datos disponibles"),
                    columns = list(Mensaje = reactable::colDef(name = ""))
               ))
          }
          
          fmt_tabla <- function(x) {
               if (is.numeric(x)) {
                    return(format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))
               }
               return(x)
          }
          
          df_formateado <- df %>%
               mutate(across(where(is.numeric), fmt_tabla))
          
          otras_columnas <- names(df_formateado)[!(names(df_formateado) %in% c("nombre_completo", "Total"))]
          
          col_defs <- list(
               nombre_completo = reactable::colDef(
                    name = "Establecimiento",
                    align = "left",
                    style = list(backgroundColor = "#191970", color = "white", fontWeight = "bold"),
                    minWidth = 100
               ),
               Total = reactable::colDef(
                    name = "Total",
                    style = list(backgroundColor = "khaki", fontWeight = "bold"),
                    align = "center"
               )
          )
          
          for (col in otras_columnas) {
               col_defs[[col]] <- reactable::colDef(
                    name = col,
                    align = "center",
                    minWidth = 50
               )
          }
          
          reactable(
               df_formateado,
               columns = col_defs,
               defaultColDef = reactable::colDef(
                    headerStyle = list(backgroundColor = "#191970", color = "white", fontWeight = "bold")
               ),
               striped = TRUE,
               bordered = TRUE,
               highlight = TRUE,
               pagination = FALSE,
               height = 400
          )
     })
     
     # ---- GRÁFICO DE ALERTAS CESÁREAS ----
     output$grafico_alertas_cesareas <- renderPlotly({
          df <- datos_alertas_cesareas()
          
          if (is.null(df) || nrow(df) == 0 || !"total" %in% names(df)) {
               plot_ly() %>%
                    layout(
                         annotations = list(
                              text = "No hay datos disponibles para los filtros seleccionados",
                              x = 0.5, y = 0.5,
                              xref = "paper", yref = "paper",
                              showarrow = FALSE,
                              font = list(size = 16, color = "#191970")
                         )
                    )
          } else {
               etiquetas <- format(df$total, big.mark = ".", decimal.mark = ",", scientific = FALSE)
               
               nombre_establecimiento_seleccionado <- ifelse(
                    input$establecimiento == "all", 
                    "Todos los establecimientos",
                    names(opciones_establecimientos)[opciones_establecimientos == input$establecimiento][1]
               )
               
               if (is.na(nombre_establecimiento_seleccionado) || is.null(nombre_establecimiento_seleccionado)) {
                    nombre_establecimiento_seleccionado <- input$establecimiento
               }
               
               max_valor <- max(df$total, na.rm = TRUE)
               if (max_valor == 0 || is.na(max_valor)) max_valor <- 1
               
               plot_ly(
                    df,
                    x = ~total,
                    y = ~reorder(nombre_completo, total),
                    type = "bar",
                    orientation = "h",
                    marker = list(
                         color = "#191970",
                         line = list(color = "#191970", width = 1)
                    ),
                    text = etiquetas,
                    textposition = "outside",
                    textfont = list(
                         color = "#191970",
                         size = 14,
                         family = "Arial, sans-serif"
                    ),
                    hoverinfo = "text",
                    hovertext = ~paste0(
                         "Establecimiento: ", nombre_completo, "<br>",
                         "Cesáreas: ", format(total, big.mark = ".", decimal.mark = ",", scientific = FALSE)
                    ),
                    hovertemplate = "%{hovertext}<extra></extra>"
               ) %>%
                    layout(
                         title = list(
                              text = paste0(
                                   "Cesáreas por establecimiento",
                                   "<br><sup>Filtros aplicados: ",
                                   nombre_establecimiento_seleccionado,
                                   " | ",
                                   ifelse("all" %in% input$mes || is.null(input$mes) || length(input$mes) == 0, 
                                          "Todos los meses", 
                                          paste(meses_nombres[as.numeric(input$mes)], collapse = ", ")),
                                   "</sup>"
                              ),
                              font = list(color = "#191970", size = 14)
                         ),
                         xaxis = list(
                              title = "Cantidad",
                              gridcolor = "#e0e0e0",
                              tickformat = ",.0f",
                              range = c(0, max_valor * 1.3)
                         ),
                         yaxis = list(
                              title = "",
                              tickfont = list(size = 9),
                              gridcolor = "#e0e0e0"
                         ),
                         plot_bgcolor = "white",
                         paper_bgcolor = "white",
                         hoverlabel = list(
                              bgcolor = "#191970",
                              font = list(color = "white")
                         ),
                         margin = list(b = 60, l = 150, r = 40, t = 80),
                         showlegend = FALSE
                    ) %>%
                    config(displayModeBar = TRUE, modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d"))
          }
     })
     
     # ---- TABLA DE DETALLE CESÁREAS ----
     output$tabla_detalle_cesareas <- renderReactable({
          df <- datos_detalle_cesareas()
          
          if (nrow(df) == 0) {
               return(reactable(
                    data.frame(Mensaje = "No hay datos disponibles"),
                    columns = list(Mensaje = reactable::colDef(name = ""))
               ))
          }
          
          fmt_tabla <- function(x) {
               if (is.numeric(x)) {
                    return(format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))
               }
               return(x)
          }
          
          df_formateado <- df %>%
               mutate(across(where(is.numeric), fmt_tabla))
          
          otras_columnas <- names(df_formateado)[!(names(df_formateado) %in% c("nombre_completo", "Total"))]
          
          col_defs <- list(
               nombre_completo = reactable::colDef(
                    name = "Establecimiento",
                    align = "left",
                    style = list(backgroundColor = "#191970", color = "white", fontWeight = "bold"),
                    minWidth = 100
               ),
               Total = reactable::colDef(
                    name = "Total",
                    style = list(backgroundColor = "khaki", fontWeight = "bold"),
                    align = "center"
               )
          )
          
          for (col in otras_columnas) {
               col_defs[[col]] <- reactable::colDef(
                    name = col,
                    align = "center",
                    minWidth = 50
               )
          }
          
          reactable(
               df_formateado,
               columns = col_defs,
               defaultColDef = reactable::colDef(
                    headerStyle = list(backgroundColor = "#191970", color = "white", fontWeight = "bold")
               ),
               striped = TRUE,
               bordered = TRUE,
               highlight = TRUE,
               pagination = FALSE,
               height = 400
          )
     })
     
     # ---- GRÁFICO DE ALERTAS ABORTOS ----
     output$grafico_alertas_abortos <- renderPlotly({
          df <- datos_alertas_abortos()
          
          if (is.null(df) || nrow(df) == 0 || !"total_abortos" %in% names(df)) {
               plot_ly() %>%
                    layout(
                         annotations = list(
                              text = "No hay datos disponibles para los filtros seleccionados",
                              x = 0.5, y = 0.5,
                              xref = "paper", yref = "paper",
                              showarrow = FALSE,
                              font = list(size = 16, color = "#191970")
                         )
                    )
          } else {
               etiquetas <- format(df$total_abortos, big.mark = ".", decimal.mark = ",", scientific = FALSE)
               
               nombre_establecimiento_seleccionado <- ifelse(
                    input$establecimiento == "all", 
                    "Todos los establecimientos",
                    names(opciones_establecimientos)[opciones_establecimientos == input$establecimiento][1]
               )
               
               if (is.na(nombre_establecimiento_seleccionado) || is.null(nombre_establecimiento_seleccionado)) {
                    nombre_establecimiento_seleccionado <- input$establecimiento
               }
               
               max_valor <- max(df$total_abortos, na.rm = TRUE)
               if (max_valor == 0 || is.na(max_valor)) max_valor <- 1
               
               plot_ly(
                    df,
                    x = ~total_abortos,
                    y = ~reorder(nombre_completo, total_abortos),
                    type = "bar",
                    orientation = "h",
                    marker = list(
                         color = "#191970",
                         line = list(color = "#191970", width = 1)
                    ),
                    text = etiquetas,
                    textposition = "outside",
                    textfont = list(
                         color = "#191970",
                         size = 14,
                         family = "Arial, sans-serif"
                    ),
                    hoverinfo = "text",
                    hovertext = ~paste0(
                         "Establecimiento: ", nombre_completo, "<br>",
                         "Cantidad: ", format(total_abortos, big.mark = ".", decimal.mark = ",", scientific = FALSE)
                    ),
                    hovertemplate = "%{hovertext}<extra></extra>"
               ) %>%
                    layout(
                         title = list(
                              text = paste0(
                                   "Abortos y otros procedimientos obstétricos por establecimiento",
                                   "<br><sup>Filtros aplicados: ",
                                   nombre_establecimiento_seleccionado,
                                   " | ",
                                   ifelse("all" %in% input$mes || is.null(input$mes) || length(input$mes) == 0, 
                                          "Todos los meses", 
                                          paste(meses_nombres[as.numeric(input$mes)], collapse = ", ")),
                                   "</sup>"
                              ),
                              font = list(color = "#191970", size = 14)
                         ),
                         xaxis = list(
                              title = "Cantidad",
                              gridcolor = "#e0e0e0",
                              tickformat = ",.0f",
                              range = c(0, max_valor * 1.3)
                         ),
                         yaxis = list(
                              title = "",
                              tickfont = list(size = 9),
                              gridcolor = "#e0e0e0"
                         ),
                         plot_bgcolor = "white",
                         paper_bgcolor = "white",
                         hoverlabel = list(
                              bgcolor = "#191970",
                              font = list(color = "white")
                         ),
                         margin = list(b = 60, l = 150, r = 40, t = 80),
                         showlegend = FALSE
                    ) %>%
                    config(displayModeBar = TRUE, modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "autoScale2d"))
          }
     })
     
     # ---- TABLA DE DETALLE ABORTOS ----
     output$tabla_detalle_abortos <- renderReactable({
          df <- datos_detalle_abortos()
          
          if (nrow(df) == 0) {
               return(reactable(
                    data.frame(Mensaje = "No hay datos disponibles"),
                    columns = list(Mensaje = reactable::colDef(name = ""))
               ))
          }
          
          fmt_tabla <- function(x) {
               if (is.numeric(x)) {
                    return(format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))
               }
               return(x)
          }
          
          df_formateado <- df %>%
               mutate(across(where(is.numeric), fmt_tabla))
          
          otras_columnas <- names(df_formateado)[!(names(df_formateado) %in% c("nombre_completo", "Total"))]
          
          col_defs <- list(
               nombre_completo = reactable::colDef(
                    name = "Establecimiento",
                    align = "left",
                    style = list(backgroundColor = "#191970", color = "white", fontWeight = "bold"),
                    minWidth = 100
               ),
               Total = reactable::colDef(
                    name = "Total",
                    style = list(backgroundColor = "khaki", fontWeight = "bold"),
                    align = "center"
               )
          )
          
          for (col in otras_columnas) {
               col_defs[[col]] <- reactable::colDef(
                    name = col,
                    align = "center",
                    minWidth = 50
               )
          }
          
          reactable(
               df_formateado,
               columns = col_defs,
               defaultColDef = reactable::colDef(
                    headerStyle = list(backgroundColor = "#191970", color = "white", fontWeight = "bold")
               ),
               striped = TRUE,
               bordered = TRUE,
               highlight = TRUE,
               pagination = FALSE,
               height = 450
          )
     })
     
     # ---- EXPORTACIÓN DE DATOS ----
     datos_export_f1 <- reactive({
          df <- datos_aggregados_f1()
          req(df)
          
          df_export <- df
          
          df_export$total_partos <- sapply(df_export$total_partos, function(x) if(is.na(x)) "0" else fmt_export(x))
          df_export$pueblos_originarios <- sapply(df_export$pueblos_originarios, function(x) if(is.na(x)) "0" else fmt_export(x))
          df_export$inmigrantes <- sapply(df_export$inmigrantes, function(x) if(is.na(x)) "0" else fmt_export(x))
          
          df_export$prematuro_00a23 <- mapply(function(val, cat) {
               if(cat == "Total" || !(cat %in% categorias_con_prematuros)) return("-")
               return(fmt_export(val))
          }, df_export$prematuro_00a23, df_export$tabla)
          
          df_export$prematuro_24a28 <- mapply(function(val, cat) {
               if(cat == "Total" || !(cat %in% categorias_con_prematuros)) return("-")
               return(fmt_export(val))
          }, df_export$prematuro_24a28, df_export$tabla)
          
          df_export$prematuro_29a32 <- mapply(function(val, cat) {
               if(cat == "Total" || !(cat %in% categorias_con_prematuros)) return("-")
               return(fmt_export(val))
          }, df_export$prematuro_29a32, df_export$tabla)
          
          df_export$prematuro_33a36 <- mapply(function(val, cat) {
               if(cat == "Total" || !(cat %in% categorias_con_prematuros)) return("-")
               return(fmt_export(val))
          }, df_export$prematuro_33a36, df_export$tabla)
          
          df_export <- df_export %>%
               rename(
                    `Partos` = tabla,
                    `Total` = total_partos,
                    `Prematuro <24 sem` = prematuro_00a23,
                    `Prematuro 24-28 sem` = prematuro_24a28,
                    `Prematuro 29-32 sem` = prematuro_29a32,
                    `Prematuro 33-36 sem` = prematuro_33a36,
                    `Pueblos Originarios` = pueblos_originarios,
                    `Inmigrantes` = inmigrantes
               )
          
          df_export
     })
     
     datos_export_f2 <- reactive({
          df <- datos_aggregados_f2()
          req(df)
          
          df_export <- df
          
          for(col in names(df_export)[names(df_export) != "tabla"]) {
               df_export[[col]] <- sapply(df_export[[col]], function(x) fmt_export(x))
          }
          
          df_export <- df_export %>%
               rename(
                    `CAUSALES` = tabla,
                    `Total` = total,
                    `Edad en años menor a 14` = menor_14,
                    `Edad en años 14-19` = de_14a19,
                    `Edad en años 20-24` = de_20a24,
                    `Edad en años 25-29` = de_25a29,
                    `Edad en años 30-34` = de_30a34,
                    `Edad en años 35-39` = de_35a39,
                    `Edad en años 40-44` = de_40a44,
                    `Edad en años 45-49` = de_45a49,
                    `Edad en años 50-54` = de_50a54,
                    `Edad en años 55 y más` = mas_55,
                    `Aborto hasta 12sem 0dias` = aborto_hasta_12sem,
                    `Aborto de 12sem 1dia hasta 14sem 0dias` = aborto_de_12a14sem,
                    `Aborto de 14sem 1dia hasta 21sem 6dias` = aborto_de_14a21sem6d,
                    `Partos vaginales inducidos` = partos_vag_inducidos,
                    `Cesáreas` = cesareas,
                    `Beneficiarias FONASA` = fonasa,
                    `Pueblos Originarios` = pueblos_originarios,
                    `Migrantes` = migrantes
               )
          
          idx_violacion <- which(df_export$CAUSALES == "Causal N° 3: Por violación")
          if (length(idx_violacion) > 0) {
               cols_no_aplican <- c("Aborto de 14sem 1dia hasta 21sem 6dias", 
                                    "Partos vaginales inducidos", 
                                    "Cesáreas")
               for (col in cols_no_aplican) {
                    df_export[idx_violacion, col] <- "-"
               }
          }
          
          df_export
     })
     
     # ---- EXPORTACIÓN DE DATOS PARA DETALLE PARTOS ----
     datos_export_detalle_partos <- reactive({
          df <- datos_detalle_partos()
          req(df)
          
          if (nrow(df) == 0) {
               return(data.frame(
                    Establecimiento = character(0),
                    Total = character(0)
               ))
          }
          
          df_export <- df
          
          for(col in names(df_export)) {
               if (is.numeric(df_export[[col]])) {
                    df_export[[col]] <- sapply(df_export[[col]], function(x) {
                         if (is.na(x) || is.null(x)) return("0")
                         return(format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))
                    })
               }
          }
          
          names(df_export)[names(df_export) == "nombre_completo"] <- "Establecimiento"
          
          df_export
     })
     
     # ---- EXPORTACIÓN DE DATOS PARA DETALLE CESÁREAS ----
     datos_export_detalle_cesareas <- reactive({
          df <- datos_detalle_cesareas()
          req(df)
          
          if (nrow(df) == 0) {
               return(data.frame(
                    Establecimiento = character(0),
                    Total = character(0)
               ))
          }
          
          df_export <- df
          
          for(col in names(df_export)) {
               if (is.numeric(df_export[[col]])) {
                    df_export[[col]] <- sapply(df_export[[col]], function(x) {
                         if (is.na(x) || is.null(x)) return("0")
                         return(format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))
                    })
               }
          }
          
          names(df_export)[names(df_export) == "nombre_completo"] <- "Establecimiento"
          
          df_export
     })
     
     # ---- EXPORTACIÓN DE DATOS PARA ABORTOS ----
     datos_export_abortos <- reactive({
          df <- datos_detalle_abortos()
          req(df)
          
          if (nrow(df) == 0) {
               return(data.frame(
                    Establecimiento = character(0),
                    Total = character(0)
               ))
          }
          
          df_export <- df
          
          for(col in names(df_export)) {
               if (is.numeric(df_export[[col]])) {
                    df_export[[col]] <- sapply(df_export[[col]], function(x) {
                         if (is.na(x) || is.null(x)) return("0")
                         return(format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))
                    })
               }
          }
          
          names(df_export)[names(df_export) == "nombre_completo"] <- "Establecimiento"
          
          df_export
     })
     
     datos_export_g1 <- reactive({
          df <- datos_aggregados_g1()
          req(df)
          
          df_export <- df
          
          for(col in names(df_export)[names(df_export) != "tabla"]) {
               df_export[[col]] <- sapply(df_export[[col]], function(x) fmt_export(x))
          }
          
          df_export <- df_export %>%
               rename(
                    `TIPO` = tabla,
                    `Total` = total,
                    `Peso al nacer menor a 500 gr` = menos_500,
                    `Peso al nacer de 500 a 999 gr` = de_500_a_999,
                    `Peso al nacer 1000 a 1499 gr` = de_1000_a_1499,
                    `Peso al nacer 1500 a 1999 gr` = de_1500_a_1999,
                    `Peso al nacer 2000 a 2499 gr` = de_2000_a_2499,
                    `Peso al nacer 2500 a 2999 gr` = de_2500_a_2999,
                    `Peso al nacer 3000 a 3499 gr` = de_3000_a_3499,
                    `Peso al nacer 3500 a 3999 gr` = de_3500_a_3999,
                    `Peso al nacer 4000 gr y más` = de_4000_y_mas,
                    `*PKU e HC - Primeras Muestras` = primeras_muestras,
                    `*PKU e HC - Muestras Repetidas` = muestras_repetidas
               )
          
          idx_fallecidos <- which(df_export$TIPO == "Nacidos fallecidos")
          if (length(idx_fallecidos) > 0) {
               cols_pku <- c("*PKU e HC - Primeras Muestras", "*PKU e HC - Muestras Repetidas")
               for (col in cols_pku) {
                    df_export[idx_fallecidos, col] <- "-"
               }
          }
          
          df_export
     })
     
     datos_export_g2 <- reactive({
          df <- datos_aggregados_g2()
          req(df)
          
          df_export <- df
          
          for(col in names(df_export)[names(df_export) != "tabla"]) {
               df_export[[col]] <- sapply(df_export[[col]], function(x) fmt_export(x))
          }
          
          df_export <- df_export %>%
               rename(
                    `TIPO` = tabla,
                    `Total` = total
               )
          
          df_export
     })
     
     datos_export_g3 <- reactive({
          df <- datos_aggregados_g3()
          req(df)
          
          df_export <- df
          
          for(col in names(df_export)[names(df_export) != "tabla"]) {
               df_export[[col]] <- sapply(df_export[[col]], function(x) fmt_export(x))
          }
          
          df_export <- df_export %>%
               rename(
                    `TIPO` = tabla,
                    `APGAR ≤ 3 (al minuto)` = apgar_3,
                    `APGAR ≤ 6 (a los 5 minutos)` = apgar_6
               )
          
          df_export
     })
     
     output$download_data <- downloadHandler(
          filename = function() { 
               paste0(format(Sys.Date(), "%y%m%d"), "_remasep_2025.xlsx") 
          },
          content = function(file) {
               
               f1_data <- datos_export_f1()
               detalle_partos <- datos_export_detalle_partos()
               detalle_cesareas <- datos_export_detalle_cesareas()
               abortos_data <- datos_export_abortos()
               f2_data <- datos_export_f2()
               g1_data <- datos_export_g1()
               g2_data <- datos_export_g2()
               g3_data <- datos_export_g3()
               apgar_detalle <- datos_export_apgar_detalle()
               ive_data <- datos_export_ive()
               porcentajes_data <- datos_export_porcentajes()
               
               validacion_detalle <- datos_validacion_consistencia()
               validacion_resumen <- datos_resumen_errores()
               
               wb <- openxlsx::createWorkbook()
               
               openxlsx::addWorksheet(wb, "Resumen Porcentajes")
               openxlsx::addWorksheet(wb, "F.1 Partos y Abortos")
               openxlsx::addWorksheet(wb, "Detalle Partos")
               openxlsx::addWorksheet(wb, "Detalle Cesareas")
               openxlsx::addWorksheet(wb, "Abortos y otros proc.")
               openxlsx::addWorksheet(wb, "F.2 IVE")
               openxlsx::addWorksheet(wb, "Detalle IVE x Estab")
               openxlsx::addWorksheet(wb, "G.1 Peso al Nacer")
               openxlsx::addWorksheet(wb, "G.2 Malformación")
               openxlsx::addWorksheet(wb, "G.3 Apgar")
               openxlsx::addWorksheet(wb, "Apgar Detalle x Estab")
               openxlsx::addWorksheet(wb, "Validacion Detalle")
               openxlsx::addWorksheet(wb, "Validacion Resumen")
               
               openxlsx::writeData(wb, "Resumen Porcentajes", porcentajes_data)
               openxlsx::writeData(wb, "F.1 Partos y Abortos", f1_data)
               openxlsx::writeData(wb, "Detalle Partos", detalle_partos)
               openxlsx::writeData(wb, "Detalle Cesareas", detalle_cesareas)
               openxlsx::writeData(wb, "Abortos y otros proc.", abortos_data)
               openxlsx::writeData(wb, "F.2 IVE", f2_data)
               openxlsx::writeData(wb, "Detalle IVE x Estab", ive_data)
               openxlsx::writeData(wb, "G.1 Peso al Nacer", g1_data)
               openxlsx::writeData(wb, "G.2 Malformación", g2_data)
               openxlsx::writeData(wb, "G.3 Apgar", g3_data)
               openxlsx::writeData(wb, "Apgar Detalle x Estab", apgar_detalle)
               
               if (nrow(validacion_detalle) == 0) {
                    validacion_detalle <- data.frame(
                         Mensaje = "No se encontraron inconsistencias en los datos"
                    )
               }
               
               if (nrow(validacion_resumen) == 0) {
                    validacion_resumen <- data.frame(
                         Mensaje = "Todos los establecimientos tienen datos consistentes"
                    )
               }
               
               openxlsx::writeData(wb, "Validacion Detalle", validacion_detalle)
               openxlsx::writeData(wb, "Validacion Resumen", validacion_resumen)
               
               for (sheet in names(wb)) {
                    openxlsx::setColWidths(wb, sheet, cols = 1, widths = 40)
                    openxlsx::setColWidths(wb, sheet, cols = 2, widths = 25)
                    if (sheet != "Resumen Porcentajes" && sheet != "Validacion Detalle" && sheet != "Validacion Resumen") {
                         openxlsx::setColWidths(wb, sheet, cols = 3:20, widths = 15)
                    }
               }
               
               openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
          }
     )
     
}

shinyApp(ui, server)
