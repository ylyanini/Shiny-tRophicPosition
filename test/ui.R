#Funcionalidad 1: Ingreso de información
#Funcionalidad 2: chequeo y graficado de información
#Funcionalidad 3: Establecer factor de discriminacion trofica (deltaN y/o deltaC)
#Funcionalidad 4: Establecer modelo Bayesiano
#Funcionalidad 5: Crear modelo JAGS/BUGS
#Funcionalidad 6: Generar muestras a posteriori
#Funcionalidad 7: Combinar, resumir gy graficar muestras a posteriori

#objetivo de la implementacion
#greta como formato sobre bugs
#metricas
#tanto comparacion de rendimiento como nuevas funcionalidades como publicacion

#isotop data a distintos formatos
#exportacion de luis para la carcasa
#dejar claro que funcionalidades hare yo, y que se elevara con shiny
#ejemplificaciones visuales en el informe



library(shiny)
library(DT)
library(tRophicPosition)
library(data.table)
library(ggplot2)
library(bslib)


shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet",
              type = "text/css",
              href = "style.css"),
  ),
  
  # tags$nav(
  #   id = 'encabezado',
  #   class = "navbar navbar-light bg-light",
  #   tags$div(class = "container-fluid",
  #            tags$h1(
  #              "tRophicPosition Aplication"))
  # ),
  
  
  
  
  sidebarLayout(
    sidebarPanel(
      id = 'formulario',
      tags$h2("Archivo csv"),
      
      fileInput(
        'file',
        tags$h3('Seleccione el archivo'),
        accept = c('text/csv',
                   'text/comma-separated-values',
                   '.csv'),
        buttonLabel = tags$b("CARGAR"),
        placeholder = "Ejemplo.csv"
      ),
      
      checkboxInput("header", "Encabezado", TRUE),
      
      radioButtons(
        "sep",
        tags$h3("Separador"),
        choices = c(
          Comma = ",",
          Semicolon = ";",
          Tab = "\t"
        ),
        selected = ",",
        inline = TRUE
      ),
      
      
      radioButtons(
        "quote",
        tags$h3("Comillas"),
        choices = c(
          None = "",
          "Double Quote" = '"',
          "Single Quote" = "'"
        ),
        selected = '"',
        inline = TRUE
      ),

      radioButtons(
        "disp",
        tags$h3("Mostrar"),
        choices = c(Encabezado = "head",
                    Todo = "all"),
        selected = "all",
        inline = TRUE
      ),
      submitButton("Enviar"),
      tags$hr(),
      tags$h2('tRophicPosition'),
      
      
      
      tags$h3('Model', icon("fas fa-chart-bar")),
      numericInput('n.chains',
                   h4('n.chains'),
                   value = 4),
      numericInput('n.adapt',
                   h4('n.adapt'),
                   value = 5000),
      
      
      
      tags$h3('posterior.samples'),
      numericInput('n.iter',
                   h4('n.iter'),
                   value = 15000),
      
      
      
      tags$h3('loadIsotopeData'),
      textInput('consumer',
                h4('consumer'),
                value = 'Bilagay'),
      
      textInput('consumersColumn',
                h4('consumersColumn'),
                value = 'FG'),
      
      textInput('b1',
                h4('b1'),
                value = 'Pelagic_BL'),
      
      textInput('b2',
                h4('b2'),
                value = 'Benthic_BL'),
      
      textInput('baselineColumn',
                h4('baselineColumn'),
                value = 'FG'),
      
      textInput('group',
                h4('group'),
                value = 'Coquimbo'),
      
      textInput('groupsColumn',
                h4('groupsColumn'),
                value = 'Location'),
   
      submitButton("Enviar"),
      
      
    ),
    mainPanel(uiOutput("content"))
  ),
  
  # tags$footer(fluidRow(
  #   column(5, 'Logo'),
  #   column(2, '© 2022 Copyright tRopicPosition'),
  #   column(5, 'Mail')
  # ))
  
))
