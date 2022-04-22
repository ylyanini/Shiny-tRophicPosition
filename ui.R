#Funcionalidad 1: Ingreso de información
#Funcionalidad 2: chequeo y graficado de información
#Funcionalidad 3: Establecer factor de discriminacion trofica (deltaN y/o deltaC)
#Funcionalidad 4: Establecer modelo Bayesiano
#Funcionalidad 5: Crear modelo JAGS/BUGS
#Funcionalidad 6: Generar muestras a posteriori
#Funcionalidad 7: Combinar, resumir gy graficar muestras a posteriori


library(shiny)
library(DT)
library(tRophicPosition)
library(ggplot2)
library(shinyWidgets)
library(summarytools)
library(bslib)



shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet",
              type = "text/css",
              href = "style.css"),
  ),
  tags$head(
    HTML('<link rel="icon", href="img/logo.png",
                                   type="image/png" />')
  ),
  
  navbarPage(
    title = div(
      img(
        src = 'img/logo.png',
        style = "margin-top: -14px;
                               padding-right:9px;
                               padding-bottom:9px",
        height = 100
      )
    ),
    windowTitle = "Shiny-tRophicPosition",
    
    ## ------------------------------------------------------------
    # Carga de archivo
    tabPanel("Dataset",
             sidebarLayout(
               sidebarPanel(
                 id = 'formulario',
                 tags$h2("Upload File", icon("fas fa-folder")),
                 tags$hr(),
                 
                 radioButtons(
                   "sep",
                   tags$h3("Separator"),
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
                   tags$h3("Comma"),
                   choices = c(
                     None = "",
                     "Double Quote" = '"',
                     "Single Quote" = "'"
                   ),
                   selected = '"',
                   inline = TRUE
                 ),
                 
                 switchInput(
                   inputId = "header",
                   label = tags$b("Have a Header?"),
                   labelWidth = "100px",
                   onLabel = "YES",
                   offLabel = "NO",
                   TRUE
                 ),
                 
                 # checkboxInput("header",
                 #               tags$b("Have a Header?"), 
                 #               TRUE),
                 # 
                 
                 fileInput(
                   'file',
                   tags$h3('Select File'),
                   accept = c('text/csv',
                              'text/comma-separated-values',
                              '.csv'),
                   buttonLabel = tags$b("UPLOAD"),
                   placeholder = "Example.csv",
                   multiple = FALSE,
                 ),
               ),
               mainPanel(uiOutput("content_data"))
             ), ),
    ## ------------------------------------------------------------
    tabPanel(
      "IsotopeData Object",
      sidebarLayout(
        sidebarPanel(
          id = 'formulario',
          tags$h2('Load Isotope Data', icon("fas fa-filter")),
          tags$hr(),
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
          
          # submitButton("Enviar"),
          
        ),
        mainPanel(uiOutput("content_TP"))
      )
      
    ),
    
    ## ------------------------------------------------------------
    tabPanel('Bayesian Model',
             sidebarLayout(
               sidebarPanel(
                 id = 'formulario',
                 tags$h2('Create Model', icon("fas fa-cubes")),
                 numericInput('n.chains',
                              h4('n.chains'),
                              value = 4),
                 numericInput('n.adapt',
                              h4('n.adapt'),
                              value = 5000),
                 
                 
                 # submitButton("Enviar"),
                 
               ),
               mainPanel(uiOutput("content_model"))
             )),
    
    ## ------------------------------------------------------------
    tabPanel('Posterior Sample',
             sidebarLayout(
               sidebarPanel(
                 id = 'formulario',
                 tags$h2('Make Post Samples', icon("fas fa-chart-bar")),
                 tags$hr(),
                 numericInput('n.iter',
                              h4('n.iter'),
                              value = 15000),
                 
                 # submitButton("Enviar"),
                 
               ),
               mainPanel(uiOutput("content_posterior"))
             ))
  )
))
