#rTrophicPosition
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


# 1)	Recolección de los datos otorgados por el usuario o la misma aplicación.
# 2)	Limpieza de los datos otorgados por el usuario o la misma aplicación.
# 3)	Exploración de los datos otorgados por el usuario o la misma aplicación.
# 4)	Generación de graficas según las disponibles al momento por la aplicación.
# 5)	Generar resúmenes visuales de los datos otorgados por el usuario o la misma aplicación.
# 6)	Modificación de parámetros de los modelos.
# 7)	Selección del tipo de modelos.
# 8)	Demostración funcional de la aplicación.



# library(tRophicPosition)
library(shiny)
# library(shinythemes)
library(data.table)
library(ggplot2)
library(bslib)

ui <- fillPage(
  theme = bs_theme(version = 5),
  
  tags$head(
    tags$link(rel = "stylesheet",
              type = "text/css",
              href = "style.css")
  ),
  
  navbarPage(id='cabezera',
             # position = c("static-top", "fixed-top", "fixed-bottom"),
             h1("tRopicPosition Visual Tool")
             ),
  
  
  # titlePanel("Ingrese los datos para su analizis"),
  fluidPage(
    sidebarLayout(
      sidebarPanel(id='formulario',
                   titlePanel('Ingrese los datos para su analizis'),
                   
                   # fileInput("file",
                   #           label = "Ingrese archivos por analizar",
                   #           placeholder = "Ejemplo.csv",
                   #           buttonLabel = "Buscar",
                   #           multiple = TRUE,),
                   
                   fileInput("file1", "Choose CSV File", accept = c(".csv")),
                   checkboxInput("header", "Header", TRUE)
      ),
      
      mainPanel(tabsetPanel(
        tabPanel(
          'Resumen de datos',
          # fluidRow(column(width = 4,
          #                 "4"),
          #          column(
          #            width = 3, offset = 2,
          #            "3 offset 2"
          #          )),
          # "Table",
          # icon = icon("fa-duotone fa-envelope"),
          # fluidRow(column(12,
          #                 "Fluid 12",
          #                 fluidRow(
          #                   column(6,
          #                          "Fluid 6",
          #                          fluidRow(
          #                            column(6,
          #                                   "Fluid 6",
          #                                   submitButton("Update View", icon = icon("bi bi-envelope"))),
          #                            column(
          #                              6,
          #                              "Fluid 6",
          #                              navbarPage(
          #                                "App Title",
          #                                tabPanel("Plot", icon = icon("bar-chart-o")),
          #                                tabPanel("Summary", icon = icon("list-alt")),
          #                                tabPanel("Table", icon = icon("fa-duotone fa-envelope"))
          #                              )
          #                            )
          #                          )),
          #                   column(width = 6,
          #                          "Fluid 6")
          #                 )))
        ),
        
        tabPanel('Tabla de datos', fluid = TRUE,
                 tableOutput('table')),
        
        tabPanel('Graficos de datos', fluid = TRUE, )
      ))
    )
  ),
  #   tags$footer(HTML("
  #                     <!-- Footer -->
  #                            <footer class='page-footer font-large indigo'>
  #                            <!-- Copyright -->
  #                            <div class='footer-copyright text-center py-3'>© 2022 Copyright tRopicPosition
  #                            </div>
  #                            <!-- Copyright -->
  # <i class='fa-duotone fa-envelope'></i>
  #                            </footer>
  #                            <!-- Footer -->")),
  #   "Table", icon = icon("fa-duotone fa-envelope"),

  tags$footer(align = "center",
    fluidRow(
      column(4,'Logo'),
      column(4,'© 2022 Copyright tRopicPosition'),
      column(4,'Mail')
    ),
  )
  
)



server <- function(input, output) {
  output$table <- renderTable({
    input$file1
  })
  
}

shinyApp(ui = ui, server = server)


#   ## input$file is a data frame and contains the details around the name, size and temp location of the files uploaded
#   # this reactive output display the content of the input$file dataframe
#   output$filedf <- renderTable({
#     if(is.null(input$file)){return ()}
#     input$file # the file input data frame object that contains the file attributes
#   })
#
#   # Extract the file path for file
#   output$filedf2 <- renderTable({
#     if(is.null(input$file)){return ()}
#     input$file$datapath # the file input data frame object that contains the file attributes
#   })
#
#   ## Below code to display the structure of the input file object
#   output$fileob <- renderPrint({
#     if(is.null(input$file)){return ()}
#     str(input$file)
#   })
#
#   ## Side bar select input widget coming through renderUI()
#   # Following code displays the select input widget with the list of file loaded by the user
#   output$selectfile <- renderUI({
#     if(is.null(input$file)) {return()}
#     list(hr(),
#          helpText("Seleccione el documento con el cual se trabajara"),
#          selectInput("Select", "Archivos disponibles: ",
#                      choices=input$file$name)
#     )
#
#   })
#
#   ## Summary Stats code ##
#   # this reactive output contains the summary of the dataset and display the summary in table format
#   output$summ <- renderPrint({
#     if(is.null(input$file)){return()}
#     summary(read.table(file=input$file$datapath[input$file$name==input$Select],
#                        sep=input$sep,
#                        header = input$header,
#                        stringsAsFactors = input$stringAsFactors))})
#
#   ## Dataset code ##
#   # This reactive output contains the dataset and display the dataset in table format
#   output$table <- renderTable({
#     if(is.null(input$file)){return()}
#     read.table(file=input$file$datapath[input$file$name==input$Select], sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
#
#   })
#
#   ## MainPanel tabset renderUI code ##
#   # the following renderUI is used to dynamically generate the tabsets when the file is loaded.
#   # Until the file is loaded, app will not show the tabset.
#   output$tb <- renderUI({
#     if(is.null(input$file)) {return()}
#     else
#       tabsetPanel(
#         tabPanel("Archivo de entrada ", tableOutput("filedf"), tableOutput("filedf2")),
#         tabPanel("Estructura del objeto de entrada", verbatimTextOutput("fileob")),
#         tabPanel("Tabla de contenido", tableOutput("table")),
#         tabPanel("Resumen de Informacion", verbatimTextOutput("summ")))
#   })
