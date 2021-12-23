#Funcionalidad 1: Ingreso de información
#Funcionalidad 2: chequeo y graficado de información
##Funcionalidad 3: Establecer factor de discriminacion trofica (deltaN y/o deltaC)
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

library(tRophicPosition)

library(shiny)
library(shinythemes)


ui <- fluidPage(theme = shinytheme("cosmo"),

  tags$h1("Demo Anteproyecto"),

  titlePanel("Sistema para el uso de la libreria tRophicPosition"),
  sidebarLayout(
    sidebarPanel(

      fileInput("file",
                label = "Ingrese archivos por analizar",
                placeholder = "Ejemplo.csv",
                buttonLabel = "Buscar",
                multiple = TRUE,),
      helpText("Solo con extenciones .csv"),

      checkboxInput(inputId = 'header',
                    label = 'Header',
                    value = TRUE),
      checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),

      radioButtons(inputId = 'sep',
                   label = 'Separator',
                   choices = c(Coma=',',PuntoyComa=';',Tabulacion='\t', Espacio=' '),
                   selected = ','),

      uiOutput("selectfile")
    ),
    mainPanel(
      uiOutput("tb")

    )

  )
)


server <- function(input,output) {



  output$filedf <- renderTable({
    if(is.null(input$file)){return ()}
    input$file
  })

  output$filedf2 <- renderTable({
    if(is.null(input$file)){return ()}
    input$file$datapath
  })

  output$fileob <- renderPrint({
    if(is.null(input$file)){return ()}
    str(input$file)
  })

  output$selectfile <- renderUI({
    if(is.null(input$file)) {return()}
    list(hr(),
         helpText("Seleccione el documento con el cual se trabajara"),
         selectInput("Select", "Archivos disponibles: ",
                     choices=input$file$name)
    )

  })

  output$summ <- renderPrint({
    if(is.null(input$file)){return()}
    summary(read.table(file=input$file$datapath[input$file$name==input$Select],
                       sep=input$sep,
                       header = input$header,
                       stringsAsFactors = input$stringAsFactors))})


  output$table <- renderTable({
    if(is.null(input$file)){return()}
    read.table(file=input$file$datapath[input$file$name==input$Select], sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)

  })

  output$tb <- renderUI({
    if(is.null(input$file)) {return()}
    else
      tabsetPanel(
        tabPanel("Archivo de entrada ", tableOutput("filedf"), tableOutput("filedf2")),
        tabPanel("Estructura del objeto de entrada", verbatimTextOutput("fileob")),
        tabPanel("Tabla de contenido", tableOutput("table")),
        tabPanel("Resumen de Informacion", verbatimTextOutput("summ")))
  })
}

shinyApp(ui, server)
