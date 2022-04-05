
library(tRophicPosition)

library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)



ui <- fluidPage(
  theme = shinytheme("cosmo"),
  
  
  
  navbarPage(
    title = "Demo Anteproyecto",
    fluid = TRUE,
    tabPanel("Plot",
             tabsetPanel(
               tabPanel(
                 "Set de Datos",
                 fluid = TRUE,
                 sidebarLayout(
                   sidebarPanel(
                     fileInput(
                       "file",
                       label = "Ingrese archivos por analizar",
                       placeholder = "Ejemplo.csv",
                       buttonLabel = "Buscar",
                       multiple = TRUE,
                     ),
                     helpText("Solo con extenciones .csv"),
                     
                     checkboxInput(
                       inputId = 'header',
                       label = 'Header',
                       value = TRUE
                     ),
                     checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
                     
                     radioButtons(
                       inputId = 'sep',
                       label = 'Separator',
                       choices = c(
                         Coma = ',',
                         PuntoyComa = ';',
                         Tabulacion = '\t',
                         Espacio = ' '
                       ),
                       selected = ','
                     ),
                     
                     uiOutput("selectfile")
                   ),
                   mainPanel(uiOutput("tb"))
                   
                 )
                 
                 
               ),
               tabPanel("Tablas", fluid = TRUE),
               tabPanel("Graficos", fluid = TRUE)
               
             )),
    navbarMenu(
      "More",
      tabPanel("Summary"),
      "----",
      "Section header",
      tabPanel("Table")
    )
  ),
  
  
  
  
  
)




server <- function(input, output) {
  output$filedf <- renderTable({
    if (is.null(input$file)) {
      return ()
    }
    input$file
  })
  
  
  output$filedf2 <- renderTable({
    if (is.null(input$file)) {
      return ()
    }
    input$file$datapath
  })
  
  
  output$fileob <- renderPrint({
    if (is.null(input$file)) {
      return ()
    }
    str(input$file)
  })
  
  
  output$selectfile <- renderUI({
    if (is.null(input$file)) {
      return()
    }
    list(
      hr(),
      helpText("Seleccione el documento con el cual se trabajara"),
      selectInput("Select", "Archivos disponibles: ",
                  choices = input$file$name)
    )
    
  })
  
  
  output$summ <- renderPrint({
    if (is.null(input$file)) {
      return()
    }
    summary(
      read.table(
        file = input$file$datapath[input$file$name == input$Select],
        sep = input$sep,
        header = input$header,
        stringsAsFactors = input$stringAsFactors
      )
    )
  })
  
  
  output$table <- renderTable({
    if (is.null(input$file)) {
      return()
    }
    read.table(
      file = input$file$datapath[input$file$name == input$Select],
      sep = input$sep,
      header = input$header,
      stringsAsFactors = input$stringAsFactors
    )
    
  })
  
  
  output$tb <- renderUI({
    if (is.null(input$file)) {
      return()
    }
    else
      tabsetPanel(
        tabPanel(
          "Archivo de entrada ",
          tableOutput("filedf"),
          tableOutput("filedf2")
        ),
        tabPanel(
          "Estructura del objeto de entrada",
          verbatimTextOutput("fileob")
        ),
        tabPanel("Tabla de contenido", tableOutput("table")),
        tabPanel("Resumen de Informacion", verbatimTextOutput("summ"))
      )
  })
}

shinyApp(ui, server)
