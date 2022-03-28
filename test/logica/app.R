
library(shiny)
library(DT)
library(tRophicPosition)


ui <- shinyUI(fluidPage(
  
  tags$h1("Logica"),
  sidebarLayout(
    sidebarPanel(
      tags$h2("Upload file"),
      fileInput('file', 'Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  '.csv'
                )),
      checkboxInput("header", "Header", TRUE),
      radioButtons("sep",
                   "Separator: ",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected=",",
                   inline=TRUE),
      
      
            radioButtons(
              "quote",
              "Quote",
              choices = c(
                None = "",
                "Double Quote" = '"',
                "Single Quote" = "'"
              ),
              selected = '"'
            ),
            # Input: Select number of rows to display ----
            radioButtons(
              "disp",
              "Display",
              choices = c(Head = "head",
                          All = "all"),
              selected = "head"
            ),
            tags$hr(),
            tags$h2('tRophicPosition'),
            
      
    ),
    mainPanel(
      fluidRow(
        column(width = 12,
               fluidRow(tags$h2("Table"),
                        DT::dataTableOutput("sample_table"))),
        column(width = 12,
               fluidPage(tags$h2("Data Summary"),
                         verbatimTextOutput('summary'))),
        column(width = 12,
               fluidPage(tags$h2("Plot"), 
                         plotOutput('plot'))),
        
        column(width = 12,
               fluidPage(tags$h2("Posterior Summary"), 
                         verbatimTextOutput('post_summary'))),
               
              
        
      ),
      
    )
  ),
  
 
  
)
)


server <- shinyServer(function(input, output) {
  library(tRophicPosition)
  
  
  df_upload <- reactive({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, 
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    

    return(df)
  })
  
  output$sample_table<- DT::renderDataTable({
    df <- df_upload()
    DT::datatable(df)
  })
  
  output$summary <-renderPrint({
    summary(df_upload())
  })
  
  output$plot <- renderPlot({
    df <- df_upload()
    consumer <-
      tRophicPosition::loadIsotopeData(
        df,
        consumer = "Bilagay",
        consumersColumn = "FG",
        b1 = "Pelagic_BL",
        b2 = "Benthic_BL",
        baselineColumn = "FG",
        group = "Coquimbo",
        groupsColumn = "Location"
      )
    plot(consumer, b1 = "Pelagic baseline", b2 = "Benthic baseline")
  })
  
  output$post_summary <-renderPrint({
    
    df <- df_upload()
    consumer <-
      tRophicPosition::loadIsotopeData(
        df,
        consumer = "Bilagay",
        consumersColumn = "FG",
        b1 = "Pelagic_BL",
        b2 = "Benthic_BL",
        baselineColumn = "FG",
        group = "Coquimbo",
        groupsColumn = "Location"
      )
    
    model.string <- tRophicPosition::jagsBayesianModel(model = "oneBaseline", TP = "dnorm(4, 0.1)")

    model <- tRophicPosition::TPmodel(data = consumer, model.string = model.string,
                     n.adapt = 5000, n.chains = 4, quiet = TRUE)

    posterior.samples <- tRophicPosition::posteriorTP(model = model, n.iter = 15000,
                                     variable.names = c("TP", "muDeltaN"),quiet = TRUE)


    summary(posterior.samples)
  })
  
}
)
 
shinyApp(ui = ui, server = server)
