

library(shiny)
library(DT)
library(tRophicPosition)


ui <- shinyUI(fluidPage(tags$h1("Logica"),
                        sidebarLayout(
                          sidebarPanel(
                            tags$h2("Upload file"),
                            fileInput(
                              'file',
                              'Choose file to upload',
                              accept = c('text/csv',
                                         'text/comma-separated-values',
                                         '.csv')
                            ),
                            checkboxInput("header", "Header", TRUE),
                            radioButtons(
                              "sep",
                              "Separator: ",
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
                            
                            
                            
                            tags$h3('Model'),
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
                            
                            h3("Buttons"),
                            actionButton("action", "Action"),
                            br(),
                            br(), 
                            submitButton("Submit"),
                            
                            
                            
                            
                          ),
                          mainPanel(fluidRow(
                            column(width = 12,
                                   fluidRow(
                                     tags$h2("Table"),
                                     DT::dataTableOutput("sample_table")
                                   )),
                            column(width = 12,
                                   fluidPage(
                                     tags$h2("Data Summary"),
                                     verbatimTextOutput('summary')
                                   )),
                            
                            column(width = 12,
                                   fluidPage(
                                     tags$h2("Posterior Summary"),
                                     verbatimTextOutput('post_summary')
                                   )),
                            
                            column(width = 12,
                                   fluidPage(tags$h2("Plot"),
                                             plotOutput('plot'),
                                             plotOutput('plot_post'),
                                             plotOutput('plot_TP'))),
                            
                          ),)
                        ),))


server <- shinyServer(function(input, output) {
  library(tRophicPosition)
  
  
  df_upload <- reactive({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(
      inFile$datapath,
      header = input$header,
      sep = input$sep,
      quote = input$quote
    )
    
    return(df)
    
  })
  
  isotopeData <- reactive({
    df <- df_upload()
    consumer <-
      tRophicPosition::loadIsotopeData(
        df,
        consumer = input$consumer,
        consumersColumn = input$consumersColumn,
        b1 = input$b1,
        b2 = input$b2,
        baselineColumn = input$baselineColumn,
        group = input$group,
        groupsColumn = input$groupsColumn
      )
    
    return(consumer)
    
  })
  
  posterior <- reactive({
    
    consumer <- isotopeData()
    
    model.string <-
      tRophicPosition::jagsBayesianModel(model = "oneBaseline",
                                         TP = "dnorm(4, 0.1)")
    
    model <-
      tRophicPosition::TPmodel(
        data = consumer,
        model.string = model.string,
        n.adapt = input$n.adapt,
        n.chains = input$n.chains,
        quiet = TRUE
      )
    
    posterior.samples <-
      tRophicPosition::posteriorTP(
        model = model,
        n.iter = input$n.iter,
        variable.names = c("TP", "muDeltaN"),
        quiet = TRUE
      )
    
    return(posterior.samples)
    
  })
  
  
  
  output$sample_table <- DT::renderDataTable({
    df <- df_upload()
    DT::datatable(df)
  })
  
  output$summary <- renderPrint({
    summary(df_upload())
  })
  
  output$plot <- renderPlot({
    consumer <- isotopeData()
    
    plot(consumer)
    
  })
  
  output$post_summary <- renderPrint({

    posterior_samples <- posterior()
    
    
    summary(posterior_samples)
  })
  
  
  output$plot_post <- renderPlot({
    
    posterior_samples <- posterior()
    
    posterior.combined <- coda::mcmc(do.call(rbind, posterior_samples))
    
    getPosteriorMode(posterior.combined)
    
    plot(posterior_samples)
    
    
  })
  

  output$plot_TP <- renderPlot({
    consumer <- isotopeData()
    
    posterior_samples <- posterior()
    
    combined <- as.data.frame(coda::mcmc(do.call(rbind, posterior_samples)))
    
    plotTP(combined, xlab = "Monitored variables")
    
    
  })
  

})

shinyApp(ui = ui, server = server)
