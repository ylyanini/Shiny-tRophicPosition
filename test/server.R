
library(shiny)
library(DT)
library(tRophicPosition)
library(data.table)
library(ggplot2)
library(bslib)

shinyServer(function(input, output) {
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
    
    posterior.combined <-
      coda::mcmc(do.call(rbind, posterior_samples))
    
    getPosteriorMode(posterior.combined)
    
    plot(posterior_samples)
    
    
  })
  
  
  output$plot_TP <- renderPlot({
    consumer <- isotopeData()
    
    posterior_samples <- posterior()
    
    combined <-
      as.data.frame(coda::mcmc(do.call(rbind, posterior_samples)))
    
    plotTP(combined, xlab = "Monitored variables")
    
  })
  
  
  output$content <- renderUI({
    if (is.null(input$file)) {
      return(
        fluidRow(
          column(width = 12,
                 fluidRow(
                   id='espera',
                   tags$h2("Ingrese datos para su analisis"),
                 ))
        )
        
      )
    } else{
      fluidRow(
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
        
        column(
          width = 12,
          fluidPage(
            tags$h2("Plot"),
            plotOutput('plot'),
            plotOutput('plot_post'),
            plotOutput('plot_TP')
          )
        ),
        
      )
    }
    
  })
  
  
})