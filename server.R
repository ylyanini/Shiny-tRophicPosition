


library(shiny)
library(DT)
library(tRophicPosition)
library(data.table)
library(ggplot2)
library(bslib)
library(summarytools)

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
    DT::datatable(df,
                  class = 'cell-border stripe',
                  options = list(pageLength = 10, dom = 'tip'))
  })
  
  output$summary <- renderPrint({
    summary(df_upload())
  })
  
  output$screenfoodweb <- renderPlot({
    df <- df_upload()

    screenFoodWeb(
      df,
      grouping = c("Spp", "FG"),
      title = "Example",
      order = TRUE
    )
    
  })
  
  output$plot_content <- renderPlot({
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
  
  
  output$content_data <- renderUI({
    if (is.null(input$file)) {
      return(fluidRow(
        column(width = 12),
        column(width = 3),
        column(width = 7,
               fluidRow(
                 id = 'espera',
                 tags$h2("Ingrese datos para su analisis", icon("table")),
                 
               )),
        column(width = 2),
      ))
    } else{
      df <- df_upload()
      fluidRow(
        column(
          width = 12,
          fluidPage(
            id = 'resultados',
            tags$h2("Data Table"),
            DT::dataTableOutput("sample_table")
          )
        ),
        column(
          width = 12,
          fluidPage(
            id = 'resultados',
            tags$h2("Data Summary"),
            
            st_options(dfSummary.custom.2 =
                         expression(
                           paste(
                             "Q1 & Q4 :",
                             
                             quantile(
                               column_data,
                               probs = .25,
                               type = 2,
                               names = FALSE,
                               na.rm = TRUE
                             ),
                             digits = 1
                             ,
                             " & ",
                             
                             quantile(
                               column_data,
                               probs = 1,
                               type = 2,
                               names = FALSE,
                               na.rm = TRUE
                             ),
                             digits = 1
                             
                           )
                         )),
            
            print(
              dfSummary(
                df,
                varnumbers   = TRUE,
                valid.col    = FALSE,
                style        = "multiline",
                graph.magnif = 1.5
              ),
              method   = 'render',
              headings = FALSE,
              bootstrap.css = FALSE
            )
            
            
          )
        ),
        column(
          width = 12,
          fluidPage(id = 'resultados',
                    tags$h2("Data Species"),
                    plotOutput('screenfoodweb'), )
        )
        
      )
    }
    
  })
  
  
  output$content_TP <- renderUI({
    if (is.null(input$file)) {
      return(fluidRow(
        column(width = 12),
        column(width = 3),
        column(width = 7,
               fluidRow(
                 id = 'espera',
                 tags$h2("Ingrese datos para su analisis", icon("table")),
               )),
        column(width = 2),
      ))
    } else{
      fluidRow(
        column(
          width = 12,
          fluidPage(
            id = 'resultados',
            tags$h2("Plot Isotope Object"),
            plotOutput('plot_content')
          )
        )
      )
    }
  })
  
  
  output$content_model <- renderUI({
    if (is.null(input$file)) {
      return(fluidRow(
        column(width = 12),
        column(width = 3),
        column(width = 7,
               fluidRow(
                 id = 'espera',
                 tags$h2("Ingrese datos para su analisis", icon("table")),
               )),
        column(width = 2),
      ))
    }else{
      
    } 
    
  })
  
  
  output$content_posterior <- renderUI({
    if (is.null(input$file)) {
      return(fluidRow(
        column(width = 12),
        column(width = 3),
        column(width = 7,
               fluidRow(
                 id = 'espera',
                 tags$h2("Ingrese datos para su analisis", icon("table")),
               )),
        column(width = 2),
      ))
    }else{
      fluidRow(
        column(
          width = 12,
          fluidPage(
            id = 'resultados',
            tags$h2("Posterior Summary"),
            verbatimTextOutput('post_summary')
          )
        ),
        column(
          width = 12,
          fluidPage(id = 'resultados',
                    tags$h2("Plot"),
                    plotOutput('plot_post'),)
        ),
        column(
          width = 12,
          fluidPage(id = 'resultados',
                    tags$h2("Plot"),
                    plotOutput('plot_TP'))
        ),
      )
    } 
    
  })
  
})