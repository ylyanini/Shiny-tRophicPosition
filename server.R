
library(DT)
library(shiny)
library(waiter)
library(shinyjs)
library(shinyWidgets)
library(summarytools)
library(shinyscreenshot)
library(tRophicPosition)


shinyServer(function(input, output) {
  
  ## ------------------------------------------------------------
  # Global Waiter
  
  
  hostess <- Hostess$new("loader")
  
  for (i in 1:10) {
    Sys.sleep(runif(1) / 3)
    hostess$set(i * 10)
  }
  waiter_hide()
  
  observeEvent(input$refresh, {
    refresh()
  })
  
  ## ------------------------------------------------------------
  # Upload file
  
  
  # make_df <- function() {
  #   inFile <- input$file
  #   if (is.null(inFile)) {
  #     return(NULL)
  #   } else{
  #     df <- read.csv(
  #       inFile$datapath,
  #       header = as.logical(input$header),
  #       sep = input$sep,
  #       quote = input$quote
  #     )
  #     
  #     return(df)
  #   }
  # }
  
  df_upload <- reactive({
    inFile <- input$file
    if (is.null(inFile)) {
      return(NULL)
    } else{
      df <- read.csv(
        inFile$datapath,
        header = as.logical(input$header),
        sep = input$sep,
        quote = input$quote
      )
      
      return(df)
    }
  })
  
  output$sample_table <- DT::renderDataTable({
    df <- df_upload()
    DT::datatable(df,
                  class = 'cell-border stripe',
                  options = list(pageLength = 10, dom = 'tip'))
  })
  
  plot_screenfoodweb <- function() {
    df <- df_upload()
    tRophicPosition::screenFoodWeb(
      df,
      grouping = c("Spp", "FG"),
      title = "Food Webs",
      order = TRUE
    )
  }
  
  output$screenfoodweb <- renderPlot({
    return(plot_screenfoodweb())
    
  })
  
  # Select file (under development)
  # observeEvent(input$select_a_file, {
  #   output$select_file <- renderUI({
  #     if (input$select_a_file == 'New') {
  #       fileInput(
  #         'file',
  #         tags$h3('Select File'),
  #         accept = c('text/csv',
  #                    'text/comma-separated-values',
  #                    '.csv'),
  #         buttonLabel = tags$b("UPLOAD"),
  #         placeholder = "Example.csv",
  #         multiple = FALSE,
  #
  #       )
  #
  #     } else{
  #       return()
  #     }
  #   })
  # })
  
  observeEvent(input$download_dfSummary,{
    screenshot(
      "#dfSummary",
      filename = paste("dfSummary_", Sys.Date(), ".png", sep = ""),
      timer = 1,
      scale = 3
    )
  })
  
  observeEvent(input$download_screenfoodweb,{
    screenshot(
      "#screenfoodweb",
      filename = paste("screenfoodweb_", Sys.Date(), ".png", sep = ""),
      timer = 1,
      scale = 3
    )
  })
  
  observe({
    if(is.null(input$file)){
      disable('up_file')
      disable('TDF_set')
      disable('create_model')
    }else{
      enable('up_file')
      enable('TDF_set')
      enable('create_model')
    }
  })
  
  # observe({
  #   if (input$create_isotop == 0){
  #     disable('create_model')
  #   }else{
  #     enable('create_model')
  #   }
  # })
  
  observeEvent(input$up_file, {
    shinyjs::disable('file')
    # updateButton(session, 'up_file', style='default')
    shinyjs::disable('up_file')
    shinyjs::show("process1")
    Sys.sleep(2)
    shinyjs::hide("process1")
  })
  
  output$content_data <- renderUI({
    df <- df_upload()
    if (as.logical(input$up_file) == TRUE &&
        !is.null(input$file)) {
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
            
            div(
              id="dfSummary",
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
              ),
            ),
            actionBttn(
              "download_dfSummary",
              label = "DOWNLOAD SUMMARY",
              icon("camera"),
              style = "jelly",
              color = "warning",
              size = "xs"
            ),
          )
        ),
        
        column(
          width = 12,
          fluidPage(
            id = 'resultados',
            tags$h2("Data Species"),
            div(
              id="screenfoodweb",
              plotOutput("screenfoodweb"),
            ),
            br(),
            actionBttn(
              "download_screenfoodweb",
              label = "DOWNLOAD PLOT",
              icon("camera"),
              style = "jelly",
              color = "warning",
              size = "xs"
            ),
          )
        )
        
      )
      
      
    }
    else{
      fluidRow(
        column(width = 12),
        column(width = 3),
        column(width = 7,
               fluidRow(
                 id = 'espera',
                 tags$h2("Upload your data to make some analysis",
                         icon("smile")),
               )),
        column(width = 2),
      )
    }
  })
  
  
  # plotReady <- reactiveValues(ok = FALSE)
  # 
  # observeEvent(input$Button1, {
  #   shinyjs::disable("Button1")
  #   shinyjs::show("text1")
  #   plotReady$ok <- FALSE
  #   # do some cool and complex stuff
  #   Sys.sleep(2)
  #   plotReady$ok <- TRUE
  # })
  # 
  # output$plot <-renderPlot({
  #   if (plotReady$ok) {
  #     shinyjs::enable("Button1")
  #     shinyjs::hide("text1")
  #     hist(rnorm(100, 4, 1),breaks = 50)
  #   }
  # })
  
  
  ## ------------------------------------------------------------
  # IsotopeData Object
  
  
  TDF_values <- eventReactive(input$TDF_set, {
    return(
      tRophicPosition::TDF(
        author = input$TDF_author,
        element = input$element,
        type = input$type
      )
    )
  })
  
  
  isotopeData <- reactive({
    df <- df_upload()
    TDF <- TDF_values()
    
    if (input$element == 'N') {
      consumer <-
        tRophicPosition::loadIsotopeData(
          df,
          consumer = input$consumer,
          consumersColumn = input$consumersColumn,
          b1 = input$b1,
          b2 = input$b2,
          baselineColumn = input$baselineColumn,
          group = input$group,
          groupsColumn = input$groupsColumn,
          deltaN = TDF$deltaN,
          deltaC = NULL
        )
      
    } else if (input$element == 'C') {
      consumer <-
        tRophicPosition::loadIsotopeData(
          df,
          consumer = input$consumer,
          consumersColumn = input$consumersColumn,
          b1 = input$b1,
          b2 = input$b2,
          baselineColumn = input$baselineColumn,
          group = input$group,
          groupsColumn = input$groupsColumn,
          deltaC = TDF$deltaC,
          deltaN = NULL
        )
      
    } else{
      consumer <-
        tRophicPosition::loadIsotopeData(
          df,
          consumer = input$consumer,
          consumersColumn = input$consumersColumn,
          b1 = input$b1,
          b2 = input$b2,
          baselineColumn = input$baselineColumn,
          group = input$group,
          groupsColumn = input$groupsColumn,
          deltaN = TDF$deltaN,
          deltaC = TDF$deltaC
        )
    }
    
    return(consumer)
    
  })
  
  output$plot_consumer <- renderPlot({
    consumer <- isotopeData()
    plot(consumer, b1 = input$b1, b2 = input$b2)
  })
  
  output$tdf_summary <- renderPrint({
    TDF <- TDF_values()
    return(TDF)
  })
  
  output$content_TP <- renderUI({
    if (is.null(input$file)) {
      fluidRow(
        column(width = 12),
        column(width = 3),
        column(width = 7,
               fluidRow(
                 id = 'espera',
                 tags$h2(
                   "Remember upload your data to make the analysis",
                   icon("smile")
                 ),
                 
               )),
        column(width = 2),
      )
    } else
      (return(fluidRow(
        column(width = 12),
        column(width = 3),
        column(width = 7,
               fluidRow(
                 id = 'espera',
                 tags$h2("Define your TDF",
                         icon("thumbs-up")),
               )),
        column(width = 2),
      )))
    
  })
  
  observeEvent(input$TDF_set, {
    df <- df_upload()
    output$load_isotope <- renderUI({
      fluidRow(
        column(
          width = 12,
          tags$h2('Load Isotope Data', icon("fas fa-filter")),
          tags$hr(),
          
          selectInput(
            'consumer',
            tags$h4('consumer'),
            choices =  c(as.list(df$Spp)),
            selected = TRUE,
          ),
          
          selectInput(
            'group',
            tags$h4('group'),
            choices =  c(as.list(df$Location)),
            selected = TRUE,
          ),
          
          selectInput(
            'b1',
            tags$h4('b1'),
            choices =  c(as.list(df$FG)),
            selected = TRUE,
          ),
          
          selectInput(
            'b2',
            tags$h4('b2'),
            choices =  c(as.list(df$FG)),
            selected = TRUE,
          ),
          
          selectInput(
            'consumersColumn',
            tags$h4('consumersColumn'),
            choices =  c(colnames(df)),
            selected = 'FG'
          ),
          
          selectInput(
            'baselineColumn',
            tags$h4('baselineColumn'),
            choices =  c(colnames(df)),
            selected = 'FG'
          ),
          
          selectInput(
            'groupsColumn',
            tags$h4('groupsColumn'),
            choices =  c(colnames(df)),
            selected = 'Location'
          ),
          
          
          actionBttn(
            inputId = "create_isotop",
            tags$h4("CREATE ISOTOPEDATA OBJECT"),
            style = "fill",
            color = "success",
            size = "sm",
            block = TRUE
          ),
        )
      )
    })
  })
  
  
  
  observeEvent(input$TDF_set, {
    output$content_TP <- renderUI({
      if (is.null(input$file)) {
        fluidRow(
          column(width = 12),
          column(width = 3),
          column(width = 7,
                 fluidRow(
                   id = 'espera',
                   tags$h2(
                     "Remember upload your data to make the analysis",
                     icon("smile")
                   ),
                   
                 )),
          column(width = 2),
        )
      } else
        (return(fluidRow(
          column(width = 12),
          column(width = 3),
          column(width = 7,
                 fluidRow(
                   id = 'espera',
                   tags$h2(
                     "Now lets define your Isotope Data Object",
                     icon("thumbs-up")
                   ),
                 )),
          column(width = 2),
        )))
    })
  })
  
  observeEvent(input$download_tdf_summary,{
    screenshot(
      "#tdf_summary",
      filename = paste("tdf_summary", Sys.Date(), ".png", sep = ""),
      timer = 1,
      scale = 3
    )
  })
  
  observeEvent(input$download_plot_consumer,{
    screenshot(
      "#plot_consumer",
      filename = paste("plot_consumer_", Sys.Date(), ".png", sep = ""),
      timer = 1,
      scale = 3
    )
  })
  
  observeEvent(input$create_isotop, {
    output$content_TP <- renderUI({
      if (is.null(input$file)) {
        fluidRow(
          column(width = 12),
          column(width = 3),
          column(width = 7,
                 fluidRow(
                   id = 'espera',
                   tags$h2(
                     "Remember upload your data to make the analysis",
                     icon("smile")
                   ),
                   
                 )),
          column(width = 2),
        )
      } else
        (return(fluidRow(
          Sys.sleep(2),
          column(
            width = 12,
            fluidPage(
              id = 'resultados',
              tags$h2("Trophic Discrimination Factor"),
              div(
                id = "tdf_summary",
                verbatimTextOutput('tdf_summary')
              ),
              br(),
              actionBttn(
                "download_tdf_summary",
                label = "DOWNLOAD SUMMARY",
                icon("camera"),
                style = "jelly",
                color = "warning",
                size = "xs"
              ),
            )
          ),
          column(
            width = 12,
            fluidPage(
              id = 'resultados',
              tags$h2("Plot Isotope Object"),
              div(
                id = "plot_consumer",
                plotOutput('plot_consumer')
              ),
              br(),
              actionBttn(
                "download_plot_consumer",
                label = "DOWNLOAD PLOT",
                icon("camera"),
                style = "jelly",
                color = "warning",
                size = "xs"
              ),
            )
          ),
          
        )))
    })
  })
  
  
  
  ## ------------------------------------------------------------
  # Posterior Sample
  
  
  output$post_summary <- renderPrint({
    posterior_samples <- posterior()
    
    
    summary(posterior_samples)
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
  
  output$plot_TP <- renderPlot({
    consumer <- isotopeData()
    
    posterior_samples <- posterior()
    
    combined <-
      as.data.frame(coda::mcmc(do.call(rbind, posterior_samples)))
    
    plotTP(combined, xlab = "Monitored variables")
    
  })
  
  output$plot_post <- renderPlot({
    posterior_samples <- posterior()
    
    posterior.combined <-
      coda::mcmc(do.call(rbind, posterior_samples))
    
    getPosteriorMode(posterior.combined)
    
    plot(posterior_samples)
    
    
  })
  
  output$content_model <- renderUI({
    if (is.null(input$file)) {
      fluidRow(
        column(width = 12),
        column(width = 3),
        column(width = 7,
               fluidRow(
                 id = 'espera',
                 tags$h2(
                   "Remember upload your data to make the analysis",
                   icon("smile")
                 ),
                 
               )),
        column(width = 2),
      )
    } else
      (return(fluidRow(
        column(width = 12),
        column(width = 3),
        column(width = 7,
               fluidRow(
                 id = 'espera',
                 tags$h2("Let's create the model now",
                         icon("thumbs-up")),
               )),
        column(width = 2),
      )))
    
  })
  
  
  observeEvent(input$create_model, {
    output$content_model <- renderUI({
      if (is.null(input$file)) {
        fluidRow(
          column(width = 12),
          column(width = 3),
          column(width = 7,
                 fluidRow(
                   id = 'espera',
                   tags$h2(
                     "Remember upload your data to make the analysis",
                     icon("smile")
                   ),
                   
                 )),
          column(width = 2),
        )
      } else
        (return(fluidRow(
          column(width = 12),
          column(width = 3),
          column(width = 7,
                 fluidRow(
                   id = 'espera',
                   tags$h2("Let's make the posterior samples",
                           icon("thumbs-up")),
                 )),
          column(width = 2),
        )))
    })
  })
  
  
  observeEvent(input$download_post_summary,{
    screenshot(
      "#post_summary",
      filename = paste("post_summary_", Sys.Date(), ".png", sep = ""),
      timer = 1,
      scale = 3
    )
  })
  
  observeEvent(input$download_plot_post,{
    screenshot(
      "#plot_post",
      filename = paste("plot_post_", Sys.Date(), ".png", sep = ""),
      timer = 1,
      scale = 3
    )
  })
  
  observeEvent(input$download_plot_TP,{
    screenshot(
      "#plot_TP",
      filename = paste("plot_TP_", Sys.Date(), ".png", sep = ""),
      timer = 1,
      scale = 3
    )
  })
  
  
  
  
  observeEvent(input$make_post, {
    output$content_model <- renderUI({
      if (is.null(input$file)) {
        fluidRow(
          column(width = 12),
          column(width = 3),
          column(width = 7,
                 fluidRow(
                   id = 'espera',
                   tags$h2(
                     "Remember upload your data to make the analysis",
                     icon("smile")
                   ),
                   
                 )),
          column(width = 2),
        )
      } else
        (return(fluidRow(
          column(
            width = 12,
            fluidPage(
              id = 'resultados',
              tags$h2("Posterior Summary"),
              div(
                id = "post_summary",
                verbatimTextOutput('post_summary')
              ),
              br(),
              actionBttn(
                "download_post_summary",
                label = "DOWNLOAD SUMMARY",
                icon("camera"),
                style = "jelly",
                color = "warning",
                size = "xs"
              ),
              
            )
          ),
          column(
            width = 12,
            fluidPage(id = 'resultados',
                      tags$h2("Plot"),
                      div(
                        id = "plot_post",
                        plotOutput('plot_post'),
                      ),
                      br(),
                      actionBttn(
                        "download_plot_post",
                        label = "DOWNLOAD PLOT",
                        icon("camera"),
                        style = "jelly",
                        color = "warning",
                        size = "xs"
                      ),
            )
          ),
          column(
            width = 12,
            fluidPage(id = 'resultados',
                      tags$h2("Plot"),
                      div(
                        id = "plot_TP",
                        plotOutput('plot_TP'),
                      ),
                      br(),
                      actionBttn(
                        "download_plot_TP",
                        label = "DOWNLOAD PLOT",
                        icon("camera"),
                        style = "jelly",
                        color = "warning",
                        size = "xs"
                      ),
            )
          ),
        )))
    })
  })
  
  ## ------------------------------------------------------------
})