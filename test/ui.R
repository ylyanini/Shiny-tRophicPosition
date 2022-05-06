
library(DT)
library(shiny)
library(waiter)
library(shinyjs)
library(shinyWidgets)
library(summarytools)
library(tRophicPosition)


shinyUI(
  fluidPage(
    useWaiter(),
    useHostess(),
    waiterShowOnLoad(
      color = "#F1FCF8",
      hostess_loader(
        "loader",
        preset = "circle",
        text_color = "black",
        class = "label-center",
        center_page = TRUE,
        fill_color = "#5fd3a7",
        stroke_color = "#69FE46"
      ),
      
    ),
    
    tags$head(
      tags$link(rel = "stylesheet",
                type = "text/css",
                href = "style.css"),
    ),
    
    tags$head(
      HTML('<link rel="icon", 
                  href="img/logo.png",
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
      # Upload the file
      
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
                       'Comma' = ",",
                       'Semicolon' = ";",
                       'Tab' = "\t"
                     ),
                     selected = ",",
                     inline = TRUE
                   ),
                   
                   radioButtons(
                     "quote",
                     tags$h3("Comma"),
                     choices = c(
                       'None' = "",
                       'Double Quote' = '"',
                       "Single Quote" = "'"
                     ),
                     selected = '"',
                     inline = TRUE
                   ),
                   
                   radioButtons(
                     inputId = 'header',
                     label = tags$h3('Header'),
                     choices = c(
                       'Yes' = TRUE,
                       'No' =  FALSE),
                     selected = TRUE,
                     inline = TRUE,
                   ),
                  
                   # Select file (under development)
                   # selectInput(
                   #   'select_a_file',
                   #   tags$h3('Name of File'),
                   #   choices =  c(list.files('data/'), 'New'),
                   #   selected = NULL
                   # ),
                   # 
                   # uiOutput("select_file"),
                   
                   fileInput(
                     'file',
                     tags$h3('Upload a File'),
                     accept = c('text/csv',
                                'text/comma-separated-values',
                                '.csv'),
                     buttonLabel = tags$b("UPLOAD"),
                     placeholder = "Example.csv",
                     multiple = FALSE,
                   ),
                   
                   actionBttn(
                     inputId = "up_file",
                     tags$h4("UPLOAD"),
                     style = "fill",
                     color = "success",
                     size = "sm",
                     block = TRUE
                   ),
                  
                  useShinyjs(),
                   actionBttn(inputId = "refresh", 
                              tags$h4("CLEAR"),
                              style = "fill",
                              color = "danger",
                              block = TRUE,
                              
                              )
                   
                   
                 ),
                 mainPanel(uiOutput("content_data"))
                          
               ),),
      
      ## ------------------------------------------------------------
      #Create IsotopeData object 
      
      tabPanel(
        "IsotopeData Object",
        sidebarLayout(
          sidebarPanel(
            id = 'formulario',
            uiOutput("load_isotope"),
            
            tags$h2('Load TDF', icon("fas fa-cog")),
            tags$hr(),
            selectInput(
              inputId = "TDF_author",
              label = tags$h4("author"),
              choices = c('Post (2002)' = 'Post',
                          'McCutchan (2003)' = 'McCutchan'
                          # 'New'
                          ),
            ),
            
            conditionalPanel(
              condition = "input.TDF_author == 'McCutchan'",
              selectInput(
                'type',
                tags$h4('type'),
                choices = c(
                  'All' = 'all',
                  'Whole' = 'whole',
                  'Muscle' = 'muscle',
                  'Acidified' = 'acidified',
                  'Unacidified' = 'unacidified',
                  'Rainbow Trout' = 'rainbowTrout',
                  'Brook Trout' = 'brookTrout'
                ),
              )
            ),
            
            selectInput('element',
                        h4('element'),
                        choices = c('Both' = 'both',
                                    'N' = 'N',
                                    'C' = 'C'),
                        ),
            
            actionBttn(
              inputId = "TDF_set",
              tags$h4("SET TDF"),
              style = "fill",
              color = "success",
              size = "sm",
              block = TRUE
            ),
            
            
            
          ),
          mainPanel(uiOutput("content_TP"))
        )
      ),
      
      ## ------------------------------------------------------------
      #Make Posterior Samples
      tabPanel('Posterior Sample',
               sidebarLayout(
                 sidebarPanel(
                   id = 'formulario',
                   uiOutput("panel_post_sample"),
                   tags$h2('Create Model', icon("fas fa-cubes")),
                   tags$hr(),
                   numericInput('n.chains',
                                h4('n.chains'),
                                value = 4),
                   numericInput('n.adapt',
                                h4('n.adapt'),
                                value = 5000),
                   
                   actionBttn(
                     inputId = "create_model",
                     tags$h4("CREATE MODEL"),
                     style = "fill",
                     color = "success",
                     size = "sm",
                     block = TRUE
                   ),
                   
                   conditionalPanel(
                     condition = "input.create_model > 0",
                     tags$h2('Make Post Samples', icon("fas fa-chart-bar"),
                             style = "margin-top: 25px;"),
                     tags$hr(),
                     numericInput('n.iter',
                                  h4('n.iter'),
                                  value = 15000),
                     
                     actionBttn(
                       inputId = "make_post",
                       tags$h4("MAKE POST SAMPLES"),
                       style = "fill",
                       color = "success",
                       size = "sm",
                       block = TRUE
                     ),
                   )
                   
                   
                   
                 ),
                 
                 mainPanel(uiOutput("content_model"))
               )),
    ),
  )
)
