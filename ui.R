
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
        ),
        useShinyjs(),
        actionBttn(inputId = "refresh", 
                   tags$h4("RESET"),
                   style = "fill",
                   color = "danger",
                   block = TRUE
                   ),
        
        actionLink(
          inputId = "github",
          label = NULL,
          color = "default",
          style = "default",
          icon("github"),
          onclick = "window.open(
          'https://github.com/ylyanini/Shiny-tRophicPosition',
          '_blank')",
        ),
      ),

      windowTitle = "Shiny-tRophicPosition",
      
      ## ------------------------------------------------------------
      # Upload the file
      
      tabPanel("Dataset",
               sidebarLayout(
                 sidebarPanel(
                   id = 'formulario',
                   tags$h2("Let's set the Dataset", icon("fas fa-folder")),
                   tags$hr(),
                   
                   radioGroupButtons(
                     inputId = "type_dataset",
                     choices = c('<h4>LOAD</h4>', 
                                 '<h4>GENERATE</h4>'),
                     justified = TRUE,
                     
                     status='primary',
                     checkIcon = list(
                                  yes = icon("ok",
                                  lib = "glyphicon",
                                  style = "color: #fff")),
                     selected = '<h4>LOAD</h4>'
                   ),
                  
                   uiOutput('dataset_config'),
                   
                   shinyjs::hidden(p(id = "process1", "Processing Data...")),
                   
                 ),
                 mainPanel(uiOutput("content_data"))
                 
               ),),
      
      ## ------------------------------------------------------------
      #Create IsotopeData object 
      
      tabPanel(
        "IsotopeData Object",
        id = 'IsotopeData_Object',
        sidebarLayout(
          sidebarPanel(
            id = 'formulario',
            
            tags$h2('Load TDF', icon("fas fa-cog")),
            tags$hr(),
            selectInput(
              inputId = "TDF_author",
              label = tags$h4("Author"),
              choices = c('Post (2002)' = 'Post',
                          'McCutchan (2003)' = 'McCutchan',
                          'Personalized' = 'Personalized'
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
                selected = NULL,
              )
            ),
            
            conditionalPanel(
              condition = "input.TDF_author == 'Personalized'",
              
              numericInput('DeltaN',
                           h4('DeltaN'),
                           value = 3.4, 
                           min = -100,
                           max = 100,
                           step = 0.01),
              
              numericInput('DeltaC',
                           h4('DeltaC'),
                           value = 0.39, 
                           min = -100,
                           max = 100,
                           step = 0.01),
              
              numericInput('sd.DeltaN',
                           h4('sd.DeltaN'),
                           value = 0.98, 
                           min = -100,
                           max = 100,
                           step = 0.01),
              
              numericInput('sd.DeltaC',
                           h4('sd.DeltaC'),
                           value = 1.3, 
                           min = -100,
                           max = 100,
                           step = 0.01),
              
              numericInput('n.obsDeltaN',
                           h4('n.obsDeltaN'),
                           value = 56, 
                           min = -100,
                           max = 100,
                           step = 0.01),
              
              numericInput('n.obsDeltaC',
                           h4('n.obsDeltaC'),
                           value = 107, 
                           min = -100,
                           max = 100,
                           step = 0.01),
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
            
            uiOutput("load_isotope"),
            
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
                   
                   selectInput(
                     inputId = "type_model",
                     label = tags$h4("Type of Model"),
                     choices = c("oneBaseline", 
                                 "twoBaselines", 
                                 "twoBaselinesFull",
                                 "Multiple Bayesian Models"
                     ),
                   ),
                   
                   conditionalPanel(
                     condition = "input.type_model == 'Multiple Bayesian Models'",
                     checkboxGroupButtons(
                       inputId = "models_selected",
                       label = tags$h4("Select Models"),
                       choices = c("oneBaseline", 
                                   "twoBaselines", 
                                   "twoBaselinesFull" 
                                   ),
                       individual = TRUE,
                       checkIcon = list(
                         yes = tags$i(class = "fa fa-circle", 
                                      style = "color: #005CC8"),
                         no = tags$i(class = "fa fa-circle-o", 
                                     style = "color: #005CC8"))
                     )
                     
                   ),
                   
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
