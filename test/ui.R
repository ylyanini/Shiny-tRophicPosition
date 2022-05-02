
library(shiny)
library(DT)
library(tRophicPosition)
library(ggplot2)
library(shinyWidgets)
library(summarytools)
library(waiter)


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
      HTML('<link rel="icon", href="img/logo.png",
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

                   selectInput(
                     'select_a_file',
                     tags$h3('Name of File'),
                     choices =  c(list.files('data/'), 'New')
                   ),
                   
                   uiOutput("select_file"),
                   
                   actionBttn(
                     inputId = "up_file",
                     tags$h4("UPLOAD"),
                     style = "fill",
                     color = "success",
                   ),
                   
                   
                 ),
                 mainPanel(uiOutput("content_data"),
                           #verbatimTextOutput(outputId = "res_bttn2")
                           )
               ),),
      
      ## ------------------------------------------------------------
      #Create IsotopeData object 
      
      tabPanel(
        "IsotopeData Object",
        sidebarLayout(
          sidebarPanel(
            id = 'formulario',
            tags$h2('Trophic Discrimination Factor', icon("fas fa-cog")),
            tags$hr(),
            selectInput(
              inputId = "TDF_author",
              label = tags$h4("author"),
              choices = c('Post (2002)',
                          'McCutchan (2003)',
                          'New'),
              selected = 'Post (2002)'
            ),
            
            uiOutput("type_select"),
            
            selectInput('Idq123',
                        h4('element'),
                        choices = c('Both', 'N', 'C')),
            
            
            tags$h2('Load Isotope Data', icon("fas fa-filter")),
            tags$hr(),
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
                     color = "success"
                   ),
                   
                   uiOutput("panel_post_sample"),
                   
                 ),
                 
                 mainPanel(uiOutput("content_model"))
               )),
    )
  )
)
