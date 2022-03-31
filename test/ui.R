
library(shiny)
library(DT)
library(tRophicPosition)
library(data.table)
library(ggplot2)
library(bslib)


shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet",
              type = "text/css",
              href = "style.css"),
  ),
  tags$h1("Logica"),
  sidebarLayout(
    sidebarPanel(
      id='formulario',
      tags$h2("Archivo csv"),
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
        selected = '"',
        inline = TRUE
      ),
      # Input: Select number of rows to display ----
      radioButtons(
        "disp",
        "Display",
        choices = c(Head = "head",
                    All = "all"),
        selected = "head",
        inline = TRUE
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
    mainPanel(uiOutput("content"))
  ),
  tags$footer(
    fluidRow(
      column(4,'Logo'),
      column(4,'© 2022 Copyright tRopicPosition'),
      column(4,'Mail')
    ))
))
