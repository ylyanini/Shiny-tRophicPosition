library(shiny)
library(waiter)

ui <- fluidPage(
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
      stroke_color = "#8CBF3F"
    ),

  ),

  sidebarLayout(
    sidebarPanel("text"),
    mainPanel("text")
  )
  

  
)

server <- function(input, output){
  hostess <- Hostess$new("loader")
  
  for(i in 1:10){
    Sys.sleep(runif(1) / 2)
    hostess$set(i * 10)
  }
  
  waiter_hide()
}

shinyApp(ui, server)