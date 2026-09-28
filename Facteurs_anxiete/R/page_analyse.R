page_analyse_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    plotOutput(ns("graphique"))
  )
}


page_analyse_server <- function(id, donnees) {
  
  moduleServer(id, function(input, output, session) {
    
    output$graphique <- renderPlot({
      
      hist(donnees$Age)
      
    })
    
  })
}