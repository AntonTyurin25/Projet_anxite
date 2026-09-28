page_modele_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    plotOutput(ns("graphique"))
  )
}


page_modele_server <- function(id, donnees) {
  
  moduleServer(id, function(input, output, session) {
    
    output$graphique <- renderPlot({
      
      hist(donnees$age)
      
    })
    
  })
}