page_donnees_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    # plotOutput(ns("Table_Donnees")),
    
    DTOutput(ns("Table_Donnees")),
    
    verbatimTextOutput(ns("skim"))
  )
}


page_donnees_server <- function(id, donnees) {
  
  moduleServer(id, function(input, output, session) {
    
    output$Table_Donnees <- renderDT({
      
      donnees
      
    })
    
    output$skim <- renderPrint({
      
      skim(donnees)
      
    })
    
  })
}