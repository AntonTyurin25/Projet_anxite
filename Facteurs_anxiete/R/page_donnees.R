page_donnees_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    h2("Données"),
    
    fileInput(
      ns("fichier"),
      "Importer un fichier"
    ),
    
    tableOutput(ns("table"))
  )
}


page_donnees_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    donnees <- reactive({
      req(input$fichier)
      
      read.csv(input$fichier$datapath)
    })
    
    output$table <- renderTable({
      donnees()
    })
    
  })
}