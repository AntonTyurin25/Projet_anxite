page_accueil_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    h1("Accueil"))
  
}


page_accueil_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    

    
  })
}