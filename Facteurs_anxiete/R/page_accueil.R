page_accueil_ui <- function(id) {
  #
  ns <- NS(id)
  
  tagList(
    h1("Accueil"),
    div(
      class = "accueil-content",
      p("Bienvenue sur l'application d'analyse des facteurs d'anxiété."),
      p("Cette application vous permet d'explorer les données, de réaliser des analyses et de modéliser les facteurs d'anxiété."),
      p("Plan avec liens hypertexte pour amener sur les pages lies, données, groupes / 1-7 / 8-10 / acp / quartiles, modelisation  avec moyen de rentrer ces params ")
    ))
  
}


page_accueil_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    

    
  })
}