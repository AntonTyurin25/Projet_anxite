page_donnees_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      column(
        width = 8,
        div(
          style = "width: 100%; overflow-x: auto; overflow-y: auto",
          DTOutput(ns("Table_Donnees"))
        )
      ),
      
      column(
        width = 4,
        div(
          style = "height: 500px; overflow-y: auto;",
          verbatimTextOutput(ns("skim"))
        )
      )
    ),
    
    fluidRow(
      column(
        width = 6,
        selectInput(
          inputId = ns("var"),
          label = "Variable :",
          choices = NULL
        )
      )
    ),
    
    fluidRow(
      column(
        width = 6,
        plotOutput(ns("distribution"))
      )
    )
  )
}


page_donnees_server <- function(id, donnees) {
  
  moduleServer(id, function(input, output, session) {
    
    output$Table_Donnees <- renderDT({
      donnees
    }, options = list(
      scrollX = TRUE, scrollY = TRUE
    ))
    
    
    output$skim <- renderPrint({
      skim(donnees)
    })
    
    
    observe({
      
      vars_num <- names(donnees)[
        vapply(donnees, is.numeric, logical(1))
      ]
      
      updateSelectInput(
        session = session,
        inputId = "var",
        choices = vars_num
      )
      
    })
    
    
    output$distribution <- renderPlot({
      
      req(input$var)
      
      x <- donnees[[input$var]]
      
      if (is.numeric(x)) {
        
        hist(
          x,
          col = "blue",
          main = paste("Distribution de", input$var),
          xlab = input$var,
          ylab = "Effectif"
        )
        
      } else {
        
        effectifs <- table(x, useNA = "ifany")
        
        barplot(
          effectifs,
          main = paste("Répartition de", input$var),
          xlab = input$var,
          ylab = "Effectif",
          las = 2
        )
        
      }
      
    })
    
  })
}