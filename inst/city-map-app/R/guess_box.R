guess_box_UI <- function(id) {
  ns <- NS(id)
  
  textInput(inputId = "guess", label = "Guess a city!")
  textOutput("answer")
  

}


guess_box_server <- function(id, activity_tracks) {
  moduleServer(id, function(input, output, session) {
    
    
    
    output$answer <- renderText({
      
      
      if(input$guess == la){
        print("Yes! You got it!")
      } else if(input$guess == ""){
        print("")
      } else(
        
        print("Sorry! That's not it!")
      )
      
      
    })
      
      
    
  })
}

