aknowledgements_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(12, htmlOutput(ns("aknowledgements"))))
  )
}


aknowledgements_server <- function(id, gpx_file) {
  moduleServer(id, function(input, output, session) {
    
    output$aknowledgements <- renderText({
      
      # req(input$gpx_file)
      req(gpx_file())
      
      as.character("<span>Thanks to Erik de Jong for his super cool <a href='https://github.com/EPdeJ/cyclingplots'>elevation plot</a> function!</span>")
    })
    
    
    
    # return(activity_tracks)  # Return the data for potential further use
  })
}