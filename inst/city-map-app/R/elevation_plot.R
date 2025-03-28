elevation_plot_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(12, plotOutput(ns("elevation_plot"), height = "500px", width = "1200px")))
  )
}


elevation_plot_server <- function(id, gpx_file) {
  moduleServer(id, function(input, output, session) {
    
    
    output$elevation_plot <- renderPlot({
      req(gpx_file())
      
      
      elevationprofile(gpx_file())
      
      
    } 
    )
    
    
    
  })
}


