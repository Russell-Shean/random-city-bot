map_UI <- function(id) {
  ns <- NS(id)
    
leafletOutput(ns("map"))
}


map_server <- function(id, activity_tracks) {
  moduleServer(id, function(input, output, session) {
    
    
    
    output$map <- renderLeaflet({
      
      city_bldgs |> 
        leaflet() |>
        addPolygons(color = "white",
                    fillOpacity = 0, 
                    weight = 1)
      
    })
    
    
  })
}

