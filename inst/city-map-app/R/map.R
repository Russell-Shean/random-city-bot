# load the road data that was created when making the map
city_roads <- sf::st_read("city_roads.geojson")





map_UI <- function(id) {
  ns <- NS(id)
    
leafletOutput(ns("map"), width = "100%", height = "100%") |>
  shinycssloaders::withSpinner( id = "loading-gif",
                            image = "https://c.tenor.com/k29LXFgOh9QAAAAd/tenor.gif")
}


map_server <- function(id, activity_tracks) {
  moduleServer(id, function(input, output, session) {
    
    
    
    output$map <- renderLeaflet({
      
      city_roads |> 
        leaflet() |>
        addPolylines(color = "white",
                     fillOpacity = 0, 
                     weight = 1) 

      
      
    })
    
    
  })
}

