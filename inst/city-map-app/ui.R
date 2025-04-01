# add libraries to shiny app
if(!require("pacman")){install.packages("pacman")}
pacman::p_load(dplyr,
               leaflet,
               sf,
               shiny)


# load the road data that was created when making the map
sf::st_read("inst/city-map-app/city_roads.geojson")


ui <- fluidPage(
  
  # include CSS
  tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css"),
  

  #titlePanel("Upload a gpx file to get started!"),
  map_UI("map"),
  guess_box_UI("guess_box"),
  
  tags$script(src="scripts/adjust_map.js")


)
