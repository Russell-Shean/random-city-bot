# add libraries to shiny app
if(!require("pacman")){install.packages("pacman")}
pacman::p_load(dplyr,
               leaflet,
               sf,
               shiny)




ui <- fluidPage(
  
  # include CSS
  tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css"),
  

  #titlePanel("Upload a gpx file to get started!"),
  map_UI("map"),
  guess_box_UI("guess_box"),
  
  tags$script(src="scripts/adjust_map.js")


)
