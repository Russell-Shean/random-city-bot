library(leaflet)

# background color code adapted from here: https://stackoverflow.com/a/74666935/16502170
map_background <- htmltools::tags$style(".leaflet-container { background: black; }" )  




  leaflet() |>
 addPolylines(data = city_roads,
             color = "white",
             fillOpacity = 0, 
             weight = 1) %>%
 # addPolygons(city_rivers, 
  #            fill = "blue",
   #           color= "blue") |>

addPolylines(data = city_rivers) |> 
    htmlwidgets::prependContent(map_background) 
 