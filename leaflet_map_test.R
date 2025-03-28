library(leaflet)

# background color code adapted from here: https://stackoverflow.com/a/74666935/16502170
map_background <- htmltools::tags$style(".leaflet-container { background: black; }" )  



city_bldgs |> 
  leaflet() |>
  addPolygons(color = "white",
              fillOpacity = 0, 
              weight = 1) %>%
  htmlwidgets::prependContent(map_background)

