library(atrrr)
library(jsonlite)
library(nominatimlite)
library(sf)
library(dplyr)
library(ggplot2)
library(osmdata)
library(crayon)
library(magick)
library(stringr)
library(stringi)
library(readr)


# prerender all the city road networks
# create roads for shiny


# load list of cities
city_list <- read_csv("data/touristy_cities.csv") |> 
  
  
  # remove accents and special characters from city query
  dplyr::mutate(city_query = stringi::stri_trans_general(str = City, 
                                                         id = "Latin-ASCII"))




# pick a random city
#random_row <- city_list |>
 # dplyr::sample_n(1) 

for(city_name in city_list$city_query){

# use the city without special characters in the name as the query
print(city_name)

if(!file.exists(paste0("data/roads/", city_name, "_buildings.geojson"))){


# Border ------------------------------------------------------------------------
# find the city's border using a reverse geocoder
# appears to work best with only the city 
city_border <- nominatimlite::geo_lite_sf(address = city_name, 
                                          points_only = FALSE)

print("city border step finished")

st_write(city_border, paste0("data/borders/", city_name, "_border.geojson"), append=FALSE)


bbox <- city_border |>
  sf::st_bbox(digits=10)


print("Bbox generated")

rm(city_border)
gc()

# Buildings ---------------------------------------------------------------------

# find building features inside bounding box
city_bldgs <- opq(bbox) |> 
  add_osm_feature(key = 'building') |> 
  osmdata::osmdata_sf()






print("city_bldgs created")

# select the building data we need?
#city_bldgs <- city_bldgs$osm_polygons |> 
# dplyr::select(osm_id, geometry)

city_bldgs <- city_bldgs$osm_polygons[, c("osm_id", "geometry")]


print("city_bldgs filtered")

st_write(city_bldgs, paste0("data/buildings/", city_name, "_buildings.geojson"), append=FALSE)

rm(city_bldgs)
gc()

# Roads ------------------------------------------------------------------------
city_roads <- opq(bbox) |> 
  add_osm_features(features = list(
    
    'highway' = 'motorway',
    'highway' = 'trunk',
    'highway' = 'primary',
    'highway' = 'secondary',
    'highway' = 'tertiary'#,
    #'highway' = 'unclassified'
  )) |> 
  
  osmdata::osmdata_sf()

print("city_roads created")


city_roads <- city_roads$osm_lines |> 
  dplyr::select(osm_id, geometry)


print("city_roads filtered")

st_write(city_roads, paste0("data/roads/", city_name, "_buildings.geojson"), append=FALSE)

rm(city_roads)
gc()


}
}

