
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


city_roads <- city_roads$osm_lines |> 
  dplyr::select(osm_id, geometry)


city_rivers <- opq(bbox) |>
  add_osm_feature(
    key = "waterway"
  ) |> 
  
  osmdata::osmdata_sf()

# select the building data we need?
city_rivers <- city_rivers$osm_multilines|> 
  dplyr::select(osm_id, geometry)