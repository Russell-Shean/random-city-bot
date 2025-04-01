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

PASSWORD <- Sys.getenv("ACCOUNT_TOKEN")

# Authenticate to Bluesky
atrrr::auth(user = "random-city-bot.bsky.social",
           password = PASSWORD)


# load list of cities
city_list <- read.csv("data/touristy_cities.csv") |> 
  
  
             # remove accents and special characters from city query
             dplyr::mutate(city_query = stringi::stri_trans_general(str = City, 
                                                id = "Latin-ASCII"))


# Create a while loop to keep sampling until we get a city with buildings
no_buildings <- TRUE

while(no_buildings){
  
  
  # pick a random city
  random_row <- city_list |>
    dplyr::sample_n(1) 
  
  
  # Try a different city if we've already posted this one
  if(random_row$posted == "yes"){
    next
  }
  
  
  # use the city without special characters in the name as the query
  la <- random_row |>
    pull(city_query)
  
  print(la)
  
  
  # find the city's border using a reverse geocoder
  # appears to work best with only the city 
  city_border <- nominatimlite::geo_lite_sf(address = la, 
                                            points_only = FALSE)
  
  print("city border step finished")
  
  
  # define a bounding box around the city border
  bbox <- city_border |>
    sf::st_bbox(digits=10)
  
  
  print("Bbox generated")
  
  
  # If the bounding box has NA values
  # Break the loop and try again with a new city 
  # It probably means the reverse geocoder couldn't find the city...
  if(NA %in% bbox){
    print("Uh oh! The geocoder couldn't find that city!")
    print("Trying again with a new city!")
    
    
    #skip to the next iteration of the while loop
    next
    
    }
  
  
  # find building features inside bounding box
  city_bldgs <- opq(bbox) |> 
    add_osm_feature(key = 'building') |> 
    osmdata::osmdata_sf()

  print("city_bldgs created")
  
  # select the building data we need?
  city_bldgs <- city_bldgs$osm_polygons |> 
    dplyr::select(osm_id, geometry)
  
  print("city_bldgs filtered")
  
  
  # check to see if there are buildings
  # and if we can escape the loop
  if(nrow(city_bldgs) < 1){
    
    next
  }
  
  
# create roads for shiny
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
  
  
  # save the city roads to the data folder
  saveRDS(city_roads, "inst/city-map-app/city_roads.rda")
  
  
  if(nrow(city_bldgs) > 0){
    
    no_buildings <- FALSE
    
  }
  
  
}


# double check that the loop is working...
print(nrow(city_bldgs))

# create a ggplot map
#map1 <- 
  
  
map1 <-  ggplot()+
  
  # add city buildings for entire bounding box
  geom_sf(city_bldgs, 
          mapping = aes(),
          #color= "white"
          ) + 
  
  # add outline of city border
  geom_sf(city_border,
          mapping = aes(),
          color = alpha("#e61b23", 0.7), 
          fill = NA,
          linewidth = 1.25) +
  
  coord_sf(crs = 4326) +
  theme_void() +
  theme(
        # Remove plot margins
        plot.margin = margin(0,0,0,0),
        
        #remove axis ticks
        axis.ticks.length = unit(0, "pt"),
        
        # remove legends
        legend.position="none", 
        
        # Create a black outline
        panel.border = element_rect(colour = "black", fill=NA, linewidth=5),
        
        # bakc the plot background black
        #plot.background = element_rect(fill = "black")
        
        ) #+ 
  
  
  scale_x_continuous(expand=c(0,0)) #+
  scale_y_continuous(expand=c(0,0))
  #theme(plot.margin = margin(0,0,0,0,"pt"))
  #theme_bw()

print("map1 finished")


# create a ggplot map
map2 <- ggplot()+
  
  # add city buildings for entire bounding box
  geom_sf(city_bldgs, 
          mapping = aes(),
          color= "white"
  ) + 
  
  # add outline of city border
  geom_sf(city_border,
          mapping = aes(),
          color = alpha("#e61b23", 0.7), 
          fill = NA,
          linewidth = 1.25) +
  
  coord_sf(crs = 4326) +
  theme_void() +
  theme(
    # Remove plot margins
    plot.margin = margin(0,0,0,0),
    
    #remove axis ticks
    axis.ticks.length = unit(0, "pt"),
    
    # remove legends
    legend.position="none", 
    
    # Create a black outline
    panel.border = element_rect(colour = "black", fill=NA, linewidth=5),
    
    # make the plot background black
    plot.background = element_rect(fill = "black")
    
  ) + 
  
  
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))
#theme(plot.margin = margin(0,0,0,0,"pt"))
#theme_bw()

print("map2 finished")


# Attempt to save the image
# we'll keep looping until it's the right size

# We'll start by assuming the file is too large for bluesky 
# and keep incrementing it down until it's not
image_too_big <- TRUE
size_factor <- 51

while(image_too_big){

  # decrease the size factor by 1
  size_factor <- size_factor - 1
  
  # save map as image
  ggsave(filename = "map1.png",
         dpi = 300,
         plot = map1 , 
         width = (bbox$xmax - bbox$xmin)* size_factor, 
         height = (bbox$ymax - bbox$ymin)* size_factor,
         limitsize = FALSE
         #units = "px"
  )
  
  # check file size
  if(file.info("map1.png")$size < 976560){
    
    image_too_big <- FALSE

    }

}

print("image 1 finished")

# we will eventually turn this into a function
# instead of this super lazy copy and paste stuff lol
image_too_big <- TRUE
size_factor <- 51

while(image_too_big){
  
  # decrease the size factor by 1
  size_factor <- size_factor - 1
  
  # save map as image
  ggsave(filename = "map2.png",
         dpi = 300,
         plot = map2 , 
         width = (bbox$xmax - bbox$xmin)* size_factor, 
         height = (bbox$ymax - bbox$ymin)* size_factor,
         limitsize = FALSE
         #units = "px"
  )
  
  # check file size
  if(file.info("map2.png")$size < 976560){
    
    image_too_big <- FALSE
    
  }
  
  
  
}

print("image 2 finished")

# Print to confirm the map was created
print(paste("File exists:", file.exists("map1.png")))
print(paste("File exists:", file.exists("map1.png")))


# post image of map to bluesky
post_results <- atrrr::post(text = "Guess which city this is!\n\nCode and answer here:https://github.com/Russell-Shean/random-city-bot \n\nMap and Bot Built by:@rshean.bsky.social\n\n#MapQuiz",
                 image = c("map1.png", "map2.png"),
                 image_alt = c("A map of a city somewhere in the world\nMap generated using data from:https://www.openstreetmap.org", "A map of a city somewhere in the world\nMap generated using data from:https://www.openstreetmap.org"))


#pause because I doubt the posting is instaneous
sleep(5)

# reply with link to the shiny

tryCatch(

  {
    post2_results <- atrrr::post(in_reply_to = post_results$uri,
                                 text = "Here's a link to shiny showing the city's street network:\nhttps://random-city-bot.shinyapps.io/todays-city/")
    
    
  }, error = function(msg){
    print("error posting yo. Here was the error")
    print(msg)
    
  })





print("posting finished")




#print(post_results)
# format a link from uri
post_id <- post_results$uri |> str_extract("(?<=app.bsky.feed.post/).*")

post_link <- paste0("<a id='",
                    post_results$uri, 
                    "' href='https://bsky.app/profile/random-city-bot.bsky.social/post/",
                    post_id,
                    "'>Link</a>")




# Record the solution
file_connection <- file("solutions.md", "a")    
writeLines(paste0("| ", Sys.Date(),
                  " | ", random_row$City,
                  " | ", random_row$country, 
                  " | ", random_row$subregion, 
                  " | ", random_row$region,
                  " | ", post_link,
                  " | ", random_row$id,
                  " |"), file_connection)          
          
close(file_connection)    

print("solutions.md updated")


# Move images into archive folder
if(!dir.exists("archive")){dir.create("archive")}

file.copy(from = "map1.png",
          to = paste0("archive/", la, "1.png"))

file.copy(from = "map2.png",
          to = paste0("archive/", la, "2.png"))


print("images archived")


# update touristy_cities
city_list |> 
  mutate(posted = ifelse(id == random_row$id,
                        "yes",
                        posted)) |>
  write.csv(file = "data/touristy_cities.csv")

print("city list updated")


