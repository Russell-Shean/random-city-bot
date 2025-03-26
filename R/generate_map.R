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

PASSWORD <- Sys.getenv("ACCOUNT_TOKEN")

# Authenticate to Bluesky
atrrr::auth(user = "random-city-bot.bsky.social",
           password = PASSWORD)


# load list of cities
city_list <- read.csv("city_list.csv") |> 
  
             #make population a number
             dplyr::mutate(pop_as_number = readr::parse_number(Population)) |>
  

  
             # filter for cities over 400 K people
             dplyr::filter(pop_as_number > 400000) |>
  
             # remove accents and special characters from city query
             dplyr::mutate(city_query = stringi::stri_trans_general(str = City, 
                                                id = "Latin-ASCII"))


# Create a while loop to keep sampling until we get a city with buildings
no_buildings <- TRUE

while(no_buildings){
  
  
  # pick a random city
  random_row <- city_list |>
    dplyr::sample_n(1) 
  
  
  # use the city without special characters in the name as the query
  la <- random_row |>
    pull(city_query)
  
  print(la)
  
  
  # find the city's border using a reverse geocoder
  # appears to work best with only the city 
  city_border <- nominatimlite::geo_lite_sf(address = la, 
                                            points_only = FALSE)
  
  
  # define a bounding box around the city border
  bbox <- city_border |>
    sf::st_bbox(digits=10)
  
  
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
  
  
  # select the building data we need?
  city_bldgs <- city_bldgs$osm_polygons |> 
    dplyr::select(osm_id, geometry)
  
  # check to see if there are buildings
  # and if we can escape the loop
  if(nrow(city_bldgs) > 0){
    
    no_buildings <- FALSE
  }
  
  
}


# double check that the loop is working...
print(nrow(city_bldgs))

# create a ggplot map
map <- ggplot()+
  geom_sf(city_bldgs, 
          mapping = aes(color= "white"
            )) + 
  
  coord_sf(crs = 4326) +
  theme_void() +
  theme(plot.margin = margin(0,0,0,0),
         axis.ticks.length = unit(0, "pt"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=5),
        plot.background = element_rect(fill = "black")
        ) + 
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))
  #theme(plot.margin = margin(0,0,0,0,"pt"))
  #theme_bw()


# save map as image
ggsave(filename = "map.png",
       dpi = 300,
       plot = map , 
       width = (bbox$xmax - bbox$xmin)*50, 
       height = (bbox$ymax - bbox$ymin)*50#,
       #units = "px"
       )

print(file.exists("map.png"))

# post image of map to bluesky
post_results <- atrrr::post(text = "Guess which city this is!\nBot and map made with #rstats.\n\nCode and answer here: https://github.com/Russell-Shean/random-city-bot \n\nQuestions, comments, concerns? Reach out to @rshean.bsky.social",
                 image = "map.png",
                   image_alt="A map of a city somewhere in the world.\n\nView code and check your answer here: https://github.com/Russell-Shean/random-city-bot")


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
                  " | ", la,
                  " | ", random_row$Region, 
                  " | ", random_row$Country, 
                  " | ", random_row$Population,
                  " | ", post_link,
                  " |"), file_connection)          
          
close(file_connection)      
