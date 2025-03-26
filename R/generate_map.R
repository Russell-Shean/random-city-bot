library(atrrr)
library(jsonlite)
library(nominatimlite)
library(sf)
library(dplyr)
library(ggplot2)
library(osmdata)
library(magik)
library(crayon)

# Authenticate to Bluesky
atrrr::auth(user = username,
           password = password)


# load list of cities
city_list <- read.csv("city_list.csv")


# pick a random city
random_row <- city_list |>
              dplyr::sample_n(1) 



la <- random_row |>
               pull(City)



# find the city's border using a reverse geocoder
# appears to work best with only the city 
city_border <- nominatimlite::geo_lite_sf(address = la, 
                                                points_only = FALSE)


# define a bounding box around the city border
bbox <- city_border |>
        sf::st_bbox(digits=10)


# find building features inside bounding box
city_bldgs <- opq(bbox) |> 
              add_osm_feature(key = 'building') |> 
              osmdata::osmdata_sf()


# select the building data we need?
city_bldgs <- city_bldgs$osm_polygons |> 
              dplyr::select(osm_id, geometry)



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


# post image of map to bluesky
post_results <- atrrr::post(text = "Guess which city this is!\nBot and map made with rstats.\n\nCode and answer here: https://github.com/Russell-Shean/random-city-bot \n\nQuestions, comments, concerns? Reach out to @rshean.bsky.social",
                 image = "map.png",
                   image_alt="A map of a city somewhere in the world.\n\nView code and check your answer here: https://github.com/Russell-Shean/random-city-bot")


# test reply
atrrr::post(in_reply_to = post_results$uri, "Test of reply system!")

# Record the solution
file_connection <- file("solutions.md", "a")    
writeLines(paste0("| ", Sys.Date(),
                  " | ", la,
                  " | ", random_row$Region, 
                  " | ", random_row$Country, 
                  " | ", random_row$Population,
                  " | ", post_results$uri,
                  " |"), file_connection)          
          
close(file_connection)      
