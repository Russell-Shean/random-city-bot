library(rvest)
library(stringr)
library(purrr)
library(dplyr)

# for this part of the project we will load all the cities over 100.000 people
# from wikipedia


# define all the wikipedia pages
link <- c("https://en.wikipedia.org/wiki/List_of_cities_by_international_visitors")



page_html <- rvest::read_html(link)
  

# Get all the tables of the .wikitable class
tables <- page_html |>
    html_nodes(".wikitable") |>
    html_table()
  


# Hard code in the third table
# I won't need to regenerate this frequently so whatever
touristy_cities <- tables[[3]] |>
  
  dplyr::rename(country = `Country /Territory`)


# get information about regions
regions_key <- countries::country_info(touristy_cities$country, fields = c("region", "subregion")) |>
  dplyr::distinct()



touristy_cities <- touristy_cities |>
  left_join(regions_key, join_by("country"=="countries")) |>

  # add id number
  dplyr::mutate(id = 1:nrow(tables[[3]])) |>


  # Default hints to no
  dplyr::mutate(posted = "No",
                hint1 = "No", 
                hint2 = "No", 
                answered = "No")



# write out csv file
write.csv(touristy_cities, 
          file = "data/touristy_cities.csv",
          row.names = FALSE)
