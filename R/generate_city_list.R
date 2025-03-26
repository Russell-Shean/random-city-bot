library(rvest)
library(stringr)
library(purrr)
library(dplyr)

# for this part of the project we will load all the cities over 100.000 people
# from wikipedia


# define all the wikipedia pages
links <- c("https://en.wikipedia.org/wiki/List_of_towns_and_cities_with_100,000_or_more_inhabitants/country:_A-B",
           "https://en.wikipedia.org/wiki/List_of_towns_and_cities_with_100,000_or_more_inhabitants/country:_C-D-E-F",
           "https://en.wikipedia.org/wiki/List_of_towns_and_cities_with_100,000_or_more_inhabitants/country:_G-H-I-J-K",
           "https://en.wikipedia.org/wiki/List_of_towns_and_cities_with_100,000_or_more_inhabitants/country:_L-M-N-O",
           "https://en.wikipedia.org/wiki/List_of_towns_and_cities_with_100,000_or_more_inhabitants/country:_P-Q-R-S",
           "https://en.wikipedia.org/wiki/List_of_towns_and_cities_with_100,000_or_more_inhabitants/country:_T-U-V-W-Y-Z")


# define a function to scrape cities from wikipedia
get_cities <- function(link){

# read html
page_html <- rvest::read_html(link)

# pull countries for the page
countries <- page_html |> 
             html_nodes(".mw-heading") |>
            html_nodes("h2") |>
            html_attr("id") |> 
  
            # remove headers without ids
            purrr::discard(is.na) |>
  
            # remove see_also and references from the list
            stringr::str_subset("See_also|References",negate = TRUE)



# Get all the tables of the .wikitable class
city_tables <- page_html |>
               html_nodes(".wikitable") |>
               html_table()




# for loop ---- I don't care if it's slow!!
for(i in seq_along(city_tables)){
  
  # standardize column names
  colnames(city_tables[[i]]) <- c("City", "Region", "Population")
  
  
  # add country to the list
  city_tables[[i]] <- city_tables[[i]] |>
    dplyr::mutate(Country = countries[i])
  
}

# combine all the tables into one data frame
all_cities <- city_tables |> 
              dplyr::bind_rows()

# return the dataframe
all_cities

}


# generate a list of tables from all the pages
all_cities <- lapply(links, get_cities)  |> 
              dplyr::bind_rows() |>
              
              # create a field for querying the openstreets map api
              # for now we'll try just city and country
              dplyr::mutate(city_country = paste(City, Country, sep = ", "))


# write out csv file
write.csv(all_cities, 
          file = "city_list.csv",
          row.names = FALSE)
