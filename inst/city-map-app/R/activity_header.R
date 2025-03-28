activity_header_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(12, htmlOutput(ns("activity_icon"), class = "cycling-icon"), 
                    textOutput(ns("activity_start")),
                    textOutput(ns("activity_location")))),
    fluidRow(column(12, textOutput(ns("activity_title"))))
    
  )
}



activity_header_server <- function(id, activity_tracks, activity_track_points) {
  moduleServer(id, function(input, output, session) {
    
    
    
    # activity_tracks <- reactive(output$activity_tracks)
    
    output$activity_icon <- renderText({
      
      req(activity_tracks())
      
      if(activity_tracks()$type == "hiking"){
        
        
        as.character("<image class='cycling-icon' src='images/hiking.png'>")
        # <a href="https://www.flaticon.com/free-icons/hiking" title="hiking icons">Hiking icons created by juicy_fish - Flaticon</a>
        
      } else if(activity_tracks()$type == "running"){
        
        
        as.character("<image class='cycling-icon' src='images/jogging.png'>")
        # https://www.flaticon.com/free-icons/jogging" title="jogging icons">Jogging icons created by Freepik - Flaticon</a>'
        
      } else if(activity_tracks()$type == "cycling"){
        
        # https://www.flaticon.com/free-icons/bicycle" title="bicycle icons">Bicycle icons created by Freepik - Flaticon</a>
        as.character("<image class='cycling-icon' src='images/bike.png'>")
        
      } else if(activity_tracks()$type == "walking"){
        
        as.character("<image class='cycling-icon' src='images/walk.png'>")
      }
      
      
    })
    
    
    output$activity_start <- renderText({ 
      
      req(activity_track_points())
      
      start_time <- activity_track_points()$time |>
        lubridate::as_datetime() |>
        min()  |> 
        lubridate::force_tz(tzone = lutz::tz_lookup(activity_track_points()[1,], 
                                                    method = "fast")) |>
        format("%B %d, %Y at %H:%m %Z") })
    
    output$activity_location <- renderText({
      
      req(activity_track_points())
      
      
      
      rev_geo_location <- tidygeocoder::reverse_geocode(as_tibble(st_coordinates(activity_track_points()))[1,],
                                                        lat = Y, 
                                                        long = X, 
                                                        full_results = TRUE ) %>%
        
        # Set default locations in case a location isn't returned
        # or we don't find the columns we expect
        mutate(location1 = "",
               location2 = "",
               location3 = "") %>%
        
        # look for different spatial units in order of preference
        # for some reason these if( city %in% columnnames(.)) things don't work with new pipes)
        mutate( location1 = if("suburb" %in% colnames(.) &
                               country %in% c("臺灣","Taiwan")){
          suburb
        }else if("city" %in% colnames(.)){
          city
        } else if("town" %in% colnames(.)){
          town
        } else if("hamlet" %in% colnames(.)){
          hamlet
        } else if("county" %in% colnames(.)){
          county 
        } else {
          location1
        },
        location2 = if("state" %in% colnames(.)){
          state
        } else if("province" %in% colnames(.)){
          province
          
        } else if("county" %in% colnames(.)){
          county
        } else if("city" %in% colnames(.)){
          city
        }else{
          location2
        },
        location3 = if("country" %in% colnames(.)){
          country
        } else{
          location3
        }) |>
        mutate(formated_location = paste(location1, 
                                         location2, 
                                         location3,
                                         sep=", ") %>% 
                 # remove any weird commas
                 stringr::str_remove_all("^, |, ,") ) 
      
      
      rev_geo_location |> pull(formated_location)
      
      
    })
    
    
    
    
    output$activity_title <- renderText({
      
      req(activity_tracks())
      
      activity_tracks()$name
      
      
    })
    
    
    
    
    
    
  })
}

