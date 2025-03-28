gpx_upload_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("gpx_file"), "Upload GPX File", 
              accept = c(".gpx"))
  )
}


gpx_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    gpx_file <- reactive({
      
      req(input$gpx_file)
      
      input$gpx_file$datapath
      
    })
    return(gpx_file)
  })
}

tracks_server <- function(id, gpx_file) {
  moduleServer(id, function(input, output, session) {
    
    activity_tracks <- reactive({
      req(gpx_file())
      
      # Read the GPX file using sf::st_read()
      activity_tracks <- st_read(gpx_file(), layer = "tracks", quiet = TRUE)
      
    })
    
    # ouput$activity_tracks <- reactive({activity_tracks()})
    return(activity_tracks)
  })
}


track_points_server <- function(id, gpx_file) {
  moduleServer(id, function(input, output, session) {
    
    activity_track_points <-  reactive({
      req(gpx_file())
      
      activity_track_points <- st_read(gpx_file(), 
                                       layer = "track_points", 
                                       quiet = TRUE) |>
        mutate(ele_diff = ele - lag(ele),
               time_diff = difftime(time, lag(time)),
               lead = geometry[row_number() + 1],
               dist = st_distance(geometry, lead, by_element = TRUE)
        )
      
      
    })
    #ouput$activity_tracks_points <- activity_track_points
    return(activity_track_points)
    
  })
}

