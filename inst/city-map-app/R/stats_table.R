
stats_table_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(12,tableOutput(ns("stats_table"))))
  )
}

stats_table_server <- function(id, activity_tracks, activity_track_points) {
  moduleServer(id, function(input, output, session) {
    
    output$stats_table <- renderTable({
      
      req(activity_tracks())
      req(activity_track_points())
      
      # distance calculations ---------------------------------------------
      distance <-  activity_tracks() |>
        st_length()  |> 
        as.numeric() * 0.0006213712
      
      distance <- distance  |> round(digits = 2)
      
      # elevation calculations -----------------------------------
      elevation_gain <- activity_track_points() |>
        dplyr::filter(ele_diff > 0)|>
        dplyr::pull(ele_diff) |>
        sum(na.rm = TRUE) * 3.280839895
      
      elevation_gain <- elevation_gain |> round()
      
      stats_table <- data.frame(Distance = paste(distance, "miles"),
                                `Elevation Gain` = paste(elevation_gain, "feet"))
      
      stats_table
      
    })
    
  })
}

