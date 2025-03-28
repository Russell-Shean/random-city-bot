# see https://github.com/EPdeJ/cyclingplots#readme for more information
# currently Jawg.Lagoon as base layer for maps in leaflet

# load packages and filepaths ---------------------------------------------
if(!require("pacman")){install.packages("pacman")}
pacman::p_load(tidyverse,sf,zoo,Cairo, scales)

# function elevation profile -------------------------------------------------------
# set start parameters for function
elevationprofile <- function(filepath,
                             seq=10,
                             roll=10,
                             rollparameter="max",
                             colorscalestr=c("#9198A7","#C9E3B9", "#F9D49D", "#F7B175", "#F47D85", "#990000"),
                             linecolor="#23b09d",
                             maxlinecol="red",
                             transparency=1,
                             plotsave=F,
                             plotsavedir=NULL,
                             plotname="",
                             ggsave_width=24,
                             ggsave_height=10,
                             ggsave_dpi=300,
                             ggsave_units="cm",
                             ggsave_background="transparent"
){
  
  
  # import gpx into sf object to add geometry
  gpx <- st_read(filepath,layer="track_points") #turn into sf object
  gpx <- gpx %>% select(track_seg_point_id, ele,geometry) #select the usefull rows
  
  # calculate the difference in distance and elevation between data points
  gpx<- gpx %>%
    mutate(
      elevation_diff=c(0,lag((lead(ele)-ele))[-1]),
      distance_diff = c(0,lag(st_distance(geometry,lead(geometry),by_element = TRUE))[-1])
    )                                     
  
  # calculate the cumsum of the distance and elevation per point
  gpx<- gpx %>% 
    mutate(
      distance_total = cumsum(distance_diff),
      elevation_total = cumsum(ifelse(elevation_diff > 0, elevation_diff, 0)),
      gradient=(elevation_diff/distance_diff)*100
    ) # elevation can be negative so only cum sum when positive 
  
  # WORK IN PROGRESS (if seq is defined it will reduce the number of datapoints from the gpx)
  if(seq){
    max_ele_index <- which.max(gpx$ele)
    n_rows <- nrow(gpx)
    intermediate_rows <- seq(from = 1 + seq, to = n_rows - 1, by = seq)
    rows_to_select <- unique(sort(c(1, max_ele_index, n_rows, intermediate_rows)))
    gpx <- gpx %>% slice(rows_to_select)
  }
  
  # calculate rolling averages
  gpx<- gpx %>% 
    mutate(
      distance_roll_mean = rollmean(distance_total, roll, fill=0),
      distance_roll_max = rollmax(distance_total, roll, fill=0),
      distance_diff_roll_mean=rollmean(distance_diff,roll, fill=0),
      distance_diff_roll_max=rollmax(distance_diff,roll, fill=0),
      elevation_roll_mean = rollmean(elevation_total,roll, fill=0),
      elevation_roll_max= rollmax(elevation_total,roll, fill=0),
      elevation_diff_roll_mean= rollmean(elevation_diff,roll, fill=0),
      elevation_diff_roll_max= rollmax(elevation_diff,roll, fill=0)
    ) #calculate rolling averages based on window named roll as set in function 
  
  # calculate rolling gradient
  gpx<- gpx %>% 
    mutate(
      gradient_roll_mean=(elevation_diff_roll_mean/distance_diff_roll_mean)*100,
      gradient_roll_max=(elevation_diff_roll_max/distance_diff_roll_max)*100
    ) %>% 
    mutate(
      gradient_roll_mean=replace_na(gradient_roll_mean, 0),
      gradient_roll_max=replace_na(gradient_roll_max, 0)
    )
  
  # turn rolling gradients into factors/steps/bins based on Garmin percentages
  gpx<- gpx %>% 
    mutate(
      gradient_roll_mean_binned=cut(gpx$gradient_roll_mean,
                                    breaks=c(-Inf,0.05, 3, 6, 9, 12, Inf),
                                    label=c("downhill or flat","<3%","3-6%", "6-9%", "9-12%",">12%"),
                                    include.lowest=T,
                                    ordered_result=T),
      gradient_roll_max_binned=cut(gpx$gradient_roll_max,
                                   breaks=c(-Inf,0.05, 3, 6, 9, 12, Inf),
                                   label=c("downhill or flat","<3%","3-6%", "6-9%", "9-12%",">12%"),
                                   include.lowest=T,
                                   ordered_result=T)
    ) 
  
  gpx <- gpx %>% st_drop_geometry()
  mem <- gpx
  
  # find maximum elevation (highest point) & max distance
  maxele <- max(gpx$ele)
  maxeledist <- gpx %>% filter(ele==maxele) %>% select(distance_total) %>% pull/1000
  maxdist <- max(gpx$distance_total)
  
  # calculate the position for three lines in between the max line and the x axis (y=0)
  rib0ele <- min(gpx$ele)
  rib0dist <- gpx %>%  mutate(difference = abs(ele - rib0ele)) %>%  
    slice_min(difference) %>% select(distance_total) %>% pull/1000
  rib1ele <- (maxele-rib0ele)/4
  rib1dist <- gpx %>%  mutate(difference = abs(ele - rib1ele)) %>%  
    slice_min(difference) %>% select(distance_total) %>% pull/1000
  rib2ele <- rib1ele*2
  rib2dist <- gpx %>%  mutate(difference = abs(ele - rib2ele)) %>%  
    slice_min(difference) %>% select(distance_total) %>% pull/1000
  rib3ele <- rib1ele*3
  rib3dist <- gpx %>%  mutate(difference = abs(ele - rib3ele)) %>%  
    slice_min(difference) %>% select(distance_total) %>% pull/1000
  
  
  # make the plot
  plot <- ggplot(data=gpx)+
    
    #add max height line
    annotate("segment",
             x=maxeledist,
             xend=maxdist/1000,
             y=maxele,
             yend=maxele,
             colour=maxlinecol,
             linetype = 'dashed',
             linewidth= .5)+
    
    #add 3 even lines inbetween max line and lowest elevation
    annotate("segment",
             x=rib1dist,
             xend=maxdist/1000,
             y=rib1ele,
             yend=rib1ele,
             colour="lightgrey",
             linewidth= .5)+
    annotate("segment",
             x=rib2dist,
             xend=maxdist/1000,
             y=rib2ele,
             yend=rib2ele,
             colour="lightgrey",
             linewidth= .5)+
    annotate("segment",
             x=rib3dist,
             xend=maxdist/1000,
             y=rib3ele,
             yend=rib3ele,
             colour="lightgrey",
             linewidth= .5)+ 
    annotate("segment",
             x=rib0dist,
             xend=maxdist/1000,
             y=rib0ele,
             yend=rib0ele,
             colour="lightgrey",
             linewidth= .5)+
    
    #plot "downhill or flat" area under the curve, which serves as the background
    geom_ribbon(aes(x=distance_total/1000,ymax=ele,ymin=0),
                fill=colorscalestr[1],
                alpha=transparency
    )+
    
    #plot <3% area under the curve
    geom_ribbon(aes(x=distance_total/1000, 
                    ymax={ 
                      if (rollparameter == "max") { # Use gradient_roll_max_binned to evaluate gradient group
                        ifelse(gradient_roll_max_binned == "<3%", ele, -Inf)
                      } else if (rollparameter == "mean") { # Use gradient_roll_mean_binned to evaluate gradient group
                        ifelse(gradient_roll_mean_binned == "<3%", ele, -Inf)
                      } else { NA_real_ 
                        warning("Invalid rollparameter specified. Select either \"max\" or \"mean\"") #Give a warning to the user
                      }
                    },
                    ymin=0),
                fill=colorscalestr[2],
                alpha=transparency
    )+
    
    #plot 3-6% area under the curve
    geom_ribbon(aes(x=distance_total/1000, 
                    ymax={ 
                      if (rollparameter == "max") { # Use gradient_roll_max_binned to evaluate gradient group
                        ifelse(gradient_roll_max_binned == "3-6%", ele, -Inf)
                      } else if (rollparameter == "mean") { # Use gradient_roll_mean_binned to evaluate gradient group
                        ifelse(gradient_roll_mean_binned == "3-6%", ele, -Inf)
                      } 
                    },
                    ymin=0),
                fill=colorscalestr[3],
                alpha=transparency
    )+
    
    #plot 6-9% area under the curve
    geom_ribbon(aes(x=distance_total/1000, 
                    ymax={ 
                      if (rollparameter == "max") { # Use gradient_roll_max_binned to evaluate gradient group
                        ifelse(gradient_roll_max_binned == "6-9%", ele, -Inf)
                      } else if (rollparameter == "mean") { # Use gradient_roll_mean_binned to evaluate gradient group
                        ifelse(gradient_roll_mean_binned == "6-9%", ele, -Inf)
                      } 
                    },
                    ymin=0),
                fill=colorscalestr[4],
                alpha=transparency
    )+
    
    #plot 9-12% area under the curve
    geom_ribbon(aes(x=distance_total/1000, 
                    ymax={ 
                      if (rollparameter == "max") { # Use gradient_roll_max_binned to evaluate gradient group
                        ifelse(gradient_roll_max_binned == "9-12%", ele, -Inf)
                      } else if (rollparameter == "mean") { # Use gradient_roll_mean_binned to evaluate gradient group
                        ifelse(gradient_roll_mean_binned == "9-12%", ele, -Inf)
                      } 
                    },
                    ymin=0),
                fill=colorscalestr[5],
                alpha=transparency
    )+
    
    #plot "good luck" >12% area under the curve
    geom_ribbon(aes(x=distance_total/1000, 
                    ymax={ 
                      if (rollparameter == "max") { # Use gradient_roll_max_binned to evaluate gradient group
                        ifelse(gradient_roll_max_binned == ">12%", ele, -Inf)
                      } else if (rollparameter == "mean") { # Use gradient_roll_mean_binned to evaluate gradient group
                        ifelse(gradient_roll_mean_binned == ">12%", ele, -Inf)
                      } 
                    },
                    ymin=0),
                fill=colorscalestr[6],
                alpha=transparency
    )+
    
    #' set y-axis limits for plot as they become wildly negative automatically due to 
    #' the set -1000000 for the shades for the gradients. This was needed because a 
    #' point with an elevation connecting to 0 would create a sloped line instead of a 
    #' vertical line. This still needs to be solved but the -1000000 patch is visually working
    scale_y_continuous(limits = c(0,maxele+20),  position = "right",
                       breaks = c(rib0ele, rib1ele,rib2ele,rib3ele,maxele),
                       labels = c(paste0(round(rib0ele,0), " m"), 
                                  paste0(round(rib1ele,0), " m"), 
                                  paste0(round(rib2ele,0), " m"),
                                  paste0(round(rib3ele,0), " m"),
                                  paste0(round(maxele,0), " m \n(Highest Point)")),
                       expand = c(0,1)
    )+ 
    scale_x_continuous(
      breaks = c(seq(0, maxdist/1000, by = 10),maxdist/1000), 
      labels = paste0(c(seq(0, maxdist/1000, by = 10),round(maxdist/1000,0)), " Km"), 
      minor_breaks = NULL,
      expand = c(0,1.5)
    ) +
    
    
    
    #add height profile line
    geom_line(aes(x=distance_total/1000,y=ele), colour= linecolor, linewidth=1, lineend="round")+
    
    #add start and end point
    geom_point(data=. %>% slice(c(1, n())),
               aes(
                 x=distance_total/1000,
                 y=ele),
               size = 4, # Adjust size as needed
               shape = 21, # Shape 21 is a circle with a fill and stroke
               fill = "transparent", # Fill color
               color = linecolor, # Stroke color
               stroke = 1, # Stroke thickness
               alpha = 1)+ # Transparency
    
    #change labels x and y axis
    xlab("Distance (km)")+
    ylab("Elevation (m)")+
    
    #further add changes to layout
    theme(panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "transparent",color = NA), 
          plot.background = element_rect(fill = "transparent", color = NA),
          axis.text.x = element_text(vjust = .5, margin = margin(t = 3), size = 12),
          axis.text.y = element_text(size=12),
          axis.ticks.length.x = unit(-0.15, "cm"),
          axis.ticks.x = element_line(color = "lightgrey", linewidth=1.5),
          axis.ticks.y = element_blank(),
          axis.title = element_blank()
    )
  
  
  
  if(plotsave){suppressMessages(ggsave(plot= plot,
                                       path = plotsavedir,
                                       filename = paste0(ifelse(plotname!="", paste(plotname), sub("\\.gpx$", "", basename(filepath))),".png"),
                                       width = as.numeric(ggsave_width),
                                       height = as.numeric(ggsave_height),
                                       units =ggsave_units,
                                       dpi = as.numeric(ggsave_dpi),
                                       type = "cairo-png",
                                       bg = ggsave_background ))
  }
  
  plot
  
}