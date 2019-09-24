#
#   View sidewalk quality with photos
#
#           Alan Jackson September 2019
#
#       Display map of area
#       Mark locations of photos
#       Draw colored lines representing sidewalk quality 
#       Photos accessed by clicking location marks
#

library(shiny)
library(tidyverse)
library(leaflet)
library(leafpop) # for popup on map

###################################
#   get and set up the basic data
###################################

#   Directory where data is stored

DataLocation <- "https://www.ajackson.org/sidewalks/data/"

#   Tibble database

OldDF <- readRDS(gzcon(url(paste0(DataLocation, "/Photos.rds"))))

#  lng = -95.404606 , lat = 29.797131, zoom = 16
MapCenter <- c(-95.404606 , 29.797131)

#   Colors for various qualities
colorDF <- data.frame(
     Quality = c("Good", "Bushes",  "Gap", "Offset", "Shattered", "Obstructed",
                "Debris/Mud", "Gravel", "No Curb Cut", "Missing"),
     Colors = c("green", "cyan2", "deepskyblue2", "dodgerblue2", "blue2",
            "yellow", "goldenrod2", "darkorange2", "magenta2", "red2"),
     Codes=c("#008000", "#00eeee", "#00b2ee", "#1c86ee", "#0000ee",
             "#ffff00", "#eeb422", "#ee7600", "#ee00ee", "#ee0000"),
     stringsAsFactors = FALSE
)

#   Build a full path in OldDF for each photo
DataLocation <- "https://www.ajackson.org/sidewalks/data/"
OldDF$SourceFile <- paste0(DataLocation, OldDF$SourceFile)

#   Add color to OldDF for each record
OldDF <- left_join(OldDF, colorDF, by="Quality")

init_zoom <- 15
init_weight <- 8

##################################################
# Define UI for displaying and annotating photos
##################################################

shinyApp(
    ui = basicPage(
      fluidRow(
        column(width = 8,
               leafletOutput("LocalMap")
        ),
        
        #       Add right column with controls for display
        column(width = 2,
               #    Quality
               checkboxGroupInput("quality", label = "Sidewalk qualities to display",
                            choices = list("Good"="Good", "Bushes"="Bushes", 
                                           "Gap"="Gap",  "Offset"="Offset", 
                                           "Shattered"="Shattered", 
                                           "Obstructed"="Obstructed", 
                                           "Debris/Mud"="Debris/Mud", 
                                           "Gravel"="Gravel", 
                                           "No Curb Cut"="No Curb Cut", 
                                           "Missing"="Missing"),
                            selected = "Good"),
               #    pick what to show
               checkboxGroupInput("classes", label="Turn elements on/off",
                                  list("Quality"="qual", "Photo points"="pnts"),
                                  selected=c("qual","pnts"))
          )
        ) # fluidRow
    ), # basicPage

#####################################################
# Define Server for displaying map and photos
#####################################################

    server <- function(input, output, session) {

            #       This bit won't change
            output$LocalMap <- renderLeaflet({
                #   Basemap
                leaflet(OldDF) %>% 
                  setView(lng = -95.404606 , lat = 29.797131, zoom = init_zoom ) %>%   
                    addTiles()
            }) 
            
            observe({
                #   Polylines (done in a loop to keep them from connecting)
                #print(paste("classes:",input$classes))
                #print(paste("null?:",is.null(input$classes)))
                #print(paste("quality:",input$quality))
                #print(paste("zoom:",input$LocalMap_zoom))
                #   scale width of lines in concert with zoom scale
                wgt <- max(2,floor(2*(input$LocalMap_zoom-init_zoom)+0.5) + init_weight)
                #print(paste("weight:",wgt))
                if (!is.null(input$classes) & 
                    !is.null(input$quality) & 
                    "qual" %in% input$classes[1]){
                    leafletProxy("LocalMap") %>% 
                    clearShapes()  
                    for (i in 1:nrow(OldDF)){
                        if (OldDF[i,]$Quality %in% input$quality) {
                            leafletProxy("LocalMap") %>% 
                              addPolylines(
                                 lat=c(OldDF[i,]$GPSLatitude, OldDF[i,]$EndLat),
                                 lng=c(OldDF[i,]$GPSLongitude, OldDF[i,]$EndLon),
                                 color=OldDF[i,]$Codes,
                                 weight=wgt,
                                 opacity=1
                              )
                        }
                    }
                } 
                else {
                    leafletProxy("LocalMap") %>% 
                    clearShapes()  
                }
            })
                # Markers (put on top so they show up and can be clicked)
             #   m %>%   addCircleMarkers(~ GPSLongitude, ~ GPSLatitude, 
             #                      radius=3, opacity=1, color="#999",
             #                popup = popupImage(OldDF$SourceFile, src="remote",
             #                                   height=150,
             #                                   width=150),
             #                  ) %>% 
             #                 addTiles()
             #   
             #               })
    }
)
