#
#   Photo Annotation for sidewalk quality
#
#           Alan Jackson September 2019
#
#       Read photos taken with phone from RawPhotos directory,
#       Scale to 1/8 original resolution
#       Crop to interesting area, rotate if needed
#       Add labels for sidewalk quality, distance that label applies,
#       and direction camera was pointed.
#       Output cropped and shrunk photo to Photos directory
#       Update tibble with annotation
#       Show active photo location on map
#	color code checkboxes
#	https://stackoverflow.com/questions/41813960/how-to-make-the-checkboxgroupinput-color-coded-in-shiny
#
#       After done, move photos from RawPhotos to Archive

library(shiny)
library(tidyverse)
library(purrr)
library(exifr)
library(magick)
library(leaflet)

###################################
#   get and set up the basic data
###################################

###  read in old tibble if it exists

rawpath <- "~/Dropbox/Rprojects/Sidewalk_Quality/RawPhotos" # New, unprocessed 
savepath <- "~/Dropbox/Rprojects/Sidewalk_Quality/Photos" # Processed Photos

#   Read files in Raw directory
image_list <- list.files(path=rawpath , pattern = "*.jpg", full.names = TRUE)
dat <- read_exif(image_list) # Read exif headers into data frame

dat2 <- select(dat,
               SourceFile, DateTimeOriginal,
               GPSLongitude, GPSLatitude) %>% 
    arrange(DateTimeOriginal) %>% 
    mutate(SourceFile=str_extract(SourceFile, "[A-Z0-9_]*.jpg$")) %>% # just filename
    mutate(Quality="Not Set", Length=0, Direction="Not Set") %>% 
    mutate(EndLon=GPSLongitude, EndLat=GPSLatitude)

image_list <- paste0(rawpath,"/",dat2$SourceFile) # make sure image_list sort same as dat2

#       Is there an old tibble?

oldfile <- paste0(savepath, "/Photos.rds")
if (file.exists(oldfile)){
    OldDF <- readRDS(oldfile)
    } else 
{
    OldDF <- tibble(SourceFile=character(),
                    DateTimeOriginal=character(),
                    GPSLongitude=numeric(),
                    GPSLatitude=numeric(),
                    Quality=character(),
                    Length=numeric(),
                    Direction=character(),
                    EndLon=numeric(),
                    EndLat=numeric())
}

#   Join new data to old file

OldDF <- bind_rows(OldDF, dat2)

image_number <- 0 # initialize
#   saved = True if photo has been saved
saved <- logical(length=nrow(dat2))

##################################################
# Define UI for displaying and annotating photos
##################################################

# Demo of clicking, hovering, brushing with imageOutput
# Note that coordinates are in pixels
shinyApp(
    ui = basicPage(
        fluidRow(
            column(width = 8,
                   imageOutput("image", height=378,
                               click = "image_click",
                               hover = hoverOpts(
                                   id = "image_hover",
                                   delay = 500,
                                   delayType = "throttle"
                               ),
                               brush = brushOpts(id = "brush")
                   ),
                    HTML("<hr>"),
                    textOutput("SourceFile"),
                    #textOutput("LatLong"),

                    HTML("<hr>"),
                      actionButton("Prev", "Prev"),
                      actionButton("Next", "Next"),
                      actionButton("Rotate", "Rotate"),
                      actionButton("Save", "Save"),
                   HTML("<hr>"),
                   
                   #        Add map
                   
                   leafletOutput("LocalMap")
            ),
            
            #       Add left column with controls for labeling
            column(width = 2,
                   #    Quality, Length, and Direction
                   radioButtons("quality", label = "Sidewalk quality",
                                choices = list("None selected" = "",
                                               "Good"="Good", "Bushes"="Bushes", 
                                               "Gap"="Gap",  "Offset"="Offset", 
                                               "Shattered"="Shattered", 
                                               "Obstructed"="Obstructed", 
                                               "Debris/Mud"="Debris/Mud", 
                                               "Gravel"="Gravel", 
                                               "No Curb Cut"="No Curb Cut", 
                                               "Missing"="Missing")
                                ),
                   numericInput("length", "Estimated length in feet",
                               min = 0, max = 500, step=5,
                               value = 50, width = '100px'),
                   radioButtons("direction", label = "Viewing direction",
                                choices = list("N" = "N", "E" = "E",
                                               "S" = "S", "W" = "W",
                                               "NW"="NW", "NE"="NE",
                                               "SE"="SE", "SW"="SW"), 
                                selected = "N")
            )
        )
    ),
    
#####################################################
# Define Server for displaying and annotating photos
#####################################################


    server = function(input, output, session) {
        
        #   Size of images from phone is 4032x3024
        #   Function to prep image
        #   want height to be 378
        imagefile <- reactiveValues(tmpfile = "tmp")
        image_prep <- function(image_file){
            image <- image_read(image_file)
            tmp <- renderImage({
                imagefile$tmpfile <- image %>% 
                    image_scale("x378") %>%
                    image_write(tempfile(fileext='.jpg'), format = 'jpg')
                # Return a list containing the filename
                list(src = imagefile$tmpfile,  contentType = "image/jpeg")
              }, deleteFile = FALSE)
            return(tmp)
        }
        
        #   Draw map - show marker at lat/long, and draw line of proper length
        #   and direction as chosen
        #   Use approximation of 1 degree = 340,000 feet
        draw_map <- function(i, len, dir){
            lat <- dat2$GPSLatitude[i]
            lon <- dat2$GPSLongitude[i]
            latlon <- len/340000. # distance in lat long space
            newcoord <- case_when(
                dir == "N" ~ list(lat+latlon, lon),
                dir == "S" ~ list(lat-latlon, lon),
                dir == "E" ~ list(lat, lon+latlon),
                dir == "W" ~ list(lat, lon-latlon),
                dir == "NE" ~ list(lat+latlon*0.707, lon+latlon*0.707),
                dir == "NW" ~ list(lat+latlon*0.707, lon-latlon*0.707),
                dir == "SE" ~ list(lat-latlon*0.707, lon+latlon*0.707),
                dir == "SW" ~ list(lat-latlon*0.707, lon-latlon*0.707)
            )
            dat2$EndLon[i] <<- newcoord[[2]]
            dat2$EndLat[i] <<- newcoord[[1]]
            LonLine <- c(lon, newcoord[[2]])
            LatLine <- c(lat, newcoord[[1]])
            output$LocalMap <- renderLeaflet({
                leaflet() %>%
                       setView(lng = lon , lat = lat, zoom = 20) %>% 
                       addTiles() %>%
                       addCircleMarkers(dat2$GPSLongitude, dat2$GPSLatitude,
                                        radius=3, opacity=1, color="#000000") %>% 
                       addCircleMarkers(dat2[saved,]$GPSLongitude, dat2[saved,]$GPSLatitude,
                                        radius=3, opacity=1, color="#008000") %>% 
                       addCircleMarkers(lon, lat,
                                        radius=3, opacity=1, color="#ff0000") %>% 
                       addPolylines(lng = LonLine, lat = LatLine)
            })
        }
        
        #       Reset controls - for when new image appears
        
        resetControls <- function(){
            session$resetBrush("brush")
            brush <<- NULL
            updateRadioButtons(session, "quality", selected = "")
            updateNumericInput(session, "length", value = 50)
        }
        
        ##############
        ### length ###
        ##############
        observeEvent(input$length, {
            draw_map(counter$image_number, input$length, input$direction)
         }, ignoreNULL=FALSE)
        
        #################
        ### direction ###
        #################
        observeEvent(input$direction, {
            draw_map(counter$image_number, input$length, input$direction)
         }, ignoreNULL=FALSE)
        
        
        
        # Defining & initializing the reactiveValues object
        counter <- reactiveValues(image_number = 0) 
        
        ###################
        ### Next button ###
        ###################
        observeEvent(input$Next, {
            resetControls()
            if (counter$image_number == length(image_list)){
              showNotification("Last image in set")
            }
            counter$image_number <- min(length(image_list), counter$image_number+1)
            output$image <- image_prep(image_list[counter$image_number])
            draw_map(counter$image_number, input$length, input$direction)
            output$SourceFile <<- renderText(dat2[counter$image_number,]$SourceFile)
            output$LatLong <<- renderText(paste(dat2[counter$image_number,]$GPSLatitude,",",
                                          dat2[counter$image_number,]$GPSLongitude))
         }, ignoreNULL=FALSE)
        
        
        ###################
        ### Prev button ###
        ###################
        observeEvent(input$Prev, {
            resetControls()
            if (counter$image_number == 1){
              showNotification("First image in set")
            }
            counter$image_number <- max(1, counter$image_number-1)
            output$image <- image_prep(image_list[counter$image_number])
            draw_map(counter$image_number, input$length, input$direction)
            output$SourceFile <<- renderText(dat2[counter$image_number,]$SourceFile)
            output$LatLong <<- renderText(paste(dat2[counter$image_number,]$GPSLatitude,",",
                                          dat2[counter$image_number,]$GPSLongitude))
         }, ignoreNULL=TRUE)
        
        
        #####################
        ### Rotate button ###
        #####################
        observeEvent(input$Rotate, {
            session$resetBrush("brush")
            brush <<- NULL
            image <- image_read(imagefile$tmpfile)
            output$image <- renderImage({
                imagefile$tmpfile <- image %>% 
                    image_rotate(90) %>% 
                    image_resize("x378") %>%
                    image_write(tempfile(fileext='.jpg'), format = 'jpg')
                # Return a list containing the filename
                list(src = imagefile$tmpfile,  contentType = "image/jpeg")
              }, deleteFile = FALSE)
         }, ignoreNULL=TRUE)
        
        ###################
        ### Save button ###
        ###################
        observeEvent(input$Save, {
            
            #   Update tibble and save
            
            if (input$quality==""){ # no quality label chosen
                showModal(modalDialog(
                    title = "Error",
                    "Choose a Quality label."
                ))
            } else if(is.null(input$brush)){ # you must crop
                showModal(modalDialog(
                    title = "Error",
                    "You must crop image."
                ))
              }
            else {
                mask <- OldDF$SourceFile==dat2$SourceFile[counter$image_number]
                OldDF$Quality[mask] <<- input$quality
                OldDF$Length[mask] <<- input$length
                OldDF$Direction[mask] <<- input$direction
                OldDF$EndLon[mask] <<- dat2$EndLon[counter$image_number]
                OldDF$EndLat[mask] <<- dat2$EndLat[counter$image_number]
                
                print(OldDF)
                
                saveRDS(OldDF, oldfile)
                
                #   Crop image and write out
                
                image <- image_read(imagefile$tmpfile)  # current view
                
                if(is.null(input$brush)){ # do not crop
                    image %>% 
                        image_strip() %>%  # remove exif headers
                        image_write( 
                                path=paste0(savepath,"/",dat2$SourceFile[counter$image_number]), 
                                format = 'jpg')
                    }
                else { # Crop image
                    geom <- 
                        geometry_area(width=input$brush$coords_img$xmax-input$brush$coords_img$xmin,
                                  height=input$brush$coords_img$ymax-input$brush$coords_img$ymin,
                                  x_off=input$brush$coords_img$xmin,
                                  y_off=input$brush$coords_img$ymin
                                  )
                    image %>% 
                        image_strip() %>%  # remove exif headers
                        image_crop(geometry=geom) %>% # crop to selected area
                        image_write( 
                                path=paste0(savepath,"/",dat2$SourceFile[counter$image_number]), 
                                format = 'jpg')
                }
                #       Reset to defaults
                resetControls()
                #       Go to Next
                saved[counter$image_number] <<- TRUE # flag image as saved
                counter$image_number <<- min(length(image_list), counter$image_number+1)
                output$image <- image_prep(image_list[counter$image_number])
                draw_map(counter$image_number, input$length, input$direction)
                if (counter$image_number == length(image_list)){
                  showNotification("Last image in set")
                }
            }   
         }, ignoreNULL=TRUE)
    }
)
