#
#   Photo Annotation for sidewalk quality
#
#           Alan Jackson September 2019
#
#       Read photos taken with phone from RawPhotos directory,
#       Scale to 1/8 original resolution
#       Crop to intersting area, rotate if needed
#       Add labels for sidewalk quality, distance label applies,
#       and direction camera was pointed.
#       Output cropped and shrunk photo to Photos directory
#       Update tibble with annotation
#

library(shiny)
library(tidyverse)
library(purrr)
library(exifr)
library(magick)

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
    mutate(SourceFile=str_extract(SourceFile, "[A-Z0-9_]*.jpg$")) %>% # just filename
    mutate(Quality="Not Set", Length=0, Direction="Not Set")

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
                    Direction=character())
}

#   Join new data to old file

OldDF <- bind_rows(OldDF, dat2)
    
image_number <- 0 # initialize

##################################################
# Define UI for displaying and annotaing photos
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

                      actionButton("Prev", "Prev"),
                      actionButton("Next", "Next"),
                      actionButton("Rotate", "Rotate"),
                      actionButton("Save", "Save")
            ),
            column(width = 2,
                   #    Quality, Length, and Direction
                   radioButtons("quality", label = "Sidewalk quality",
                                choices = list("Good"="Good", "Bushes"="Bushes", 
                                               "Gap"="Gap",  "Offset"="Offset", 
                                               "Shattered"="Shattered", 
                                               "Gravel"="Gravel", 
                                               "No Curb Cut"="No Curb Cut", 
                                               "Missing"="Missing"),
                                selected = "Good"),
                   numericInput("length", "Estimated length in feet",
                               min = 0, max = 250, step=5,
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
    server = function(input, output, session) {
        
        #   Size of images from phone is 4032x3024
        #   Function to prep image
        imagefile <- reactiveValues(tmpfile = "tmp")
        image_prep <- function(image_file, rotate=0){
            image <- image_read(image_file)
            tmp <- renderImage({
                imagefile$tmpfile <- image %>% 
                    image_scale(504) %>%
                    image_rotate(rotate) %>% 
                    image_write(tempfile(fileext='.jpg'), format = 'jpg')
                # Return a list containing the filename
                list(src = imagefile$tmpfile,  contentType = "image/jpeg")
              }, deleteFile = FALSE)
            return(tmp)
        }
        
        ###################
        ### Next button ###
        ###################
        # Defining & initializing the reactiveValues object
        counter <- reactiveValues(image_number = 0) 
        observeEvent(input$Next, {
            session$resetBrush("brush")
            brush <<- NULL
            counter$image_number <- min(length(image_list), counter$image_number+1)
            output$image <- image_prep(image_list[counter$image_number])
         }, ignoreNULL=FALSE)
        
        
        ###################
        ### Prev button ###
        ###################
        observeEvent(input$Prev, {
            session$resetBrush("brush")
            brush <<- NULL
            counter$image_number <- max(1, counter$image_number-1)
            output$image <- image_prep(image_list[counter$image_number])
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
                    image_write(tempfile(fileext='.jpg'), format = 'jpg')
                # Return a list containing the filename
                list(src = imagefile$tmpfile,  contentType = "image/jpeg")
              }, deleteFile = FALSE)
            #output$image <- image_prep(image_list[counter$image_number], 90)
         }, ignoreNULL=TRUE)
        
        ###################
        ### Save button ###
        ###################
        observeEvent(input$Save, {
            print(paste("Save ", imagefile$tmpfile, "to", dat2$SourceFile[counter$image_number]))
            
            #   Update tibble and save
            print(paste("Quality = ", input$quality ))
            print(paste("Length = ", input$length))
            print(paste("Direction = ", input$direction))
            
            mask <- OldDF$SourceFile==dat2$SourceFile[counter$image_number]
            OldDF$Quality[mask] <- input$quality
            OldDF$Length[mask] <- input$length
            OldDF$Direction[mask] <- input$direction
            
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
            session$resetBrush("brush")
            brush <<- NULL
         }, ignoreNULL=TRUE)
    }
)
