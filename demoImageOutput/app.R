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

#   workingset is a snapshot of the RawPhotos directory, and is where the work occurs
#   OldDF is the master database, and gets updated and saved after each execution of "save"
#   Inputs live in RawPhotos
#   Outputs live in Photos
#   OriginalLat holds location from exif header. NewLat may get edited (corrected)

library(shiny)
library(tidyverse)
library(purrr)
library(exifr)
library(magick)
library(leaflet)
library(leaflet.extras)

###################################
#   get and set up the basic data
###################################

###  read in old tibble if it exists

rawpath <- "~/Dropbox/Rprojects/Sidewalk_Quality/RawPhotos" # New, unprocessed 
savepath <- "~/Dropbox/Rprojects/Sidewalk_Quality/Photos" # Processed Photos

#   Read files in Raw directory
image_list <- list.files(path=rawpath , pattern = "*.jpg", full.names = TRUE)
dat <- read_exif(image_list) # Read exif headers into data frame

workingset <- select(dat,
               SourceFile, DateTimeOriginal,
               GPSLongitude, GPSLatitude) %>% 
    arrange(DateTimeOriginal) %>% 
    mutate(SourceFile=str_extract(SourceFile, "[A-Z0-9_]*.jpg$")) %>% # just filename
    mutate(Quality="Not Set", Length=0, Direction="Not Set") %>% 
    mutate(EndLon=GPSLongitude, EndLat=GPSLatitude) %>% 
    mutate(NewLat=GPSLatitude, NewLong=GPSLongitude)

image_list <- paste0(rawpath,"/",workingset$SourceFile) # make sure image_list sort same as workingset

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
                    EndLat=numeric(),
                    NewLat=numeric(),
                    NewLong=numeric())
}

#   Join new data to old file - but only new records

OldDF <- bind_rows(OldDF, anti_join(workingset, OldDF, by="SourceFile"))

#   Add an ID field to workingset

workingset <- cbind(id=rownames(workingset), workingset, stringsAsFactors=FALSE)

image_number <- 0 # initialize
#   saved = True if photo has been saved
saved <- logical(length=nrow(workingset))

#   Flag records from workingset that are already done

saved[workingset$SourceFile %in% anti_join(workingset, OldDF)$SourceFile] <- TRUE


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
            column(width = 4,
                   #    Quality, Length, and Direction
              tabsetPanel(id="tabs",
                tabPanel("Annotate", fluid=TRUE,value="annotate",
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
                               min = 0, max = 1000, step=5,
                               value = 50, width = '80%'),
                   radioButtons("direction", label = "Viewing direction",
                                choices = list("N" = "N", "E" = "E",
                                               "S" = "S", "W" = "W",
                                               "NW"="NW", "NE"="NE",
                                               "SE"="SE", "SW"="SW"), 
                                selected = "N")
                ), # end tabPanel Annotate
                tabPanel("Align", fluid=TRUE, value="Aligntab",
                         HTML("<hr>"),
                         tags$em("Pick the start and end points for alignment"),
                         tags$em("and then draw the line to move them to."),
                         HTML("<hr>"),
                         textOutput("EndPoints"),
                         HTML("<hr>"),
                         actionButton("Align", "Align"),
                         actionButton("ClearAlign", "Clear"),
                         HTML("<hr>"),
                         actionButton("RevertCurrent", "Revert Current"),
                         actionButton("RevertAll", "Revert All")
                ), # end tabPanel Align
                tabPanel("Move", fluid=TRUE, value="Movetab",
                         HTML("<hr>"),
                         tags$em("Pick the point to be moved"),
                         tags$em("and then pick the destination."),
                         HTML("<hr>"),
                         actionButton("Move", "Move"),
                         actionButton("ClearMove", "Clear"),
                         HTML("<hr>"),
                         actionButton("RevertMove", "Revert Move")
                )  # end tabPanel Move
            ) 
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
        draw_points <- function(i, len, dir){
            lat <- workingset$NewLat[i]
            lon <- workingset$NewLong[i]
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
            workingset$EndLon[i] <<- newcoord[[2]]
            workingset$EndLat[i] <<- newcoord[[1]]
            LonLine <- c(lon, newcoord[[2]])
            LatLine <- c(lat, newcoord[[1]])
            nextfive <- logical(length=nrow(workingset))
            nextfive[(counter$image_number+1):(counter$image_number+6)] <- TRUE
            leafletProxy("LocalMap") %>% 
                       clearShapes() %>% 
                              # aqua means one of next five to be done
                       addCircleMarkers(workingset[nextfive,]$NewLong, 
                                        workingset[nextfive,]$NewLat,
                                        radius=3, opacity=1, color="#00FFFF",
                                        layerId = workingset[nextfive,]$id) %>% 
                              # green means already done
                       addCircleMarkers(workingset[saved,]$NewLong, 
                                        workingset[saved,]$NewLat,
                                        radius=3, opacity=1, color="#008000",
                                        layerId = workingset[saved,]$id) %>% 
                              # red means current point
                       addCircleMarkers(lon, lat,
                                        radius=3, opacity=1, color="#ff0000") %>% 
                       addPolylines(lng = LonLine, lat = LatLine)
        }  #  end of draw points
        
        #   Draw a single point to highlight
        draw_highlight <- function(ids){
            temp <- workingset %>% filter(id %in% c(ids[1]:ids[length(ids)]))
            #temp <- workingset %>% filter(id %in% ids)
            print(temp)
            leafletProxy("LocalMap") %>% 
                       clearGroup(group="align") %>% 
                       addCircleMarkers(workingset$NewLong, 
                                        workingset$NewLat,
                                        radius=3, opacity=1, color="#000000", 
                                        layerId = workingset$id,
                                        label = workingset$id,
                                        group = "align", 
                                        labelOptions = labelOptions(noHide = TRUE, 
                                                                    offset=c(18,0), 
                                                                    textOnly = TRUE)) %>% 
                              # aqua means selected
                       addCircleMarkers(temp$NewLong, 
                                        temp$NewLat,
                                        radius=3, opacity=1, color="#00FFFF",
                                        layerId = temp$id,
                                        group = "align") 
        }  # end draw highlight
        
        draw_map <- function(i, len, dir){
            lat <- workingset$NewLat[i]
            lon <- workingset$NewLong[i]

            output$LocalMap <- renderLeaflet({
                leaflet() %>%
                       setView(lng = lon , lat = lat, zoom = zoom) %>% 
                       addTiles() %>%
                       addCircleMarkers(workingset$NewLong, 
                                        workingset$NewLat,
                                        radius=3, opacity=1, color="#000000") 

            }) 
        }  # end of draw_map
        draw_mapedit <- function(i, len, dir){
          lat <- workingset$NewLat[i]
          lon <- workingset$NewLong[i]
          
            output$LocalMap <- renderLeaflet({
              print("--1--")
                leaflet() %>%
                       setView(lng = lon , lat = lat, zoom = zoom) %>% 
                       addTiles() %>%
          #if (!is.null(lineID)){ print(lineID)
          #  removeShape(map=LocalMap, layerID=lineID)}
                       addCircleMarkers(workingset$NewLong, 
                                        workingset$NewLat,
                                        radius=3, opacity=1, color="#000000", 
                                        layerId = workingset$id,
                                        label = workingset$id,
                                        labelOptions = labelOptions(noHide = TRUE, 
                                                                    offset=c(18,0), 
                                                                    textOnly = TRUE)) %>% 
              addDrawToolbar(
                targetGroup='draw',
                polygonOptions=FALSE,
                circleOptions=FALSE,
                circleMarkerOptions=FALSE,
                rectangleOptions=FALSE,
                markerOptions=FALSE,
                singleFeature = TRUE,
                editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>%
                addLayersControl(overlayGroups = c('draw'), options =
                                   layersControlOptions(collapsed=FALSE))
              
            })
        } # end  of draw_mapedit
        
        ####  clearAlign clear stuff set for alignment tab
        clearAlign <- function(){
          pt_ids <<- c()
          line_ends <<- tribble(
            ~lat, ~lon
          )
          output$EndPoints <<- renderText("No points picked")
          draw_mapedit(counter$image_number, input$length, input$direction)
          AlignLine <<- AlignLine[0,]
          #if (!is.null(lineID)){ print(lineID)
          #  removeShape(map=LocalMap, layerID=lineID)}
          print("clearAlign")
        }
        
        ####  Find project of point onto line
        ProjPt <- function(pt_id, endpts){ # id = id for point, endpts = endpoints of line
          print("--- ProjPt ---")
          #print(endpts)
          px <- workingset$NewLong[workingset$id==pt_id]
          py <- workingset$NewLat[workingset$id==pt_id]
          dot <- (endpts[2,]$x - endpts[1,]$x)*(px - endpts[1,]$x) +
                 (endpts[2,]$y - endpts[1,]$y)*(py - endpts[1,]$y)
          len <- (endpts[2,]$x - endpts[1,]$x)**2 + (endpts[2,]$y - endpts[1,]$y)**2
          return(c(endpts[1,]$x + (dot*(endpts[2,]$x - endpts[1,]$x))/len,
                   endpts[1,]$y + (dot*(endpts[2,]$y - endpts[1,]$y))/len))
          
        }

        ##############
        #### tabs ####
        ##############
      observeEvent(input$tabs, { # change map based on tab exposed
        print(paste("tab:", input$tabs))  
        if (input$tabs=="annotate") {
          draw_map(counter$image_number, input$length, input$direction)
          draw_points(counter$image_number, input$length, input$direction)
        } else {
          clearAlign()
          draw_mapedit(counter$image_number, input$length, input$direction)
        }
      })
      
        ####################
        ### marker click ###
        ####################
      observeEvent(input$LocalMap_marker_click, {
        click <- input$LocalMap_marker_click
        if(is.null(click) | (input$tabs=="annotate")) # only respond if on correct tab
          return()
        print(paste("click ", click[[1]]))
        #   Change color to show selected and store id
        #   Only store a max of 2 ID's. Overwrite second one if needed
        if (length(pt_ids)==2){
          if (pt_ids[1]==click[[1]]) {return()}  # can't set end = start
          pt_ids[2] <<- click[[1]]
          output$EndPoints <<- renderText(paste("Start:",pt_ids[1], "End:", pt_ids[2]))
        } else if (length(pt_ids)==1) {
          if (pt_ids[1]==click[[1]]) {return()}  # can't set end = start
          pt_ids <<- append(pt_ids, click[[1]])
          output$EndPoints <<- renderText(paste("Start:",pt_ids[1], "End:", pt_ids[2]))
        } else {
          pt_ids[1] <<- click[[1]]
          output$EndPoints <<- renderText(paste("Start:",pt_ids[1]))
        }
        draw_highlight(pt_ids)
        print(paste("pt_ids:", pt_ids))
        
      })   
      
        #################
        ### Draw Line ###
        ################# 
      #observeEvent(input$LocalMap_draw_stop, { # fails on second use
      observeEvent(input$LocalMap_draw_new_feature, {
        if (input$LocalMap_draw_new_feature$properties$feature_type=="polyline"){
          print("------- draw line --------")
          #   Save points for later use
          AlignLine <<- tribble(
            ~x, ~y,
            input$LocalMap_draw_new_feature$geometry$coordinates[[1]][[1]],
            input$LocalMap_draw_new_feature$geometry$coordinates[[1]][[2]],
            input$LocalMap_draw_new_feature$geometry$coordinates[[2]][[1]],
            input$LocalMap_draw_new_feature$geometry$coordinates[[2]][[2]]
          )
          print(AlignLine)
          #lineID <<- input$LocalMap_draw_new_feature$properties$`_leaflet_id`
        } ##  end draw poly
      }, ignoreNULL=TRUE)
        
        #########################
        ### ClearAlign button ###
        #########################
        observeEvent(input$ClearAlign, {
            clearAlign()
         }, ignoreNULL=TRUE)
        
        #########################
        ### Align button ###
        #########################
        observeEvent(input$Align, {
          print(AlignLine)
          for (pt in c(pt_ids[1]:pt_ids[length(ids)])) {
            print(paste("--point id--", pt))
            newpt <- ProjPt(pt, AlignLine) # id = id for point, endpts = endpoints of line
            print(paste("old point:", workingset$NewLong[workingset$id==pt],
                                      workingset$NewLat[workingset$id==pt]))
            print(paste("new point:", newpt[1], newpt[2]))
            #####   delete below here
            leafletProxy("LocalMap") %>% 
              # aqua means selected
              addCircleMarkers(newpt[1],
                               newpt[2],
                               radius=3, opacity=1, color="#0000FF")
            #####  stop delete here
          }
        }, ignoreNULL=TRUE)
        ############################
        ### RevertCurrent button ###
        ############################
        observeEvent(input$RevertCurrent, {
          workingset[workingset$id %in% c(ids[1]:ids[length(ids)]),]$NewLat <- 
                     workingset[workingset$id %in% c(ids[1]:ids[length(ids)]),]$GPSLatitude
          workingset[workingset$id %in% c(ids[1]:ids[length(ids)]),]$NewLong <- 
                     workingset[workingset$id %in% c(ids[1]:ids[length(ids)]),]$GPSLongitude
        }, ignoreNULL=TRUE)
        ########################
        ### RevertAll button ###
        ########################
        observeEvent(input$RevertAll, {
          workingset$NewLong <- workingset$GPSLongitude
          workingset$NewLat <- workingset$GPSLatitude
          
        }, ignoreNULL=TRUE)  
        
        
        
        # Start of Drawing
        #observeEvent(input$LocalMap_draw_start, {
        #  print("------------------Start of drawing")
        #  print(input$LocalMap_draw_start)
        #})
        
        # Stop of Drawing
        #observeEvent(input$LocalMap_draw_stop, {
        #  print("------------------Stopped drawing")
        #  print(input$LocalMap_draw_new_feature$geometry$coordinates[[1]][[1]])
        #  print(input$LocalMap_draw_new_feature$geometry$coordinates[[1]][[2]])
        #  print(input$LocalMap_draw_new_feature$geometry$coordinates[[2]][[1]])
        #  print(input$LocalMap_draw_new_feature$geometry$coordinates[[2]][[2]])
        #})
        
        # New Feature
        observeEvent(input$LocalMap_draw_new_feature, {
          print("------------------New Feature")
          print(input$LocalMap_draw_new_feature)
          #print(input$LocalMap_draw_new_feature$geometry$coordinates[[1]][[1]])
          #print(input$LocalMap_draw_new_feature$geometry$coordinates[[1]][[2]])
          #print(input$LocalMap_draw_new_feature$geometry$coordinates[[2]][[1]])
          #print(input$LocalMap_draw_new_feature$geometry$coordinates[[2]][[2]])
        })
        
        # Edited Features
        observeEvent(input$LocalMap_draw_edited_features, {
          print("------------------Edited Features")
          #print(input$LocalMap_draw_edited_features)
          print(input$LocalMap_draw_edited_features$features[[1]]$geometry$coordinates[[1]][[1]])
          print(input$LocalMap_draw_edited_features$features[[1]]$geometry$coordinates[[1]][[2]])
          print(input$LocalMap_draw_edited_features$features[[1]]$geometry$coordinates[[2]][[1]])
          print(input$LocalMap_draw_edited_features$features[[1]]$geometry$coordinates[[2]][[2]])
        })
        #observeEvent(input$LocalMap_draw_all_features, {
        #  print("------------------All Features")
        #  print(input$LocalMap_draw_all_features)
        #})
        
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
            #draw_map(counter$image_number, input$length, input$direction)
            draw_points(counter$image_number, input$length, input$direction)
         }, ignoreNULL=FALSE)
        
        #################
        ### direction ###
        #################
        observeEvent(input$direction, {
            #draw_map(counter$image_number, input$length, input$direction)
            draw_points(counter$image_number, input$length, input$direction)
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
            draw_points(counter$image_number, input$length, input$direction)
            output$SourceFile <<- renderText(workingset[counter$image_number,]$SourceFile)
            output$LatLong <<- renderText(paste(workingset[counter$image_number,]$NewLat,",",
                                          workingset[counter$image_number,]$NewLong))
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
            draw_points(counter$image_number, input$length, input$direction)
            output$SourceFile <<- renderText(workingset[counter$image_number,]$SourceFile)
            output$LatLong <<- renderText(paste(workingset[counter$image_number,]$NewLat,",",
                                          workingset[counter$image_number,]$NewLong))
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
                mask <- OldDF$SourceFile==workingset$SourceFile[counter$image_number]
                OldDF$Quality[mask] <<- input$quality
                OldDF$Length[mask] <<- input$length
                OldDF$Direction[mask] <<- input$direction
                OldDF$EndLon[mask] <<- workingset$EndLon[counter$image_number]
                OldDF$EndLat[mask] <<- workingset$EndLat[counter$image_number]
                
                #print(OldDF)
                
                saveRDS(OldDF, oldfile)
                
                #   Crop image and write out
                
                image <- image_read(imagefile$tmpfile)  # current view
                
                if(is.null(input$brush)){ # do not crop
                    image %>% 
                        image_strip() %>%  # remove exif headers
                        image_write( 
                                path=paste0(savepath,"/",workingset$SourceFile[counter$image_number]), 
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
                                path=paste0(savepath,"/",workingset$SourceFile[counter$image_number]), 
                                format = 'jpg')
                }
                #       Reset to defaults
                resetControls()
                #       Go to Next
                saved[counter$image_number] <<- TRUE # flag image as saved
                counter$image_number <<- min(length(image_list), counter$image_number+1)
                output$image <- image_prep(image_list[counter$image_number])
                draw_map(counter$image_number, input$length, input$direction)
                draw_points(counter$image_number, input$length, input$direction)
                if (counter$image_number == length(image_list)){
                  showNotification("Last image in set")
                }
            }   
         }, ignoreNULL=TRUE)
    }
)

#print("This is where I can do some cleanup work")
#print(paste("saved", saved))
