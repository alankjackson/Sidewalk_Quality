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

#   Move a few saved quantites to workingset

mask <- OldDF$SourceFile %in% workingset$SourceFile # pick out records on OldDF that exist in workingset
workingset$Quality <- OldDF$Quality[mask]
workingset$EndLon <- OldDF$EndLon[mask]
workingset$EndLat <- OldDF$EndLat[mask]
workingset$NewLong <- OldDF$NewLong[mask]
workingset$NewLat <- OldDF$NewLat[mask]

SavePending<<-FALSE # flag to prevent leaving a tab with unfinished business

#######################################################
#   Javascript for removing polylines
# https://github.com/bhaskarvk/leaflet.extras/issues/96
#######################################################
scr <- tags$script(HTML(
  "
Shiny.addCustomMessageHandler(
  'removeleaflet',
  function(x){
    console.log('deleting',x)
    // get leaflet map
    var map = HTMLWidgets.find('#' + x.elid).getMap();
    // remove
    map.removeLayer(map._layers[x.layerid])
  })
"
))
#map.invalidateSize() # if I knew where to put this to ensure tiles get drawn
##################################################
# Define UI for displaying and annotating photos
##################################################

# Note that coordinates are in pixels
shinyApp(
    ui = basicPage(
        fluidRow(
          scr,
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
                    conditionalPanel(
                    condition = "input.tabs == 'AnnotateTab'",
                    #HTML("<hr>"),
                    textOutput("SourceFile"),
                    #textOutput("LatLong"),

                    HTML("<hr>"),
                      actionButton("Prev", "Prev"),
                      actionButton("Next", "Next"),
                      actionButton("Rotate", "Rotate"),
                      actionButton("Save", "Save"),
                      actionButton("Ends", "Ends")),
                   HTML("<hr>"),
                   
                   #        Add map
                   
                   leafletOutput("LocalMap", height="45vh")
                   #uiOutput("LocalMapDisplay")
                   #leafletOutput("LocalMap", height="60vh"))
            ),
            
            #       Add left column with controls for labeling
            column(width = 4,
                   #    Quality, Length, and Direction
              tabsetPanel(id="tabs",
                tabPanel("Annotate", fluid=TRUE,value="AnnotateTab",
                   radioButtons("quality", label = "Sidewalk quality",
                                choices = list("None selected" = "",
                                               "Good"="Good", "Bushes"="Bushes", 
                                               "Gap"="Gap",  "Offset"="Offset", 
                                               "Shattered"="Shattered", 
                                               "Obstructed"="Obstructed", 
                                               "Debris/Mud"="Debris/Mud", 
                                               "Gravel"="Gravel", 
                                               "No Curb Cut"="No Curb Cut", 
                                               "Missing"="Missing",
                                               "BadPict"="BadPict")
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
                tabPanel("Align", fluid=TRUE, value="AlignTab",
                         HTML("<hr>"),
                         tags$em("Pick the start and end points for alignment"),
                         tags$em("and then draw the line to move them to."),
                         HTML("<hr>"),
                         textOutput("EndPoints"),
                         HTML("<hr>"),
                         actionButton("Align", "Align"),
                         actionButton("ClearAlign", "Clear"),
                         actionButton("ApplyAlign", "Apply"),
                         HTML("<hr>"),
                         actionButton("RevertCurrent", "Revert Current"),
                         actionButton("RevertAll", "Revert All"),
                         actionButton("SaveAlign", "Save")
                ), # end tabPanel Align
                tabPanel("Move", fluid=TRUE, value="MoveTab",
                         HTML("<hr>"),
                         tags$em("Pick the point to be moved"),
                         tags$em("and then pick the destination."),
                         HTML("<hr>"),
                         actionButton("Move", "Move"),
                         actionButton("ClearMove", "Clear"),
                         actionButton("ApplyMove", "Apply"),
                         HTML("<hr>"),
                         actionButton("RevertMove", "Revert Move"),
                         actionButton("SaveMove", "Save")
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
        
        #########################################
        #   Draw map - show marker at lat/long, and draw line of proper length
        #   and direction as chosen
        #   Use approximation of 1 degree = 340,000 feet
        #########################################
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
        #########################################
        #   Draw lines to endpoints
        #########################################
        draw_ends <- function(){
          for (i in 1:nrow(workingset)){
              leafletProxy("LocalMap") %>% 
                addPolylines(
                  lat=c(workingset[i,]$NewLat, workingset[i,]$EndLat),
                  lng=c(workingset[i,]$NewLong, workingset[i,]$EndLon),
                  color="#FF0000",
                  weight=2,
                  opacity=1
                )
          }
        }
        #########################################
        #   Draw a single point to highlight
        #########################################
        draw_highlight <- function(ptids){
            temp <- workingset %>% filter(id %in% c(ptids[1]:ptids[length(ptids)]))
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
        
        #########################################
        ####    Base Map
        #########################################
        draw_map <- function(i){
          print("==== draw map ====")
            lat <- workingset$NewLat[i]
            lon <- workingset$NewLong[i]
            
          Sys.sleep(.5) # to give map frame time to settle down so map will draw correctly
            
            output$LocalMap <- renderLeaflet({
                leaflet() %>%
                       addTiles() %>%
                       setView(lng = lon , lat = lat, zoom = zoom) %>% 
                       addCircleMarkers(workingset$NewLong, 
                                        workingset$NewLat,
                                        radius=3, opacity=1, color="#000000") 

            }) 
        }  # end of draw_map
        #########################################
        ###   Draw edit map for Align and Move
        #########################################
        draw_mapedit <- function(action){
          if(action=="Align") {polyline <- drawPolylineOptions()
                               marker <- FALSE
          } else {polyline <- FALSE
                  marker <- drawMarkerOptions()
          }
              leafletProxy("LocalMap") %>% 
                       clearShapes() %>% 
                       clearGroup(group="projected") %>% 
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
                polylineOptions = polyline,
                polygonOptions=FALSE,
                circleOptions=FALSE,
                circleMarkerOptions=FALSE,
                rectangleOptions=FALSE,
                markerOptions=marker,
                singleFeature = TRUE,
                editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions(),
                                                 edit=FALSE, remove=FALSE))  %>%
                addLayersControl(overlayGroups = c('draw'), options =
                                   layersControlOptions(collapsed=FALSE))
              
        } # end  of draw_mapedit
        
        #########################################
        ####  clearAlign clear stuff set for alignment tab
        #########################################
        clearAlign <- function(){
          pt_ids <<- c()
          line_ends <<- tribble(
            ~lat, ~lon
          )
          output$EndPoints <<- renderText("No points picked")
          draw_mapedit("Align")
          AlignLine <<- AlignLine[0,]
          if (!is.null(lineID)){ 
            session$sendCustomMessage(
              "removeleaflet",
              list(elid="LocalMap", layerid=lineID)
            )
            }
          print("clearAlign")
        }
        
        #########################################
        ####  Find projection of point onto line
        #########################################
        ProjPt <- function(pt_id, endpts){ # id = id for point, endpts = endpoints of line
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
        if (input$tabs=="AnnotateTab") {
          if (SavePending) {
            showNotification("Must Save or Clear")
            return()
          }
          draw_map(counter$image_number)
          draw_points(counter$image_number, input$length, input$direction)
        } else if (input$tabs=="AlignTab") {
          if (SavePending) {
            showNotification("Must Save or Clear")
            return()
          }
          draw_map(counter$image_number)
          draw_mapedit("Align")
          pt_ids <<- c()
        } else {
          if (SavePending) {
            showNotification("Must Save or Clear")
            return()
          }
          draw_map(counter$image_number)
          draw_mapedit("Move")
          
        }
      }, ignoreNULL=FALSE, ignoreInit=TRUE)
      
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
        } else { # first point
          pt_ids <<- c(click[[1]])
          output$EndPoints <<- renderText(paste("Start:",pt_ids[1]))
        }
        draw_highlight(pt_ids)
        print(paste("pt_ids:", pt_ids))
        
      }, ignoreNULL=FALSE, ignoreInit=TRUE)   
      
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
          lineID <<- input$LocalMap_draw_new_feature$properties$`_leaflet_id`
        } ##  end draw poly
      }, ignoreNULL=FALSE, ignoreInit=TRUE)
        
        #########################
        ### ClearAlign button ###
        #########################
        observeEvent(input$ClearAlign, {
          clearAlign()
          pt_ids <<- c()
          SavePending<<-FALSE
         }, ignoreNULL=TRUE)
        
        #########################
        ### Align button ###
        #########################
        observeEvent(input$Align, {
          print(paste("Align Button:", AlignLine))
          for (pt in c(pt_ids[1]:pt_ids[length(pt_ids)])) {
            newpt <- ProjPt(pt, AlignLine) # id = id for point, endpts = endpoints of line
            #####   add projected points to map
            leafletProxy("LocalMap") %>% 
              # aqua means selected
              addCircleMarkers(newpt[1],
                               newpt[2],
                               group = "projected", 
                               radius=3, opacity=1, color="#0000FF")
          }
        }, ignoreNULL=TRUE)
        ####################
        ### Apply button ###
        ####################
        observeEvent(input$ApplyAlign, {
          print("--- apply ---")
          for (pt in c(pt_ids[1]:pt_ids[length(pt_ids)])) {
            newpt <- ProjPt(pt, AlignLine) # id = id for point, endpts = endpoints of line
            workingset$NewLong[workingset$id==pt] <<- newpt[1] 
            workingset$NewLat[workingset$id==pt] <<- newpt[2]
            #     Also need to move EndLon and EndLat
print(paste("endpts before", workingset$EndLon[workingset$id==pt], workingset$EndLat[workingset$id==pt]))
            workingset$EndLon[workingset$id==pt] <<- newpt[1] -
                                                     workingset$GPSLongitude[workingset$id==pt] +
                                                     workingset$EndLon[workingset$id==pt]
            workingset$EndLat[workingset$id==pt] <<- newpt[2] -
                                                     workingset$GPSLatitude[workingset$id==pt] +
                                                     workingset$EndLat[workingset$id==pt]
print(paste("endpts after ", workingset$EndLon[workingset$id==pt], workingset$EndLat[workingset$id==pt]))
          }
          #draw_map(counter$image_number)
          draw_map(as.numeric(pt)) ###############################  untested
          draw_mapedit("Align")
          draw_ends() ### delete me
          SavePending<<-TRUE
        }, ignoreNULL=TRUE)
        ############################
        ### RevertCurrent button ###
        ############################
        observeEvent(input$RevertCurrent, {
          print("--- revert ---")
          workingset[workingset$id %in% c(pt_ids[1]:pt_ids[length(pt_ids)]),]$NewLat <<- 
                     workingset[workingset$id %in% c(pt_ids[1]:pt_ids[length(pt_ids)]),]$GPSLatitude
          workingset[workingset$id %in% c(pt_ids[1]:pt_ids[length(pt_ids)]),]$NewLong <<- 
                     workingset[workingset$id %in% c(pt_ids[1]:pt_ids[length(pt_ids)]),]$GPSLongitude
          SavePending<<-FALSE
          draw_map(counter$image_number)
          draw_mapedit("Align")
        }, ignoreNULL=TRUE)
        ########################
        ### RevertAll button ###
        ########################
        observeEvent(input$RevertAll, {
          workingset$NewLong <<- workingset$GPSLongitude
          workingset$NewLat <<- workingset$GPSLatitude
          SavePending<<-FALSE
          draw_map(counter$image_number)
          draw_mapedit("Align")
          
        }, ignoreNULL=TRUE)  
        ########################
        ### SaveAlign button ###
        ########################
        observeEvent(input$SaveAlign, {
          if (!SavePending) {
              showNotification("Must Apply before Saving")
            return()
          }
          print("--- SaveAlign ---")

          #   Update OldDF with new points
          for (i in pt_ids[1]:pt_ids[length(pt_ids)]) {
            maskWork <- workingset$id == i
            maskOld <- OldDF$SourceFile==workingset[maskWork,]$SourceFile
            OldDF$EndLon[maskOld] <- workingset$EndLon[maskWork]
            OldDF$EndLat[maskOld] <- workingset$EndLat[maskWork]
            OldDF$NewLong[maskOld] <- workingset$NewLong[maskWork]
            OldDF$NewLat[maskOld] <- workingset$NewLat[maskWork]
          }
          #   Write OldDF out to permanent file
          saveRDS(OldDF, oldfile)
          #   Reset flags
          pt_ids <<- c()
          SavePending<<-FALSE
        }, ignoreNULL=TRUE)  
        
        
        # New Feature
        #observeEvent(input$LocalMap_draw_new_feature, {
        #  print("------------------New Feature")
        #  print(input$LocalMap_draw_new_feature)
          #print(input$LocalMap_draw_new_feature$geometry$coordinates[[1]][[1]])
          #print(input$LocalMap_draw_new_feature$geometry$coordinates[[1]][[2]])
          #print(input$LocalMap_draw_new_feature$geometry$coordinates[[2]][[1]])
          #print(input$LocalMap_draw_new_feature$geometry$coordinates[[2]][[2]])
        #})
        
        # Edited Features
        #observeEvent(input$LocalMap_draw_edited_features, {
          #print("------------------Edited Features")
          #print(input$LocalMap_draw_edited_features)
          #print(input$LocalMap_draw_edited_features$features[[1]]$geometry$coordinates[[1]][[1]])
          #print(input$LocalMap_draw_edited_features$features[[1]]$geometry$coordinates[[1]][[2]])
          #print(input$LocalMap_draw_edited_features$features[[1]]$geometry$coordinates[[2]][[1]])
          #print(input$LocalMap_draw_edited_features$features[[1]]$geometry$coordinates[[2]][[2]])
        #})
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
        ### Ends   ###
        ##############
        observeEvent(input$Ends, {
            #draw_map(counter$image_number, input$length, input$direction)
            draw_ends()
         }, ignoreNULL=FALSE)
        
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
            #draw_map(counter$image_number, input$length, input$direction)
            draw_map(counter$image_number)
            draw_points(counter$image_number, input$length, input$direction)
            output$SourceFile <<- renderText(paste(workingset[counter$image_number,]$SourceFile, ":",workingset[counter$image_number,]$id))
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
            #draw_map(counter$image_number, input$length, input$direction)
            draw_map(counter$image_number)
            draw_points(counter$image_number, input$length, input$direction)
            output$SourceFile <<- renderText(paste(workingset[counter$image_number,]$SourceFile, ":",workingset[counter$image_number,]$id))
            #output$SourceFile <<- renderText(workingset[counter$image_number,]$SourceFile)
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
                OldDF$NewLong[mask] <<- workingset$NewLong[counter$image_number]
                OldDF$NewLat[mask] <<- workingset$NewLat[counter$image_number]
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
                #draw_map(counter$image_number, input$length, input$direction)
                draw_map(counter$image_number)
                draw_points(counter$image_number, input$length, input$direction)
                if (counter$image_number == length(image_list)){
                  showNotification("Last image in set")
                }
            }   
         }, ignoreNULL=TRUE)
    }
)

