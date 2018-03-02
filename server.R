
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)
library(leaflet.extras)
library(XML)
library(googleway)
library(geosphere)
library(formattable)
library(plyr)
library(mapview)

key <- "AIzaSyC_Ou0_TXeuXbvCUgh2Ezih5Krh_0RIE9I"

# function to get the most frequent Reason of being in a place
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

shinyServer(function(input, output) {
  
  
  # load example data when 'Run example'
  mydata <- observeEvent(input$action, {
    tbl <- read.table("Sample_SVB.txt", header = TRUE, sep = '\t')
  
  })
  
  # Calculate distance and time data  
  mydata <- reactive({
    
    inFile <- input$file
    
    if (is.null(inFile) & input$action == 0){
      return(read.table("Sample_SVB.txt", header = TRUE, sep = '\t'))
    }
    
    if (!is.null(inFile)){
      tbl <- read.delim(inFile$datapath, header = TRUE, sep='\t')
    }
    
    if (is.null(inFile) & input$action != 0){
      tbl <- read.table("Sample_SVB.txt", header = TRUE, sep = '\t')
    }
      
    
    Place <- as.vector(unique(tbl$Place))
    Lat <- c()
    Lon <- c()
    for(e in 1:length(Place)){
      LatLonData <- google_geocode(as.character(Place[e]), key = key, simplify = TRUE)
      Lat <- c(Lat, LatLonData$results$geometry$location$lat[1])
      Lon <- c(Lon, LatLonData$results$geometry$location$lng[1])
    }
    
    PlaceLatLon <- as.data.frame(cbind(Place,Lat,Lon))
    
    tbl <- merge(tbl, PlaceLatLon, by="Place")
    
    tbl <- tbl[order(as.Date(tbl$Arrive)),]
    
    tbl$Days <- as.integer(as.Date(tbl$Leave) - as.Date(tbl$Arrive)) 
    tbl$Years <- tbl$Days/365
    
    
    tbl$Lat <- as.numeric(as.character((tbl$Lat)))
    tbl$Lon <- as.numeric(as.character((tbl$Lon)))
    
    tbl$Distance_km <- 0
    
    for(e in 1:(nrow(tbl)-1)){ 
      tbl$Distance_km[e+1] <- as.numeric(round((distm(c(tbl$Lon[e], tbl$Lat[e]), c(tbl$Lon[e+1], tbl$Lat[e+1]), 
                                     fun = distHaversine))/1000, 2))
    }
    
    tbl <- tbl[,c(2,3,1,4,5,6,7,8,9)]
    rownames(tbl) <- c()

    colnames(tbl) <- c("Arrive", "Leave", "Place", "Reason", "Lat", "Lon", "Days", "Years", "Distance (km)")
    return(tbl)
    
  })
  
  
  # Return original data with distance and time stats
  output$table1 <- renderFormattable({
    if(ncol(mydata()) == 4){
       formattable(mydata(), list("Years" = color_bar("orange", fun = "proportion"),
                                  "Distance (km)" = color_bar("pink", fun = "proportion"),
                                  "Reason" = formatter("span", style = x ~ifelse(x == "work", style(color = "black", font.weight = "normal"),
                                                                              ifelse(x == "conference", style(color = "orange", font.weight = "normal"),
                                                                                     ifelse(x == "leisure", style(color = "blue", font.weight = "normal"),
                                                                                            ifelse(x == "companion", style(color = "red", font.weight = "normal"),
                                                                                                   ifelse(x == "field", style(color = "green", font.weight = "normal"),NA)))))
                               )))
    }
    else{
      formattable(mydata()[,c(1:4,7:9)], list("Years" = color_bar("orange", fun = "proportion"),
                                              "Distance (km)" = color_bar("pink", fun = "proportion"),
                                              "Reason" = formatter("span", style = x ~ifelse(x == "work", style(color = "black", font.weight = "normal"),
                                                                                ifelse(x == "conference", style(color = "orange", font.weight = "normal"),
                                                                                       ifelse(x == "leisure", style(color = "blue", font.weight = "normal"),
                                                                                              ifelse(x == "companion", style(color = "red", font.weight = "normal"),
                                                                                                     ifelse(x == "field", style(color = "green", font.weight = "normal"),NA)))))
                                 )))
    }
    
  })
  
  # Calculate table with stats for 'reasons'
  mydata2 <- reactive({
    if (!is.null(input$file) | input$action != 0){
      tbl <- mydata()
      
      work <- 0
      conference <- 0
      leisure <- 0
      companion <- 0
      field <- 0
      
      reasons <- c('work', 'conference', 'leisure','companion','field')
      for(e in 1:(nrow(tbl)-1)){
        
        if(tbl$Reason[e] == 'work'       & tbl$Reason[e+1] == 'work')      {work       <- work       + as.numeric(tbl$"Distance (km)"[e+1])}
        if(tbl$Reason[e] == 'conference' & tbl$Reason[e+1] == 'conference'){conference <- conference + as.numeric(tbl$"Distance (km)"[e+1])}
        if(tbl$Reason[e] == 'leisure'    & tbl$Reason[e+1] == 'leisure')   {leisure    <- leisure    + as.numeric(tbl$"Distance (km)"[e+1])}
        if(tbl$Reason[e] == 'companion'  & tbl$Reason[e+1] == 'companion') {companion  <- companion  + as.numeric(tbl$"Distance (km)"[e+1])}
        if(tbl$Reason[e] == 'field'      & tbl$Reason[e+1] == 'field')     {field      <- field      + as.numeric(tbl$"Distance (km)"[e+1])}
        
        if(tbl$Reason[e] == 'work'       & tbl$Reason[e+1] == 'conference'){conference <- conference + as.numeric(tbl$"Distance (km)"[e+1])}
        if(tbl$Reason[e] == 'work'       & tbl$Reason[e+1] == 'leisure')   {leisure    <- leisure    + as.numeric(tbl$"Distance (km)"[e+1])}
        if(tbl$Reason[e] == 'work'       & tbl$Reason[e+1] == 'companion') {companion  <- companion  + as.numeric(tbl$"Distance (km)"[e+1])}
        if(tbl$Reason[e] == 'work'       & tbl$Reason[e+1] == 'field')     {field      <- field      + as.numeric(tbl$"Distance (km)"[e+1])}
        
        if(tbl$Reason[e] == 'conference' & tbl$Reason[e+1] == 'work')      {conference <- conference + as.numeric(tbl$"Distance (km)"[e+1])}
        if(tbl$Reason[e] == 'conference' & tbl$Reason[e+1] == 'leisure')   {leisure    <- leisure    + as.numeric(tbl$"Distance (km)"[e+1])}
        if(tbl$Reason[e] == 'conference' & tbl$Reason[e+1] == 'companion') {companion  <- companion  + as.numeric(tbl$"Distance (km)"[e+1])}
        if(tbl$Reason[e] == 'conference' & tbl$Reason[e+1] == 'field')     {field      <- field      + as.numeric(tbl$"Distance (km)"[e+1])}
        
        if(tbl$Reason[e] == 'leisure'    & tbl$Reason[e+1] == 'work')      {leisure    <- leisure    + as.numeric(tbl$"Distance (km)"[e+1])}
        if(tbl$Reason[e] == 'leisure'    & tbl$Reason[e+1] == 'conference'){conference <- conference + as.numeric(tbl$"Distance (km)"[e+1])}
        if(tbl$Reason[e] == 'leisure'    & tbl$Reason[e+1] == 'companion') {companion  <- companion  + as.numeric(tbl$"Distance (km)"[e+1])}
        if(tbl$Reason[e] == 'leisure'    & tbl$Reason[e+1] == 'field')     {field      <- field      + as.numeric(tbl$"Distance (km)"[e+1])}
        
        if(tbl$Reason[e] == 'companion'  & tbl$Reason[e+1] == 'work')      {companion  <- companion  + as.numeric(tbl$"Distance (km)"[e+1])}
        if(tbl$Reason[e] == 'companion'  & tbl$Reason[e+1] == 'leisure')   {leisure    <- leisure    + as.numeric(tbl$"Distance (km)"[e+1])}
        if(tbl$Reason[e] == 'companion'  & tbl$Reason[e+1] == 'conference'){conference <- conference + as.numeric(tbl$"Distance (km)"[e+1])}
        if(tbl$Reason[e] == 'companion'  & tbl$Reason[e+1] == 'field')     {field      <- field      + as.numeric(tbl$"Distance (km)"[e+1])}
        
        if(tbl$Reason[e] == 'field'      & tbl$Reason[e+1] == 'work')      {field      <- field      + as.numeric(tbl$"Distance (km)"[e+1])}
        if(tbl$Reason[e] == 'field'      & tbl$Reason[e+1] == 'leisure')   {leisure    <- leisure    + as.numeric(tbl$"Distance (km)"[e+1])}
        if(tbl$Reason[e] == 'field'      & tbl$Reason[e+1] == 'conference'){conference <- conference + as.numeric(tbl$"Distance (km)"[e+1])}
        if(tbl$Reason[e] == 'field'      & tbl$Reason[e+1] == 'companion') {companion  <- companion  + as.numeric(tbl$"Distance (km)"[e+1])}
      }
      
      tblDistance <- cbind(reasons, c(work, conference, leisure, companion, field))
      colnames(tblDistance) <- c("Reason", "Distance (km)")
      
      tblDays <- ddply(tbl, c("Reason"), summarise, Days = sum(Days))
      tblDays$Years <- tblDays$Days/365
  
      
      tblProcessed <- merge(tblDays, tblDistance, by="Reason")
      tblProcessed <- as.data.frame(tblProcessed)
      
      tblProcessed$Reason <- as.character(tblProcessed$Reason)
      tblProcessed$Days <- as.numeric(as.character(tblProcessed$Days))
      tblProcessed$Years <- as.numeric(as.character(tblProcessed$Years))
      tblProcessed$"Distance (km)" <- as.numeric(as.character(tblProcessed$"Distance (km)"))
      
      tbTotals <- data.frame(Reason="Total",t(colSums(tblProcessed[,-1])))
      colnames(tbTotals) <- names(tblProcessed)  
    
      tblProcessed <- rbind(tblProcessed, tbTotals)
      
  
      return(tblProcessed)
    }
  })
  
  output$table2 <- renderFormattable({
    if (!is.null(input$file) | input$action != 0){
      formattable(mydata2(), list("Years" = color_bar("orange", fun = "proportion"),
                                  "Distance (km)" = color_bar("pink", fun = "proportion"),
                                  "Reason" = formatter("span", style = x ~ifelse(x == "work", style(color = "black", font.weight = "normal"),
                                                                                 ifelse(x == "conference", style(color = "orange", font.weight = "normal"),
                                                                                        ifelse(x == "leisure", style(color = "blue", font.weight = "normal"),
                                                                                               ifelse(x == "companion", style(color = "red", font.weight = "normal"),
                                                                                                      ifelse(x == "field", style(color = "green", font.weight = "normal"),NA))))))))
    }
    else{
      return(NULL)
    }
  })
  
  # Calculate table with stats per 'place'
  mydata3 <- reactive({
    if (!is.null(input$file) | input$action != 0){
      
      tbl <- mydata()
      
      tblPlace <- ddply(tbl, c("Place"), summarise, Days = sum(Days))
      tblPlace$Years <- tblPlace$Days/365
      
      return(tblPlace)
    }
  })    
  
  output$table3 <- renderFormattable({
    if (!is.null(input$file) | input$action != 0){
      formattable(mydata3(), list("Years" = color_bar("orange", fun = "proportion")))
    }
    else{
      return(NULL)
    }
  })
  
  
  # Make the map
  output$map <- renderLeaflet({
    
    inFile <- input$file
    
    # Show empty map if no data
    if (is.null(inFile) & input$action == 0)
      return(
        leaflet() %>%
          setView(04.0761361, 51.1681139, zoom = 2) %>%
          addTiles() %>%
          addProviderTiles("Stamen.Watercolor") %>%
          addProviderTiles("Stamen.TonerHybrid") %>%
          addFullscreenControl(pseudoFullscreen = TRUE)
      )
    
    if(!is.null(inFile) | input$action != 0){
   
      tbl <- mydata()
      
      # Summarize with most frequent reason of travel path
      tblPlace <- ddply(tbl, c("Place"), summarise, Reason = Mode(Reason), Days = sum(Days), Lat = mean(Lat), Lon = mean(Lon))
      
      colors <- c("black", "orange", "blue", "red","green")
      Reason <- c("work", "conference", "leisure", "companion","field")
      df <- data.frame(Reason, colors)
      
      tblPlace <- merge(tblPlace, df, by="Reason")
      tbl <- merge(tbl, df, by="Reason")
      
      tbl <- tbl[order(as.Date(tbl$Arrive)),]
      
      tbl$dest <- NA
      
      for(e in 1:(nrow(tbl)-1)){
        tbl$dest[e] <- as.character(tbl$Place[(e+1)])
      }
      
      # tbl$route <- paste(sort(tbl$Place,tbl$dest))
      route <- apply(cbind(as.character(tbl$Place),as.character(tbl$dest)), 1, function(x) paste(sort(x), collapse=" "))
      
      tbl <- cbind(tbl, route)
      tblfreq <- count(tbl, c('route'))
      
      tbl <- merge(tbl, tblfreq, by="route")
      tbl <- tbl[order(as.Date(tbl$Arrive)),]
      
      lineCols <- tbl$colors
      
      icons <- awesomeIcons(
        icon = 'whatever',
        library = 'ion',
        markerColor = tblPlace$colors
      )
      
      if(input$checkbox == FALSE){
        return(
          leaflet() %>%
            setView(04.0761361, 51.1681139, zoom = 2) %>%
            addTiles() %>%
            addProviderTiles("Stamen.Watercolor") %>%
            addProviderTiles("Stamen.TonerHybrid") %>%
            addFullscreenControl(pseudoFullscreen = TRUE) %>%
            addCircleMarkers(data=tblPlace, ~Lon, ~Lat, color = tblPlace$colors, radius = log(tblPlace$Days)*3) %>%
            addAwesomeMarkers(data=tblPlace, ~Lon, ~Lat,  icon = icons, popup = paste(tblPlace$Place, sep="<br>"))
        )
      }
      
      if(input$checkbox == TRUE){
        
          basemap <- leaflet() %>%
            setView(04.0761361, 51.1681139, zoom = 2) %>%
            addTiles() %>%
            addProviderTiles("Stamen.Watercolor") %>%
            addProviderTiles("Stamen.TonerHybrid") %>%
            addFullscreenControl(pseudoFullscreen = TRUE) %>%
            addCircleMarkers(data=tblPlace, ~Lon, ~Lat, color = tblPlace$colors, radius = log(tblPlace$Days)*3) %>%
            addAwesomeMarkers(data=tblPlace, ~Lon, ~Lat,  icon = icons, popup = paste(tblPlace$Place, sep="<br>"))
          
          for(e in 1:(nrow(tbl)-1)){
            
            # work get lowest priority
            if(tbl$Reason[e+1] == 'work'){
            colr <- tbl$colors[e]
            }
            else{ colr <- tbl$colors[e+1]}

            basemap <- basemap %>% addPolylines(data=tbl[e:(e+1),], ~Lon, ~Lat, color = colr, weight = tbl$freq[e], opacity = 0.6)
          }
        
        return(basemap)
      }
    }
  })
  
  output$fileNotUploaded <- reactive({
    return(is.null(input$file))
  })
  outputOptions(output, 'fileNotUploaded', suspendWhenHidden=FALSE)
  
  output$fileUploaded <- reactive({
    return(!is.null(input$file))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)

  output$processed <- reactive({
    return(!is.null(mydata2()))
  })
  outputOptions(output, 'processed', suspendWhenHidden=FALSE)
})
