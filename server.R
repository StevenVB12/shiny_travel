
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

###
key <- "AIzaSyC_Ou0_TXeuXbvCUgh2Ezih5Krh_0RIE9I"

# function to get the most frequent Reason of being in a place
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
###



shinyServer(function(input, output) {
  
  
  # load example data when 'Run example'
  mydata <- eventReactive(input$action, {
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
    country <- c()
    for(e in 1:length(Place)){
      LatLonData <- google_geocode(as.character(Place[e]), key = key, simplify = TRUE)
      Lat <- c(Lat, LatLonData$results$geometry$location$lat[1])
      Lon <- c(Lon, LatLonData$results$geometry$location$lng[1])
      countryIndx <- grep('country', LatLonData$results$address_components[[1]]$types)
      country <- c(country, LatLonData$results$address_components[[1]]$long_name[countryIndx])
    }
    
    PlaceLatLon <- as.data.frame(cbind(Place,Lat,Lon,country))
    
    tbl <- merge(tbl, PlaceLatLon, by="Place")
    
    tbl <- tbl[order(as.Date(tbl$Arrive)),]
    
    tbl$Days <- as.integer(as.Date(tbl$Depart) - as.Date(tbl$Arrive)) 
    tbl$Years <- tbl$Days/365
    
    
    tbl$Lat <- as.numeric(as.character((tbl$Lat)))
    tbl$Lon <- as.numeric(as.character((tbl$Lon)))
    
    tbl$Distance_km <- 0
    
    for(e in 1:(nrow(tbl)-1)){ 
      tbl$Distance_km[e+1] <- as.numeric(round((distm(c(tbl$Lon[e], tbl$Lat[e]), c(tbl$Lon[e+1], tbl$Lat[e+1]), 
                                     fun = distHaversine))/1000, 2))
    }
    
    tbl <- tbl[,c(2,3,1,4,5,6,7,8,9,10)]
    rownames(tbl) <- c()

    colnames(tbl) <- c("Arrive", "Depart", "Place", "Reason", "Lat", "Lon", "Country", "Days", "Years", "Distance (km)")
    return(tbl)
    
  })
  
  
  # Return original data with distance and time stats
  output$table1 <- renderFormattable({
    if(ncol(mydata()) == 4){ # If nothing has been calculate, just show formatted table
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
  
  
  # Calculate table with stats per 'country'
  mydata4 <- reactive({
    if (!is.null(input$file) | input$action != 0){
      
      tbl <- mydata()
      
      tblPlace <- ddply(tbl, c("Country"), summarise, Days = sum(Days))
      tblPlace$Years <- tblPlace$Days/365
      
      return(tblPlace)
    }
  })    
  
  output$table4 <- renderFormattable({
    if (!is.null(input$file) | input$action != 0){
      formattable(mydata4(), list("Years" = color_bar("orange", fun = "proportion")))
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
      
      tbl$Dateconc <- paste(tbl$Arrive, tbl$Depart, sep = ' till ')
      
      # Summarize with most frequent reason of travel path
      tblPlace <- ddply(tbl, c("Place"), summarise, 
                        Reason = Mode(Reason), 
                        Days = sum(Days), 
                        Lat = mean(Lat), 
                        Lon = mean(Lon), 
                        dates=paste(Dateconc, collapse = "<br>"))
      
      colors <- c("black", "orange", "blue", "red","green")
      Reason <- c("work", "conference", "leisure", "companion", "field")
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
            addAwesomeMarkers(data=tblPlace, ~Lon, ~Lat,  icon = icons, popup = paste(tblPlace$Place, paste('Days: ', tblPlace$Days, sep=''), tblPlace$dates, sep="<br>"))
          
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
  
  
  # make plot distance
  output$plot1 <- renderPlot({
    
    if (!is.null(input$file) | input$action != 0){
    
      tbl <- mydata()
      tbl$Arrive <- as.Date(tbl$Arrive)
      tbl$Depart <- as.Date(tbl$Depart)
      
      tbl$Year1 <- format(as.Date(tbl$Arrive, format="%Y-%m-%d"),"%Y")
      tbl$Year1 <- as.numeric(tbl$Year1)
      tbl$Year2 <- format(as.Date(tbl$Depart, format="%Y-%m-%d"),"%Y")
      tbl$Year2 <- as.numeric(tbl$Year2)
      
      colnames(tbl) <- c("Arrive", "Depart", "Place", "Reason", "Lat", "Lon", "Country", "Days", "Years", "Distance", "Year1", "Year2")
      
      tbl$Reason2 <- tbl$Reason
      for(e in 2:(nrow(tbl)-1)){
        if(tbl$Reason[e] == 'work'){
          tbl$Reason2[e] <- tbl$Reason[e-1]
        }  
      }
      
      
      tblDistanceYear <- ddply(tbl, c("Year1","Reason2"), summarise, Distance = sum(Distance))
      tblDistanceYearT <- ddply(tbl, c("Year1"), summarise, Distance = sum(Distance))
      
      # Years <- as.numeric(unique(tblDistanceYear$Year))
      colors <- c("black", "orange", "blue", "red","green")
      Reasons <- c("work", "conference", "leisure", "companion", "field")
      Reasons2 <- as.vector(unique(tbl$Reason2))
      
      colors2 <- c()
      for(e in 1:length(Reasons2)){
        indx <- grep(Reasons2[e], Reasons)
        colors2 <- c(colors2, colors[indx])
      }
      
      
      # add zeros for missing years
      for(e in min(tbl$Year1):max(tbl$Year2)){
        subTable <- subset(tblDistanceYear, tblDistanceYear$Year == e)
        for(i in 1:length(Reasons2)){
          if(!is.element(Reasons2[i], subTable$Reason2)){
            tblDistanceYear <- rbind(tblDistanceYear, c(e, Reasons2[i], 0))
          }
        }
      }
      tblDistanceYear$Year1 <- as.numeric(tblDistanceYear$Year1)
      tblDistanceYear$Distance <- as.numeric(tblDistanceYear$Distance)
      tblDistanceYear <- tblDistanceYear[order(tblDistanceYear$Year1),]
      
     
      # par(mar=c(5.1, 4.1, 4.1, 11.1), xpd=TRUE)
      
      plot(NULL, 
           xlab = "Year",
           ylab = "Distance (km)",
           xaxt = "n",
           bty = 'l',
           xlim = c(min(tblDistanceYear$Year1),max(tblDistanceYear$Year1)), 
           ylim = c(min(tblDistanceYearT$Distance),max(tblDistanceYearT$Distance)))
      
      y <- rep(tblDistanceYearT$Distance)
      x <- rep(tblDistanceYearT$Year1)
      x2 <- c(min(x), x, max(x))
      y2 <- c(0, y, 0)
      
      polygon(x2, y2, col = adjustcolor("pink", alpha.f = 0.2), border = NA)
      
      for(i in 1:length(Reasons2)){
        
        subtbl <- subset(tblDistanceYear, as.character(tblDistanceYear$Reason2) == Reasons2[i])
        par(new = TRUE)
        plot(subtbl$Year1, subtbl$Distance, col = adjustcolor(colors2[i], alpha.f = 0.3), pch = 19, type = 'b', lwd = 2, cex = 2, 
             xlim = c(min(tblDistanceYear$Year1),max(tblDistanceYear$Year1)), 
             ylim = c(min(tblDistanceYearT$Distance),max(tblDistanceYearT$Distance)),
             xaxt = "n",
             axes = FALSE, ann = FALSE)
      }
      axis(1, at = min(tblDistanceYear$Year1):max(tblDistanceYear$Year1))
      
      # par(new = TRUE)
      # plot(tblDistanceYearT$Year, tblDistanceYearT$Distance, col = adjustcolor("black", alpha.f = 0.3), pch = 1, type = 'b', lwd = 2, cex = 2, 
      #      xlim = c(min(tblDistanceYearT$Year),max(tblDistanceYearT$Year)), 
      #      ylim = c(min(tblDistanceYearT$Distance),max(tblDistanceYearT$Distance)),
      #      axes = FALSE, ann = FALSE)
      legend(min(tblDistanceYear$Year1), max(tblDistanceYearT$Distance),
               Reasons2, lwd=2, #inset=c(-0.2,0),
               col=colors2, box.col = adjustcolor("white", alpha.f = 0.5), bg = adjustcolor("white", alpha.f = 0.7), box.lwd = 0, y.intersp=1.5)
      
    }
    else{
      return(NULL)
    }
  })
  
  
  # make plot days
  output$plot2 <- renderPlot({
    
    if (!is.null(input$file) | input$action != 0){
      
      tbl <- mydata()
      tbl$Arrive <- as.Date(tbl$Arrive)
      tbl$Depart <- as.Date(tbl$Depart)
      
      tbl$Year1 <- format(as.Date(tbl$Arrive, format="%Y-%m-%d"),"%Y")
      tbl$Year1 <- as.numeric(tbl$Year1)
      tbl$Year2 <- format(as.Date(tbl$Depart, format="%Y-%m-%d"),"%Y")
      tbl$Year2 <- as.numeric(tbl$Year2)
      
      tbl$DaysYear1 <- 0
      tbl$DaysYear2 <- 0
      for(e in 1:nrow(tbl)){
        if(tbl$Year1[e] == tbl$Year2[e]){
          tbl$DaysYear1[e] <- tbl$Days[e]
          tbl$DaysYear2[e] <- 0
        }
        if(tbl$Year1[e] != tbl$Year2[e]){
          tbl$DaysYear1[e] <- as.Date(paste(tbl$Year1[e],'12','31',sep='-')) - tbl$Arrive[e]
          tbl$DaysYear2[e] <- tbl$Depart[e] - as.Date(paste(tbl$Year2[e],'01','01',sep='-'))
        }
      }
      
      colnames(tbl) <- c("Arrive", "Depart", "Place", "Reason", "Lat", "Lon", "Country", "Days", "Years", "Distance", "Year1","Year2","DaysYear1","DaysYear2")

      
      tblDistanceYearY1 <- ddply(tbl, c("Year1","Reason"), summarise, Days = sum(DaysYear1))
      tblDistanceYearY2 <- ddply(tbl, c("Year2","Reason"), summarise, Days = sum(DaysYear2))
      
      colnames(tblDistanceYearY1) <- c('Year','Reason','Days')
      colnames(tblDistanceYearY2) <- c('Year','Reason','Days')
      
      tblDistanceYearY12 <- rbind(tblDistanceYearY1,tblDistanceYearY2)
      tblDistanceYearD <- ddply(tblDistanceYearY12, c("Year","Reason"), summarise, Days = sum(Days))
      
      tblDistanceYearY12NW <- subset(tblDistanceYearY12, tblDistanceYearY12$Reason != "work")
      tblDistanceYearDT <- ddply(tblDistanceYearY12NW, c("Year"), summarise, Days = sum(Days))
      
      Years <- as.numeric(unique(tblDistanceYearD$Year))
      colors <- c("black", "orange", "blue", "red","green")
      Reasons <- c("work", "conference", "leisure", "companion", "field")
      
      Reasons2 <- as.vector(unique(tbl$Reason))
      
      colors2 <- c()
      for(e in 1:length(Reasons2)){
        indx <- grep(Reasons2[e], Reasons)
        colors2 <- c(colors2, colors[indx])
      }
      
      for(e in min(Years):max(Years)){
        if(!is.element(e, as.numeric(as.character(tblDistanceYearDT$Year)))){
          tblDistanceYearDT <- rbind(tblDistanceYearDT, c(e, 0))
        }
      }
      tblDistanceYearDT <- tblDistanceYearDT[order(tblDistanceYearDT$Year),]
      
      tblDistanceYearD$Reason <- as.character(tblDistanceYearD$Reason)
      
      # add zeros for missing years
      for(e in min(Years):max(Years)){
        if(!is.element(e, as.numeric(as.character(tbl$Year1))) & !is.element(e, as.numeric(as.character(tbl$Year2)))){
          tblDistanceYearD <- rbind(tblDistanceYearD, c(e, "work", 365))
          tblDistanceYearD <- rbind(tblDistanceYearD, c(e, "conference", 0))
          tblDistanceYearD <- rbind(tblDistanceYearD, c(e, "leisure", 0))
          tblDistanceYearD <- rbind(tblDistanceYearD, c(e, "companion", 0))
          tblDistanceYearD <- rbind(tblDistanceYearD, c(e, "field", 0))
        }
        else{
          subTable <- subset(tblDistanceYearD, tblDistanceYearD$Year == e)
          for(i in 1:length(Reasons2)){
            if(!is.element(Reasons2[i], subTable$Reason)){
              tblDistanceYearD <- rbind(tblDistanceYearD, c(e, Reasons2[i], 0))
            }
          }
        }
      }
      tblDistanceYearD$Year <- as.numeric(tblDistanceYearD$Year)
      tblDistanceYearD$Days <- as.numeric(tblDistanceYearD$Days)
      tblDistanceYearD <- tblDistanceYearD[order(tblDistanceYearD$Year),]
      
      # par(mar=c(5.1, 4.1, 4.1, 11.1), xpd=TRUE)
      
      plot(NULL, 
           xlab = "Year",
           ylab = "Days",
           bty = 'l',
           xaxt = 'n',
           xlim = c(min(tblDistanceYearD$Year),max(tblDistanceYearD$Year)), 
           ylim = c(min(tblDistanceYearD$Days),max(tblDistanceYearD$Days)))
      
      y <- rep(tblDistanceYearDT$Days)
      x <- rep(tblDistanceYearDT$Year)
      x2 <- c(min(x), x, max(x))
      y2 <- c(0, y, 0)
      
      polygon(x2, y2, col = adjustcolor("pink", alpha.f = 0.2), border = NA)
      
      
      for(i in 1:length(Reasons2)){
        
        subtbl <- subset(tblDistanceYearD, as.character(tblDistanceYearD$Reason) == Reasons2[i])
        
        par(new = TRUE)
        plot(subtbl$Year, subtbl$Days, col = adjustcolor(colors2[i], alpha.f = 0.3), pch = 19, type = 'b', lwd = 2, cex = 2,
             xlim = c(min(tblDistanceYearD$Year),max(tblDistanceYearD$Year)), 
             ylim = c(min(tblDistanceYearD$Days),max(tblDistanceYearD$Days)),
             axes = FALSE, ann = FALSE)
      }
      axis(1, at = min(tblDistanceYearD$Year):max(tblDistanceYearD$Year))
      
      
      legend(min(tblDistanceYearD$Year), max(tblDistanceYearD$Days),
             Reasons2, lwd=2, #inset=c(-0.2,0),
               col = colors2, box.col = adjustcolor("white", alpha.f = 0.5), bg = adjustcolor("white", alpha.f = 0.7), box.lwd = 0, y.intersp=1.5)
      
    }
    else{
      return(NULL)
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
