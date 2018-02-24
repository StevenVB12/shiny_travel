
# This is the user-interface definition of a Shiny web application.
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

# function to get the most frequent Reason of being in a place
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

shinyUI(fluidPage(
  
  title = h3("Shiny travel"), 
  
  fluidRow(column(9,
                  leafletOutput("map", width="100%")),
           
           column(3,
                  h3("Travel information"),
                  h5("Provide travel inforamtion in a TAB delimited format as shown in the example. 
                     Possible reasons for travelling are 'work', 'conference', 'leisure' and 'companion'"),
                  
                  fileInput("file", 
                            accept = c('text/tab-separated-values'),
                            h4("File input")),
                  
                  checkboxInput("checkbox", "Show travel connections", value = TRUE),
                  
                  actionButton("action", "Run example"),
                  
                  # div(img(src = "SVB.jpg", height = 120, width = 180, align = "center"), style="text-align: center;"),
                  h5("- Steven M. Van Belleghem", align = "right"))
  ),
  
  fluidRow(column(7, 
                  conditionalPanel(condition = "output.fileNotUploaded" , h3("Example data")),
                  conditionalPanel(condition = "output.fileUploaded" , h3("Your data")),
                  formattableOutput("table1")),
           column(4, offset = 1,
                  conditionalPanel(condition = "output.processed" , h3("Time and distance travelled")),
                  formattableOutput("table2"),
                  conditionalPanel(condition = "output.processed" , h3("Time spent in place")),
                  formattableOutput("table3")))
           
           
  ))
  
  

