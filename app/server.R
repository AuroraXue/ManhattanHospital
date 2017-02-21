#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(datasets)

# Define server logic required to plot various variables against mpg
library(ggplot2)
library("leaflet")
library("dplyr")
library("png")
library("grid")

filename<-"../data/hospital_manhattan_basic_info.csv"
Manhattan_hospital<-as.data.frame(read.csv(file = filename,header=T))

shinyServer(function(input, output) {
  
  

  nyc<-reactive({
  content1<-paste("<b><a href ='http://www.mountsinai.org'> Mount Sinai Hospital</a><b>","<br/>",
                    "Overall Rank: ","<br/>", 
                    "Average Cost: ")
    content2<-paste("<b><a href ='http://www.mountsinai.org/locations/st-lukes'> Mount Sinai St. Luke's Hospital</a><b>","<br/>",
                    "Overall Rank: ","<br/>", 
                    "Average Cost: ")
    content2<-paste("<b><a href ='http://www.mountsinai.org/locations/st-lukes'> Mount Sinai St. Luke's Hospital</a><b>","<br/>",
                    "Overall Rank: ", "<br/>",
                    "Average Cost: ")
    content3<-paste("<b><a href ='www.nyee.edu'> New York Eyes and Ear Infirmary</a><b>","<br/>",
                    "Overall Rank: ","<br/>", 
                    "Average Cost: ")
    content4<-paste("<b><a href ='http://www.nyp.org/'> New York Presbyterian Hospital</a><b>","<br/>",
                    "Overall Rank: ", "<br/>",
                    "Average Cost: ")
    content5<-paste("<b><a href ='https://www.northwell.edu/find-care/locations/lenox-hill-hospital'> Lenox Hill Hospital | Northwell Health</a><b>","<br/>",
                    "Overall Rank: ", "<br/>",
                    "Average Cost: ")
    content6<-paste("<b><a href ='http://www.bethisraelny.org/petrie/'> Mount Sinai Beth Israel - Petrie Division</a><b>","<br/>",
                    "Overall Rank: ", "<br/>",
                    "Average Cost: ")
    content7<-paste("<b><a href ='http://www.nychealthandhospitals.org/metropolitan/'> NYC Health + Hospitals| Metropolitan</a><b>","<br/>",
                    "Overall Rank: ", "<br/>",
                    "Average Cost: ")
    content8<-paste("<b><a href ='http://www.nychealthandhospitals.org/bellevue/'> NYC Health + Hospitals| Bellevue</a><b>","<br/>",
                    "Overall Rank: ", "<br/>",
                    "Average Cost: ")
    content9<-paste("<b><a href ='https://profiles.health.ny.gov/hospital/view/103021'> NYU Hospital Center</a><b>","<br/>",
                    "Overall Rank: ","<br/>",
                    "Average Cost: ")
    content10<-paste("<b><a href ='http://www.nychealthandhospitals.org/harlem/'> Harlem Hospital Center</a><b>","<br/>",
                     "Overall Rank: ", "<br/>",
                     "Average Cost: ")
    content11<-paste("<b><a href ='https://www.hss.edu/why-choose-hss.asp?gclid=COP0hOaFndICFQ5YDQodKWcJ4Q'> Hospital for Special Surgery</a><b>","<br/>",
                     "Overall Rank: ", "<br/>",
                     "Average Cost: ")
    content12<-paste("<b><a href ='https://www.rucares.org/'> The Rockefeller University Hospital</a><b>","<br/>",
                     "Overall Rank: ", "<br/>",
                     "Average Cost: ")
    hospital_content<-c(content1,content2,content3,content4,content5,content6,content7,content8,content9,content10,content11,content12)
    
    rank<-seq(1,nrow(Manhattan_hospital))
    # Plot a default web map (brackets display the result)
    (m <- leaflet() %>% addTiles())
    img <- readPNG("../data/nyc2013_map_results.png")
    grid.raster(img)
    
    # map for the whole new york city
    m %>% setView(lng = -73.835242 , lat =  40.740610, zoom = 10)
    
    # map for Manhattan area
    m%>% setView(lng = -73.9712 , lat =  40.7831, zoom = 12)

      nyc_map<-m%>% setView(lng = -73.9712 , lat =  40.7831, zoom = 12)
      (m2 <- m %>% # popup
          addTiles() %>%
          # add som markers:
          addMarkers(Manhattan_hospital$lng,Manhattan_hospital$lat,popup=hospital_content))
      return(nyc_map)})
  
      output$nyc_map<-renderLeaflet({
        nyc()%>% setView(lng = -73.9712 , lat =  40.7831, zoom = 12)
        (m2 <- m %>% # popup
            addTiles() %>%
            # add som markers:
            addMarkers(Manhattan_hospital$lng,Manhattan_hospital$lat,popup=hospital_content))
   
})
      datasetInput <- reactive({
        switch(as.character(Manhattan_hospital[,c(1,2,3)]),
               "Phone Number" = Phone.Number
               )
               
      })
      output$rawdata <- DT::renderDataTable(DT::datatable(
        Manhattan_hospital,
        options = list(pageLength = 12), rownames = FALSE
      ))
      
      
      
   
})
