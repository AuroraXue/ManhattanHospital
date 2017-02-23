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
library(fmsb)
# Define server logic required to plot various variables against mpg
library(ggplot2)
library("leaflet")
library("dplyr")
library("png")
library("grid")

source('help_func.R')
source("HospitalMap.R")
source("Plot.R")

state_filter <- function(csv_df, state) {
  subset(csv_df, State == 'NY')
}

clean_numeric <- function(csv_df, name) {
  col <- get_col_index(csv_df, name)
  csv_df <- subset(csv_df, grepl('[0-9]', csv_df[,col]))
  csv_df[,col] <- as.numeric(csv_df[,col])
  csv_df
}

order_col <- function(csv_df, name) {
  col <- get_col_index(csv_df, name)
  csv_df <- csv_df[order(csv_df[,col], csv_df$'Hospital Name'),]
  csv_df$Rank <- rank(csv_df[,col], ties.method='min')
  csv_df$Value <- csv_df[,col]
  csv_df
}

# 

col_rank <- function(csv_df, name) {
  
  col <- get_col_index(csv_df, name)
  
  csv_df <- csv_df[order(csv_df[,col], csv_df$'Hospital Name'),]
  
  csv_df$Rank <- rank(csv_df[,col], ties.method='min')
  
  csv_df$Value <- csv_df[,col]
  csv_df
}

apply_params <- function(state, outcome, range) {
  csv_df <- state_filter(csv_df, state)
  csv_df <- clean_numeric(csv_df, outcome)
  csv_df <- order_col(csv_df, outcome)  
  nmin <- range[1]
  nmax <- range[2]
  mid(csv_df, nmin, nmax)
}



complications<-read.csv("Complications_Manhattan.csv")
filename<-"../data/hospital_manhattan_basic_info.csv"
Manhattan_hospital<-as.data.frame(read.csv(file = filename,header=T))

new_complications<-as.data.frame(matrix(NA,ncol=length(levels(complications$Measure.Name)),nrow=length(levels(complications$Hospital.Name))))
colnames(new_complications)<-levels(complications$Measure.Name)
colnames(new_complications)
rownames(new_complications)<-levels(complications$Hospital.Name)
for(i in rownames(new_complications)){
  for(j in colnames(new_complications)){
    new_complications[i,j]=complications$Score[complications$Hospital.Name==i&complications$Measure.Name==j]
  }
}
new_complications<-new_complications[,-4]
new_complications<-apply(new_complications,2,order)
rownames(new_complications)<-levels(complications$Hospital.Name)
averageNYCperformance<-rep(6,ncol(new_complications))
new_complications=rbind(new_complications,averageNYCperformance)


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
      

  ### shiny part
    # hospitalfilter<- reactive({
    #   apply_params(input$colnames(new_complications))
    # })
    # 
    # output$hospitalfilter<-

      
    
    filtered <- reactive({
      apply_params(input$state, input$outcome, input$range)
    })
    
    output$filtered <- renderTable({
      csv_df <- filtered()
      csv_df[,sapply(c('Hospital', 'Rank', 'Value', input$fields), function(name) get_col_index(csv_df, name))]
      
    })
    
    output$outcome <- renderText(input$outcome)
    
    output$barplot <- renderPlot({
      df <- filtered()
      glimpse(df)
      
      df$x = df$Value
      df$y = df$Hospital
      ggplot(df, aes(x=x, y=reorder(y, -x))) +
        geom_point(colour='black') +
        ylab('') + xlab('') +
        ggtitle(input$outcome) +
        geom_segment(aes(yend=y), xend=0, colour="grey50") +
        #theme_gray()
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"),
              legend.position = "none") 
      
      # Cols für Plot
      # $ Hospital                        (chr) "RUSSELLVILLE H.", "CRESTWOOD M.C.", "BAPTIST M.C. EAST", "PRAT...
      # $ Rank                            (int) 1, 2, 3, 3, 5, 6, 7, 8, 9, 10, 11, 11, 13, 14, 14, 14, 17, 18, ...
      # $ Value                           (dbl) 10.1, 10.2, 10.5, 10.5, 10.6, 10.7, 10.8, 11.0, 11.1, 11.2, 11....
    })
    
    output$hospital <- renderText({
      names(which(new_complications[,input$care]==1))
    })
    
    
    output$radarPlot1 <- renderPlot({
      new_complications<-as.data.frame(matrix(NA,ncol=length(levels(complications$Measure.Name)),nrow=length(levels(complications$Hospital.Name))))
      colnames(new_complications)<-levels(complications$Measure.Name)
      colnames(new_complications)
      rownames(new_complications)<-levels(complications$Hospital.Name)
      for(i in rownames(new_complications)){
        for(j in colnames(new_complications)){
          new_complications[i,j]=complications$Score[complications$Hospital.Name==i&complications$Measure.Name==j]
        }
      }
      new_complications<-new_complications[,-4]
      new_complications<-apply(new_complications,2,order)
      rownames(new_complications)<-levels(complications$Hospital.Name)
      averageNYCperformance<-rep(6,ncol(new_complications))
      new_complications=rbind(new_complications,averageNYCperformance)

      BELLEVUE_HOSPITAL_CENTER<-new_complications["BELLEVUE HOSPITAL CENTER",]
      BELLEVUE_HOSPITAL_CENTER=as.data.frame(rbind(rep(11,10) , rep(0,10) , BELLEVUE_HOSPITAL_CENTER))

     radarchart( BELLEVUE_HOSPITAL_CENTER  , axistype=1 ,

                      #custom polygon
                      pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=2 ,

                      #custom the grid
                      cglcol="grey",cglty=1, axislabcol="grey", caxislabels=seq(0,12,3),cglwd=0.2,

                      title = "BELLEVUE HOSPITAL CENTER COMPLICATIONS PERFORMANCE",
                      vlabels=c("Wound Reopen","Accidental Cut","Blood Stream","Collapsed Lung","Death Rate of Complications","Infections","Pressure Sore","Hip/Knee Replacement Complication Rate","Serious Blood Clots","Serious Complications"),
                      #custom labels
                      vlcex=0.4
      )
      legend(0.9,1.2,
             legend=c("0-3:Best Performance","3-6:Below Average","6-9:Above Average","9-12:Worse Performance"),text.col="black",
             bty = "n",cex = 0.7)
    })
    
    output$hospital1 <- renderText({
      paste("Most recommanded hospital BELLEVUE HOSPITAL CENTER")
    })
    # output$radarPlot2 <- renderPlot({
    #   new_complications<-as.data.frame(matrix(NA,ncol=length(levels(complications$Measure.Name)),nrow=length(levels(complications$Hospital.Name))))
    #   colnames(new_complications)<-levels(complications$Measure.Name)
    #   colnames(new_complications)
    #   rownames(new_complications)<-levels(complications$Hospital.Name)
    #   for(i in rownames(new_complications)){
    #     for(j in colnames(new_complications)){
    #       new_complications[i,j]=complications$Score[complications$Hospital.Name==i&complications$Measure.Name==j]
    #     }
    #   }
    #   new_complications<-new_complications[,-4]
    #   new_complications<-apply(new_complications,2,order)
    #   rownames(new_complications)<-levels(complications$Hospital.Name)
    #   averageNYCperformance<-rep(6,ncol(new_complications))
    #   new_complications=rbind(new_complications,averageNYCperformance)
    #   
    #   #Spider for HARLEM HOSPITAL CENTER
    #   HARLEM_HOSPITAL_CENTER<-new_complications["HARLEM HOSPITAL CENTER",]
    #   HARLEM_HOSPITAL_CENTER=as.data.frame(rbind(rep(11,10) , rep(0,10) , HARLEM_HOSPITAL_CENTER))
    #   radarchart( HARLEM_HOSPITAL_CENTER  , axistype=1 , 
    #               
    #               #custom polygon
    #               pcol=rgb(0.9,0.6,0.5,0.9) , pfcol=rgb(0.7,0.5,0.5,0.5) , plwd=2 , 
    #               
    #               #custom the grid
    #               cglcol="grey",cglty=1, axislabcol="grey", caxislabels=seq(0,12,3),cglwd=0.2,
    #               
    #               title = "HARLEM HOSPITAL CENTER COMPLICATIONS PERFORMANCE",
    #               
    #               vlabels=c("Wound Reopen","Accidental Cut","Blood Stream","Collapsed Lung","Death Rate of Complications","Infections","Pressure Sore","Hip/Knee Replacement Complication Rate","Serious Blood Clots","Serious Complications"),
    #               
    #               #custom labels
    #               vlcex=0.4 
    #   )
    #   legend(0.9,1.2,
    #          legend=c("0-3: Best Performance","3-6: Below Average","6-9: Above Average","9-12: Worse Performance"),text.col="black",
    #          bty = "n",cex = 0.7)
    #   
    # })
    # 
    output$hospital2 <- renderText({
      paste("Most recommanded hospital HARLEM HOSPITAL CENTER")
    })
    
    
    # output$radarPlot3 <- renderPlot({
    #   new_complications<-as.data.frame(matrix(NA,ncol=length(levels(complications$Measure.Name)),nrow=length(levels(complications$Hospital.Name))))
    #   colnames(new_complications)<-levels(complications$Measure.Name)
    #   colnames(new_complications)
    #   rownames(new_complications)<-levels(complications$Hospital.Name)
    #   for(i in rownames(new_complications)){
    #     for(j in colnames(new_complications)){
    #       new_complications[i,j]=complications$Score[complications$Hospital.Name==i&complications$Measure.Name==j]
    #     }
    #   }
    #   new_complications<-new_complications[,-4]
    #   new_complications<-apply(new_complications,2,order)
    #   rownames(new_complications)<-levels(complications$Hospital.Name)
    #   averageNYCperformance<-rep(6,ncol(new_complications))
    #   new_complications=rbind(new_complications,averageNYCperformance)
    #   
    #   HOSPITAL_FOR_SPECIAL_SURGERY<-new_complications["HOSPITAL FOR SPECIAL SURGERY",]
    #   HOSPITAL_FOR_SPECIAL_SURGERY=as.data.frame(rbind(rep(11,10) , rep(0,10) , HOSPITAL_FOR_SPECIAL_SURGERY))
    #   radarchart( HOSPITAL_FOR_SPECIAL_SURGERY  , axistype=1 , 
    #               
    #               #custom polygon
    #               pcol=rgb(0.2,0.3,0.5,0.9) , pfcol=rgb(0.2,0.2,0.5,0.5) , plwd=2 , 
    #               
    #               #custom the grid
    #               cglcol="grey",cglty=1, axislabcol="grey", caxislabels=seq(0,12,3),cglwd=0.2,
    #               
    #               title = "HOSPITAL_FOR_SPECIAL_SURGERY COMPLICATIONS PERFORMANCE",
    #               
    #               vlabels=c("Wound Reopen","Accidental Cut","Blood Stream","Collapsed Lung","Death Rate of Complications","Infections","Pressure Sore","Hip/Knee Replacement Complication Rate","Serious Blood Clots","Serious Complications"),
    #               
    #               #custom labels
    #               vlcex=0.4 
    #   )
    #   legend(0.9,1.2,
    #          legend=c("0-3: Best Performance","3-6: Below Average","6-9: Above Average","9-12: Worse Performance"),text.col="black",
    #          bty = "n",cex = 0.7)
    #   
    # })
    output$hospital3 <- renderText({
    paste("Most recommanded hospital HOSPITAL_FOR_SPECIAL_SURGERY")
})
    # output$radarPlot4 <- renderPlot({
    #   new_complications<-as.data.frame(matrix(NA,ncol=length(levels(complications$Measure.Name)),nrow=length(levels(complications$Hospital.Name))))
    #   colnames(new_complications)<-levels(complications$Measure.Name)
    #   colnames(new_complications)
    #   rownames(new_complications)<-levels(complications$Hospital.Name)
    #   for(i in rownames(new_complications)){
    #     for(j in colnames(new_complications)){
    #       new_complications[i,j]=complications$Score[complications$Hospital.Name==i&complications$Measure.Name==j]
    #     }
    #   }
    #   new_complications<-new_complications[,-4]
    #   new_complications<-apply(new_complications,2,order)
    #   rownames(new_complications)<-levels(complications$Hospital.Name)
    #   averageNYCperformance<-rep(6,ncol(new_complications))
    #   new_complications=rbind(new_complications,averageNYCperformance)
    #   
    #   LENOX_HILL_HOSPITAL<-new_complications["LENOX HILL HOSPITAL",]
    #   LENOX_HILL_HOSPITAL=as.data.frame(rbind(rep(11,10) , rep(0,10) , LENOX_HILL_HOSPITAL))
    #   radarchart( LENOX_HILL_HOSPITAL , axistype=1 , 
    #               
    #               #custom polygon
    #               pcol=rgb(0.9,0.8,0.5,0.9) , pfcol=rgb(0.9,0.8,0.6,0.5) , plwd=2 , 
    #               
    #               #custom the grid
    #               cglcol="grey",cglty=1, axislabcol="grey", caxislabels=seq(0,12,3),cglwd=0.2,
    #               
    #               title = "LENOX HILL HOSPITAL COMPLICATIONS PERFORMANCE",
    #               
    #               vlabels=c("Wound Reopen","Accidental Cut","Blood Stream","Collapsed Lung","Death Rate of Complications","Infections","Pressure Sore","Hip/Knee Replacement Complication Rate","Serious Blood Clots","Serious Complications"),
    #               
    #               #custom labels
    #               vlcex=0.4 
    #   )
    #   legend(0.9,1.2,
    #          legend=c("0-3: Best Performance","3-6: Below Average","6-9: Above Average","9-12: Worse Performance"),text.col="black",
    #          bty = "n",cex = 0.7)
    #   
    # })
    # 
    
    output$hospital4 <- renderText({
      paste("Most recommanded hospital LENOX HILL HOSPITAL")
    })
    
    
    
    # output$radarPlot5 <- renderPlot({
    #   new_complications<-as.data.frame(matrix(NA,ncol=length(levels(complications$Measure.Name)),nrow=length(levels(complications$Hospital.Name))))
    #   colnames(new_complications)<-levels(complications$Measure.Name)
    #   colnames(new_complications)
    #   rownames(new_complications)<-levels(complications$Hospital.Name)
    #   for(i in rownames(new_complications)){
    #     for(j in colnames(new_complications)){
    #       new_complications[i,j]=complications$Score[complications$Hospital.Name==i&complications$Measure.Name==j]
    #     }
    #   }
    #   new_complications<-new_complications[,-4]
    #   new_complications<-apply(new_complications,2,order)
    #   rownames(new_complications)<-levels(complications$Hospital.Name)
    #   averageNYCperformance<-rep(6,ncol(new_complications))
    #   new_complications=rbind(new_complications,averageNYCperformance)
    #   
    #   # Spider for METROPOLITAN HOSPITAL CENTER
    #   METROPOLITAN_HOSPITAL_CENTER<-new_complications["METROPOLITAN HOSPITAL CENTER",]
    #   METROPOLITAN_HOSPITAL_CENTER=as.data.frame(rbind(rep(11,10) , rep(0,10) , METROPOLITAN_HOSPITAL_CENTER))
    #   radarchart( METROPOLITAN_HOSPITAL_CENTER , axistype=1 , 
    #               
    #               #custom polygon
    #               pcol=rgb(0.9,0.6,1,0.9) , pfcol=rgb(0.9,0.8,0.9,0.5) , plwd=2 , 
    #               
    #               #custom the grid
    #               cglcol="grey",cglty=1, axislabcol="grey", caxislabels=seq(0,12,3),cglwd=0.2,
    #               
    #               title = "METROPOLITAN HOSPITAL CENTER COMPLICATIONS PERFORMANCE",
    #               
    #               vlabels=c("Wound Reopen","Accidental Cut","Blood Stream","Collapsed Lung","Death Rate of Complications","Infections","Pressure Sore","Hip/Knee Replacement Complication Rate","Serious Blood Clots","Serious Complications"),
    #               
    #               #custom labels
    #               vlcex=0.4 
    #   )
    #   legend(0.9,1.2,
    #          legend=c("0-3: Best Performance","3-6: Below Average","6-9: Above Average","9-12: Worse Performance"),text.col="black",
    #          bty = "n",cex = 0.7)
    #   
    # })
    # 
    
    
    output$hospital5 <- renderText({
      paste("Most recommanded hospital METROPOLITAN HOSPITAL CENTER")
    })
    
    
    # output$radarPlot6 <- renderPlot({
    #   new_complications<-as.data.frame(matrix(NA,ncol=length(levels(complications$Measure.Name)),nrow=length(levels(complications$Hospital.Name))))
    #   colnames(new_complications)<-levels(complications$Measure.Name)
    #   colnames(new_complications)
    #   rownames(new_complications)<-levels(complications$Hospital.Name)
    #   for(i in rownames(new_complications)){
    #     for(j in colnames(new_complications)){
    #       new_complications[i,j]=complications$Score[complications$Hospital.Name==i&complications$Measure.Name==j]
    #     }
    #   }
    #   new_complications<-new_complications[,-4]
    #   new_complications<-apply(new_complications,2,order)
    #   rownames(new_complications)<-levels(complications$Hospital.Name)
    #   averageNYCperformance<-rep(6,ncol(new_complications))
    #   new_complications=rbind(new_complications,averageNYCperformance)
    #   
    #   # Spider for MOUNT SINAI BETH ISRAEL/PETRIE CAMPUS
    #   MOUNT_SINAI_BETH_ISRAELPETRIE_CAMPUS<-new_complications["MOUNT SINAI BETH ISRAEL/PETRIE CAMPUS",]
    #   MOUNT_SINAI_BETH_ISRAELPETRIE_CAMPUS=as.data.frame(rbind(rep(11,10) , rep(0,10) , MOUNT_SINAI_BETH_ISRAELPETRIE_CAMPUS))
    #   radarchart( MOUNT_SINAI_BETH_ISRAELPETRIE_CAMPUS , axistype=1 , 
    #               
    #               #custom polygon
    #               pcol=rgb(0.6,0.2,0.4,0.9) , pfcol=rgb(0.6,0.2,0.2,0.5) , plwd=2 , 
    #               
    #               #custom the grid
    #               cglcol="grey",cglty=1, axislabcol="grey", caxislabels=seq(0,12,3),cglwd=0.2,
    #               
    #               title = "MOUNT_SINAI_BETH_ISRAELPETRIE_CAMPUS COMPLICATIONS PERFORMANCE",
    #               
    #               vlabels=c("Wound Reopen","Accidental Cut","Blood Stream","Collapsed Lung","Death Rate of Complications","Infections","Pressure Sore","Hip/Knee Replacement Complication Rate","Serious Blood Clots","Serious Complications"),
    #               
    #               #custom labels
    #               vlcex=0.4 
    #   )
    #   legend(0.9,1.2,
    #          legend=c("0-3: Best Performance","3-6: Below Average","6-9: Above Average","9-12: Worse Performance"),text.col="black",
    #          bty = "n",cex = 0.7)
    #   
    #   
    # })
    #
    
    output$hospital6 <- renderText({
      paste("Most recommanded hospital MOUNT_SINAI_BETH_ISRAELPETRIE_CAMPUS")
    })
    # output$radarPlot7 <- renderPlot({
    #   new_complications<-as.data.frame(matrix(NA,ncol=length(levels(complications$Measure.Name)),nrow=length(levels(complications$Hospital.Name))))
    #   colnames(new_complications)<-levels(complications$Measure.Name)
    #   colnames(new_complications)
    #   rownames(new_complications)<-levels(complications$Hospital.Name)
    #   for(i in rownames(new_complications)){
    #     for(j in colnames(new_complications)){
    #       new_complications[i,j]=complications$Score[complications$Hospital.Name==i&complications$Measure.Name==j]
    #     }
    #   }
    #   new_complications<-new_complications[,-4]
    #   new_complications<-apply(new_complications,2,order)
    #   rownames(new_complications)<-levels(complications$Hospital.Name)
    #   averageNYCperformance<-rep(6,ncol(new_complications))
    #   new_complications=rbind(new_complications,averageNYCperformance)
    #   
    #   # Spider for MOUNT SINAI HOSPITAL
    #   MOUNT_SINAI_HOSPITAL<-new_complications["MOUNT SINAI HOSPITAL",]
    #   MOUNT_SINAI_HOSPITAL=as.data.frame(rbind(rep(11,10) , rep(0,10) , MOUNT_SINAI_HOSPITAL))
    #   radarchart( MOUNT_SINAI_HOSPITAL , axistype=1 , 
    #               
    #               #custom polygon
    #               pcol=rgb(0.5,0.6,1,0.9) , pfcol=rgb(0.5,0.8,1,0.5) , plwd=2 , 
    #               
    #               #custom the grid
    #               cglcol="grey",cglty=1, axislabcol="grey", caxislabels=seq(0,12,3),cglwd=0.2,
    #               
    #               title = "MOUNT SINAI HOSPITAL COMPLICATIONS PERFORMANCE",
    #               
    #               vlabels=c("Wound Reopen","Accidental Cut","Blood Stream","Collapsed Lung","Death Rate of Complications","Infections","Pressure Sore","Hip/Knee Replacement Complication Rate","Serious Blood Clots","Serious Complications"),
    #               
    #               #custom labels
    #               vlcex=0.4 
    #   )
    #   legend(0.9,1.2,
    #          legend=c("0-3: Best Performance","3-6: Below Average","6-9: Above Average","9-12: Worse Performance"),text.col="black",
    #          bty = "n",cex = 0.7)
    #   
    # })
    output$hospital7 <- renderText({
      paste("Most recommanded hospital MOUNT SINAI HOSPITAL")
    }) 
    # output$radarPlot8 <- renderPlot({
    #   new_complications<-as.data.frame(matrix(NA,ncol=length(levels(complications$Measure.Name)),nrow=length(levels(complications$Hospital.Name))))
    #   colnames(new_complications)<-levels(complications$Measure.Name)
    #   colnames(new_complications)
    #   rownames(new_complications)<-levels(complications$Hospital.Name)
    #   for(i in rownames(new_complications)){
    #     for(j in colnames(new_complications)){
    #       new_complications[i,j]=complications$Score[complications$Hospital.Name==i&complications$Measure.Name==j]
    #     }
    #   }
    #   new_complications<-new_complications[,-4]
    #   new_complications<-apply(new_complications,2,order)
    #   rownames(new_complications)<-levels(complications$Hospital.Name)
    #   averageNYCperformance<-rep(6,ncol(new_complications))
    #   new_complications=rbind(new_complications,averageNYCperformance)
    #   
    #   # Spider for N Y EYE AND EAR INFIRMARY
    #   EYE_EAR_INFIRMARY<-new_complications["N Y EYE AND EAR INFIRMARY",]
    #   EYE_EAR_INFIRMARY=as.data.frame(rbind(rep(11,10) , rep(0,10) , EYE_EAR_INFIRMARY))
    #   radarchart( EYE_EAR_INFIRMARY , axistype=1 , 
    #               
    #               #custom polygon
    #               pcol=rgb(0.1,0,0.9,0.9) , pfcol=rgb(0.1,0.2,1,0.5) , plwd=2 , 
    #               
    #               #custom the grid
    #               cglcol="grey",cglty=1, axislabcol="grey", caxislabels=seq(0,12,3),cglwd=0.2,
    #               
    #               title = "New York EYE & EAR INFIRMARY COMPLICATIONS PERFORMANCE",
    #               
    #               vlabels=c("Wound Reopen","Accidental Cut","Blood Stream","Collapsed Lung","Death Rate of Complications","Infections","Pressure Sore","Hip/Knee Replacement Complication Rate","Serious Blood Clots","Serious Complications"),
    #               
    #               #custom labels
    #               vlcex=0.4 
    #   )
    #   legend(0.9,1.2,
    #          legend=c("0-3: Best Performance","3-6: Below Average","6-9: Above Average","9-12: Worse Performance"),text.col="black",
    #          bty = "n",cex = 0.7)
    #   
    # })
    
    output$hospital8 <- renderText({
    paste("Most recommanded hospital New York EYE & EAR INFIRMARY")
})  
    # output$radarPlot9 <- renderPlot({
    #   new_complications<-as.data.frame(matrix(NA,ncol=length(levels(complications$Measure.Name)),nrow=length(levels(complications$Hospital.Name))))
    #   colnames(new_complications)<-levels(complications$Measure.Name)
    #   colnames(new_complications)
    #   rownames(new_complications)<-levels(complications$Hospital.Name)
    #   for(i in rownames(new_complications)){
    #     for(j in colnames(new_complications)){
    #       new_complications[i,j]=complications$Score[complications$Hospital.Name==i&complications$Measure.Name==j]
    #     }
    #   }
    #   new_complications<-new_complications[,-4]
    #   new_complications<-apply(new_complications,2,order)
    #   rownames(new_complications)<-levels(complications$Hospital.Name)
    #   averageNYCperformance<-rep(6,ncol(new_complications))
    #   new_complications=rbind(new_complications,averageNYCperformance)
    #   
    #   # Spider for NEW YORK-PRESBYTERIAN HOSPITAL
    #   NEW_YORK_PRESBYTERIAN_HOSPITAL<-new_complications["NEW YORK-PRESBYTERIAN HOSPITAL",]
    #   NEW_YORK_PRESBYTERIAN_HOSPITAL=as.data.frame(rbind(rep(11,10) , rep(0,10) , NEW_YORK_PRESBYTERIAN_HOSPITAL))
    #   radarchart( NEW_YORK_PRESBYTERIAN_HOSPITAL , axistype=1 , 
    #               
    #               #custom polygon
    #               pcol=rgb(0.1,0,0.1,0.9) , pfcol=rgb(0.1,0,0.3,0.5) , plwd=2 , 
    #               
    #               #custom the grid
    #               cglcol="grey",cglty=1, axislabcol="grey", caxislabels=seq(0,12,3),cglwd=0.2,
    #               
    #               title = "NEW YORK PRESBYTERIAN HOSPITAL COMPLICATIONS PERFORMANCE",
    #               
    #               vlabels=c("Wound Reopen","Accidental Cut","Blood Stream","Collapsed Lung","Death Rate of Complications","Infections","Pressure Sore","Hip/Knee Replacement Complication Rate","Serious Blood Clots","Serious Complications"),
    #               
    #               #custom labels
    #               vlcex=0.4 
    #   )
    #   legend(0.9,1.2,
    #          legend=c("0-3: Best Performance","3-6: Below Average","6-9: Above Average","9-12: Worse Performance"),text.col="black",
    #          bty = "n",cex = 0.7)
    #   
    #   
    # })
    output$hospital9 <- renderText({
      paste("Most recommanded hospital NEW YORK PRESBYTERIAN HOSPITAL")
    })  
    
    
     
    # output$radarPlot10 <- renderPlot({
    #   new_complications<-as.data.frame(matrix(NA,ncol=length(levels(complications$Measure.Name)),nrow=length(levels(complications$Hospital.Name))))
    #   colnames(new_complications)<-levels(complications$Measure.Name)
    #   colnames(new_complications)
    #   rownames(new_complications)<-levels(complications$Hospital.Name)
    #   for(i in rownames(new_complications)){
    #     for(j in colnames(new_complications)){
    #       new_complications[i,j]=complications$Score[complications$Hospital.Name==i&complications$Measure.Name==j]
    #     }
    #   }
    #   new_complications<-new_complications[,-4]
    #   new_complications<-apply(new_complications,2,order)
    #   rownames(new_complications)<-levels(complications$Hospital.Name)
    #   averageNYCperformance<-rep(6,ncol(new_complications))
    #   new_complications=rbind(new_complications,averageNYCperformance)
    #   
    #   # Spider for NYU HOSPITALS CENTER
    #   NYU_HOSPITALS_CENTER<-new_complications["NYU HOSPITALS CENTER",]
    #   NYU_HOSPITALS_CENTER=as.data.frame(rbind(rep(11,10) , rep(0,10) , NYU_HOSPITALS_CENTER))
    #   radarchart(NYU_HOSPITALS_CENTER, axistype=1 , 
    #              
    #              #custom polygon
    #              pcol=rgb(0,0.4,0.3,0.9) , pfcol=rgb(0,0.1,0.4,0.5) , plwd=2 , 
    #              
    #              #custom the grid
    #              cglcol="grey",cglty=1, axislabcol="grey", caxislabels=seq(0,12,3),cglwd=0.2,
    #              
    #              title = "NYU HOSPITALS CENTER COMPLICATIONS PERFORMANCE",
    #              
    #              vlabels=c("Wound Reopen","Accidental Cut","Blood Stream","Collapsed Lung","Death Rate of Complications","Infections","Pressure Sore","Hip/Knee Replacement Complication Rate","Serious Blood Clots","Serious Complications"),
    #              
    #              #custom labels
    #              vlcex=0.4 
    #   )
    #   legend(0.9,1.2,
    #          legend=c("0-3: Best Performance","3-6: Below Average","6-9: Above Average","9-12: Worse Performance"),text.col="black",
    #          bty = "n",cex = 0.7)
    #   
    #   
    # })
    output$hospital10 <- renderText({
      paste("Most recommanded hospital NYU HOSPITALS CENTER")
    })  
    
     
    # output$radarPlot11 <- renderPlot({
    #   new_complications<-as.data.frame(matrix(NA,ncol=length(levels(complications$Measure.Name)),nrow=length(levels(complications$Hospital.Name))))
    #   colnames(new_complications)<-levels(complications$Measure.Name)
    #   colnames(new_complications)
    #   rownames(new_complications)<-levels(complications$Hospital.Name)
    #   for(i in rownames(new_complications)){
    #     for(j in colnames(new_complications)){
    #       new_complications[i,j]=complications$Score[complications$Hospital.Name==i&complications$Measure.Name==j]
    #     }
    #   }
    #   new_complications<-new_complications[,-4]
    #   new_complications<-apply(new_complications,2,order)
    #   rownames(new_complications)<-levels(complications$Hospital.Name)
    #   averageNYCperformance<-rep(6,ncol(new_complications))
    #   new_complications=rbind(new_complications,averageNYCperformance)
    #   
    #   # Spider for ST LUKE'S ROOSEVELT HOSPITAL
    #   ST_LUKE_ROOSEVELT_HOSPITAL<-new_complications["ST LUKE'S ROOSEVELT HOSPITAL",]
    #   ST_LUKE_ROOSEVELT_HOSPITAL=as.data.frame(rbind(rep(11,10) , rep(0,10) , ST_LUKE_ROOSEVELT_HOSPITAL))
    #   radarchart(ST_LUKE_ROOSEVELT_HOSPITAL, axistype=1 , 
    #              
    #              #custom polygon
    #              pcol=rgb(0.5,0.1,0.6,0.9) , pfcol=rgb(0.5,0,0.1,0.5) , plwd=2 , 
    #              
    #              #custom the grid
    #              cglcol="grey",cglty=1, axislabcol="grey", caxislabels=seq(0,12,3),cglwd=0.2,
    #              
    #              title = "ST LUKE'S ROOSEVELT HOSPITAL COMPLICATIONS PERFORMANCE",
    #              
    #              vlabels=c("Wound Reopen","Accidental Cut","Blood Stream","Collapsed Lung","Death Rate of Complications","Infections","Pressure Sore","Hip/Knee Replacement Complication Rate","Serious Blood Clots","Serious Complications"),
    #              
    #              #custom labels
    #              vlcex=0.4 
    #   )
    #   
    #   legend(0.9,1.2,
    #          legend=c("0-3: Best Performance","3-6: Below Average","6-9: Above Average","9-12: Worse Performance"),text.col="black",
    #          bty = "n",cex = 0.7)
    #   
    # })
    
    output$hospital11 <- renderText({
      paste("Most recommanded hospital ST LUKE'S ROOSEVELT HOSPITAL")
    }) 
    
   
    datasetInput <- reactive({
      switch(input$new_complications,
             "A wound that splits open  after surgery on the abdomen or pelvis" = ,
             "Accidental cuts and tears from medical treatment" =  ,
             "Blood stream infection after surgery" = ,
             "Collapsed lung due to medical treatment" = ,
             "Deaths among Patients with Serious Treatable Complications after Surgery" = ,
             "Infections from a large venous catheter" = ,
             "Pressure sores" = ,
             "Rate of complications for hip/knee replacement patients" = ,
             "Serious blood clots after surgery" = ,
             "Serious complications" = )
    })
    
   
  })
    
    

  