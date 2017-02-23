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

source("help_func.R",local=T)
source("HospitalMap.R",local=T)
source("Plot.R",local=T)

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
      df=as.data.frame(rbind(rep(11,10) , rep(0,10) , new_complications[names(which(new_complications[,input$care]==1)),]))
      
      radarchart( df  , axistype=1 , 
                  
                  #custom polygon
                  pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=2 , 
                  
                  #custom the grid
                  cglcol="grey",cglty=1, axislabcol="grey", caxislabels=seq(0,12,3),cglwd=0.2,
                  
                  title = "COMPLICATIONS PERFORMANCE",
                  vlabels=c("Wound Reopen","Accidental Cut","Blood Stream","Collapsed Lung","Death Rate of Complications","Infections","Pressure Sore","Hip/Knee Replacement Complication Rate","Serious Blood Clots","Serious Complications"),
                  #custom labels
                  vlcex=0.4 
      )
      legend(0.9,1.2,
             legend=c("0-3:Best Performance","3-6:Below Average","6-9:Above Average","9-12:Worse Performance"),text.col="black",
             bty = "n",cex = 0.7)
    })

#     
   
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
    
    

  