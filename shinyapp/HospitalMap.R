library("leaflet")
library("dplyr")
library("png")
library("grid")

filename<-"hospital_manhattan_basic_info1.csv"
Manhattan_hospital<-read.csv(file = filename,header=T)

cost<-c()
for (i in 1:nrow(Manhattan_hospital)){
  if (is.na(Manhattan_hospital$Price.Score[i])==FALSE){
    if (Manhattan_hospital$Price.Score[i]<0.965){
      cost[i]<-"Lower than Average"
    }else if(Manhattan_hospital$Price.Score[i]>=0.965 & Manhattan_hospital$Price.Score[i]<=0.9975){
      cost[i]<-"About Average"
    }else{
      cost[i]<-"Above Average"
    }
  }else{
    cost[i]<="NA"
  }
}
Manhattan_hospital<-cbind(Manhattan_hospital,cost)
averageWaitingtime<-read.csv("Average_Waitingtime.csv")
averageWaitingtime<-averageWaitingtime$Average.WaitingTime

content1<-paste("<b><a href ='http://www.mountsinai.org'> Mount Sinai Hospital</a><b>","<br/>",
                "Average Waiting Time: ",averageWaitingtime[1],"<br/>", 
                "Cost Level: ",Manhattan_hospital$cost[1])
content2<-paste("<b><a href ='http://www.mountsinai.org/locations/st-lukes'> Mount Sinai St. Luke's Hospital</a><b>","<br/>",
                "Average Waiting Time: ",averageWaitingtime[2], "<br/>",
                "Cost Level: ",Manhattan_hospital$cost[2])
content3<-paste("<b><a href ='www.nyee.edu'> New York Eyes and Ear Infirmary</a><b>","<br/>",
                "Average Waiting Time: ",averageWaitingtime[3],"<br/>", 
                "Cost Level: ",Manhattan_hospital$cost[3])
content4<-paste("<b><a href ='http://www.nyp.org/'> New York Presbyterian Hospital</a><b>","<br/>",
                "Average Waiting Time: ",averageWaitingtime[4], "<br/>",
                "Cost Level: ",Manhattan_hospital$cost[4])
content5<-paste("<b><a href ='https://www.northwell.edu/find-care/locations/lenox-hill-hospital'> Lenox Hill Hospital | Northwell Health</a><b>","<br/>",
                "Average Waiting Time: ",averageWaitingtime[5], "<br/>",
                "Cost Level: ",Manhattan_hospital$cost[5])
content6<-paste("<b><a href ='http://www.bethisraelny.org/petrie/'> Mount Sinai Beth Israel - Petrie Division</a><b>","<br/>",
                "Average Waiting Time: ",averageWaitingtime[6], "<br/>",
                "Cost Level: ",Manhattan_hospital$cost[6])
content7<-paste("<b><a href ='http://www.nychealthandhospitals.org/metropolitan/'> NYC Health + Hospitals| Metropolitan</a><b>","<br/>",
                "Average Waiting Time: ",averageWaitingtime[7], "<br/>",
                "Cost Level: ",Manhattan_hospital$cost[7])
content8<-paste("<b><a href ='http://www.nychealthandhospitals.org/bellevue/'> NYC Health + Hospitals| Bellevue</a><b>","<br/>",
                "Average Waiting Time: ",averageWaitingtime[8], "<br/>",
                "Cost Level: ",Manhattan_hospital$cost[8])
content9<-paste("<b><a href ='https://profiles.health.ny.gov/hospital/view/103021'> NYU Hospital Center</a><b>","<br/>",
                "Average Waiting Time: ",averageWaitingtime[9],"<br/>",
                "Cost Level: ",Manhattan_hospital$cost[9])
content10<-paste("<b><a href ='http://www.nychealthandhospitals.org/harlem/'> Harlem Hospital Center</a><b>","<br/>",
                 "Average Waiting Time: ",averageWaitingtime[10], "<br/>",
                 "Cost Level: ",Manhattan_hospital$cost[10])
content11<-paste("<b><a href ='https://www.hss.edu/why-choose-hss.asp?gclid=COP0hOaFndICFQ5YDQodKWcJ4Q'> Hospital for Special Surgery</a><b>","<br/>",
                 "Average Waiting Time: ",averageWaitingtime[11], "<br/>",
                 "Cost Level: ",Manhattan_hospital$cost[11])
content12<-paste("<b><a href ='https://www.rucares.org/'> The Rockefeller University Hospital</a><b>","<br/>",
                 "Average Waiting Time: ",averageWaitingtime[12], "<br/>",
                 "Cost Level: ",Manhattan_hospital$cost[12])
hospital_content<-c(content1,content2,content3,content4,content5,content6,content7,content8,content9,content10,content11,content12)

# Plot a default web map (brackets display the result)
(m <- leaflet() %>% addTiles())
img <- readPNG("nyc2013_map_results.png")
grid.raster(img)

# map for the whole new york city
m %>% setView(lng = -73.835242 , lat =  40.740610, zoom = 10)

# map for Manhattan area
m %>% setView(lng = -73.9712 , lat =  40.7831, zoom = 12)
(m2 <- m 
  %>% # popup
    addTiles() %>%
    # add som markers:
    addMarkers(Manhattan_hospital$Longitude,Manhattan_hospital$Latitude,popup=hospital_content))