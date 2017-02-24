#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(datasets)
library(ggplot2)
library(shinythemes)
library("leaflet")
library("dplyr")
library("png")
library("grid")
# Define UI for application that draws a histogram
library(shiny)
library(devtools)
# devtools::install_github('ramnathv/rCharts',force = T)
library(rCharts)
library(rjson)
library("googleVis")
library("maps")
suppressPackageStartupMessages(library(googleVis))


years <- c("2008" = "2008",
           "2009" = "2009",
           "2010" = "2010",
           "2011" = "2011",
           "2012" = "2012",
           "2013" = "2013",
           "2014" = "2014")
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
filename<-"hospital_manhattan_basic_info1.csv"
Manhattan_hospital<-read.csv(file = filename,header=T)

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



source('helpers.R',local=T)
library(fields)
source('info.R',local=T)
shinyUI(navbarPage("Hospital New York",theme = shinytheme("cerulean"),
                   
                   tabPanel("Introduction",
                            navlistPanel("Introduction",
                                         tabPanel("info",info),
                                         tabPanel("Contact",
                                                  includeMarkdown("contact.md"))
                            )),
                   tabPanel("Hospital ", titlePanel(h2("Hospital")),
                            sidebarLayout(
                              sidebarPanel(
                                selectInput(inputId = "outcome",
                                            h4("Ranked According To"),
                                            choices = outcomes),
                                sliderInput(inputId = "range",
                                            h4("Ranks"),
                                            min = 1,
                                            max = 100,
                                            value = c(1, 20)),
                                checkboxGroupInput("fields",
                                                   h4("Fields"),
                                                   choices = columns)
                              ),
                              mainPanel(
                                tabsetPanel(type = "tabs", selected = "Info",
                                            tabPanel("Hospital Info", leafletOutput("nyc_map")),
                                            
                                            tabPanel('Table', h3(textOutput('outcome')),
                                                     tableOutput("filtered")),
                                            tabPanel('Plot', plotOutput('barplot')))
                              )
                            )),
                   tabPanel("Spider",titlePanel(h2("Spider")),
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("care",
                                            h4("What you care about"),
                                            choices = colnames(new_complications),
                                            hr())#,
                                          # submitButton("Show value")
                                ),
                              mainPanel( 
                                textOutput("hospital"),
                                plotOutput('radarPlot1'),
                                leafletOutput("hospitalMap")
                              )
                            )
                   ),
                   
                   
                   tabPanel("Compare Your Local Hospitals",titlePanel(h2("Compare Your Local Hospitals")),
                   sidebarLayout(
                     sidebarPanel(
                       h3('Hospital Associated Infection (HAI)'),
                       p('HAIs are among the leading causes of death in the United States. HAIs are largely preventable using widely publicized guidelines and interventions, such as better hygiene and advanced scientifically tested techniques. HAI measure data are collected by the Centers for Disease Control and Prevention (CDC) via the National Healthcare Safety Network (NHSN) tool.'),
                       selectInput("year", "Year", choices = years)#,
                       #submitButton("Show value")
                     ),
                     mainPanel(
                       h5('Data from the Centers for Medicare & Medicaid Services'),
                       p('Smaller values indicate lower rates of infection. Hospitals may be missing due to lack of data or they do not take Medicare or Medicaid patients.'),
                       showOutput('newBar', 'dimple')
                     )
                   ))
                   
))


                   
                   
 
                           
                  


