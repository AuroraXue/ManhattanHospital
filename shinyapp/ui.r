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


                   
                   
 
                           
                  


