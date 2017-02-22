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

source('info.R')
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
                                
                                #     Dropdown outcome
                                
                                selectInput(inputId = "outcome",
                                            h4("Ranked According To"),
                                            choices = outcomes),
                                #     Dropdown states
                                #selectInput(inputId = "state",
                                #h4("Select State"),choices = states),  
                                
                                
                                
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
                                         #Radar plot   tabPanel("Radar Plot",leafletOutput())
                                           
                                            
                                             tabPanel('Table',
                                                    h3(textOutput('outcome')),
                                                    tableOutput("filtered")),
                                            
                                            tabPanel('Plot ggplot', plotOutput('barplot'))
                                           
                                            
                                )
                              )
                            
                            
                   )))

             
             
                             )
                           
                  


