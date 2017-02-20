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
 

shinyUI(navbarPage("Hospital New York",theme = shinytheme("cyborg"),
                   tabPanel("Introduction",
                            navlistPanel("Introduction",
                                         tabPanel("Introduction",
                                                  includeMarkdown("introduction.md")),
                                         tabPanel("Contact",
                                                  includeMarkdown("contact.md"))
                                         )),
                   tabPanel("Map",
                            titlePanel(h2("Map")),
                          # Sidebar with controls to select the variable to plot against mpg
                         # and to specify whether outliers should be included
                         leafletOutput("nyc_map",height=600)
                            ))
                 #  tabPanel("Hospital Information",titlePanel(h2("Hospital Information")),
                  #          fluidRow(column(12,DT::dataTableOutput('rawdata')
                  #          )
                              
                    #        )
                       #     )
                  )

                   
                            
                