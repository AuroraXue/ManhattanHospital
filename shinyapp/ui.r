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
                                selectInput(inputId = "outcome",
                                            h3("Ranked According To"),
                                            choices = outcomes),
                                
                                
                                sliderInput(inputId = "range",
                                            h3("Ranks"),
                                            min = 1,
                                            max = 100,
                                            value = c(1, 20)),
                                
                                
                                checkboxGroupInput("fields",
                                                   h4("Fields"),
                                                   choices = columns)
                              ),
                              mainPanel(
                                tabsetPanel(type = "tabs", selected = "Info",
                                            
                                            tabPanel("Hospital",leafletOutput("nyc_map",height=600)),
                                           
                                           
                                            
                                          
                                            tabPanel('radarchart ', plotOutput('barplot')),
                                            tabPanel('Doctor Table',
                                                     h3(textOutput('outcome')),
                                                    tableOutput("filtered")),
                                            tabPanel('Table',
                                                     h3(textOutput('outcome')),
                                                    tableOutput("filtered")),
                                             
                                            tabPanel('Plot ggplot', plotOutput('barplot'))
                                            
                                           
                                            
                                )
                              
                                
                            # Sidebar with controls to select the variable to plot against mpg
                            # and to specify whether outliers should be included
                           # leafletOutput("nyc_map",height=600)
                   )))

             #     tabPanel("Hospital Information",titlePanel(h2("Hospital Information")),
             #            #  tableOutput("view")
              #           fluidRow(
              #             DT::dataTableOutput('rawdata'))
               #       )
             
                             )
                           )
                  


