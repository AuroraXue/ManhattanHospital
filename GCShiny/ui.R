library(shiny)

source('info.R')
source('help_func.R')

shinyUI(fluidPage(
  
  titlePanel("Quality of Hospital Care In NYC"),
  
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
                
                  
                  tabPanel("Info", info),
                  tabPanel('Table',
                           h3(textOutput('outcome')),
                           tableOutput("filtered")),
                  
                  tabPanel('Plot ggplot', plotOutput('barplot'))
                 
      )
    )
  )
))