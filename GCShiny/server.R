library(ggplot2)
library(dplyr)

source('help_func.R')


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

### shiny part

library(shiny)

shinyServer(function(input, output) {
  
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
  

})
