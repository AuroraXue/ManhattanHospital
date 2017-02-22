url.data <- 'https://data.medicare.gov/data/hospital-compare'
url.sourcecode <- 'dummy url'

info <- list(
  br(),
  h4('NYC Hospital Care Outcomes- ShinyApp'),
  p('About:'),
  br(),
  p('Looking At Mortality and Readmission Rates in NYC Hospitals.'),
  hr(),
  br(),

  tag('ul', list(
    
    tag('li', list('Date', a(href=url.data, url.data))),
    tag('li', list('To Find Under', strong('CSV Flat Files - Revised'))) #,

  )),
  br(),
  p('This Shiny App Was Made For Applied STATGU 5243 At Columbia University'),
  #p('link zu Sourcecode - GitHub:'),
  #p(a(url.sourcecode, href=url.sourcecode)),
  hr(),
  p('Reference', a('Shiny RStudio', href='http://shiny.rstudio.com/'))
)

