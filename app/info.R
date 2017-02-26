url.data <- 'https://data.medicare.gov/data/hospital-compare'
url.sourcecode <- 'dummy url'

info <- list(
br(),
h4('QUALICARE'),
p('This app pay attention to patient safety measure for the hospital quality, based on HAI(Hospital Associated Infection), mortality estimate and main complications. Provide information, like rank and value plot about the predict mortality in last 30 days. Besides, give the radar plot about main complication may happen in the hospital. And make recommendation about the hospital based on customer\'s concern. For the third part, this app provides the visual bar plot about the history data of HAI to help customers to better decide which hospital to go.
'),
img(src="name.png", height = 200, width = 300, align = "middle"),
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

