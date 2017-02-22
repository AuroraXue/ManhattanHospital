tabPanel("Map", titlePanel(h2("Map")),
                            # Sidebar with controls to select the variable to plot against mpg
                            # and to specify whether outliers should be included
                            leafletOutput("nyc_map",height=600)
                   ),
