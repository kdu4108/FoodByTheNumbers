
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(plotly)


# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(plotly)

shinyUI(navbarPage("", 
                   tabPanel("Plot",
                            sidebarLayout(sidebarPanel(
                              "Upserve Plots", 
                              dateInput('date',
                                                         label = 'Start date: yyyy-mm-dd',
                                                         value = "2017-05-01"
                                                         
                              ),
                              
                              dateInput('date2',
                                        label = 'End date: yyyy-mm-dd',
                                        value = "2017-6-31"
                                        
                              )
                            ), 
                            mainPanel(
                              plotlyOutput("plot3", width="125%"),
                              plotlyOutput("distPlot", width="125%"),
                              plotlyOutput("plot2", width="125%")
                            )
                            )
                   ),
                   tabPanel("Data",
                            sidebarLayout(sidebarPanel(
                              "Upserve Data"
                            ),
                            mainPanel(
                              DT::dataTableOutput("table")
                            )
                            )
                   )
)
)
# 
# shinyUI(fluidPage(
# 
#   # Application title
#   titlePanel("Upserve Data"),
# 
#   column(4, wellPanel(
#     
#     # dateRangeInput('dateRange',
#     #                label = 'Date range input: yyyy-mm-dd',
#     #                start = Sys.Date() - 2, end = Sys.Date() + 2
#     # ),
#     
#     dateInput('date',
#               label = 'Start date: yyyy-mm-dd',
#               value = "2017-05-01"
#               
#     ),
#     
#     dateInput('date2',
#               label = 'End date: yyyy-mm-dd',
#               value = "2017-6-31"
#               
#     )
#   )),
#   
#   column(6,
#          verbatimTextOutput("dateText"),
#          verbatimTextOutput("dateText2"),
#          verbatimTextOutput("dateRangeText"),
#          verbatimTextOutput("dateRangeText2")
#   ),
#   
#     # Show a plot of the generated distribution
#     mainPanel(
#       plotlyOutput("plot3"),
#       plotlyOutput("distPlot"),
#       plotlyOutput("plot2")
#     )
#   )
# )
