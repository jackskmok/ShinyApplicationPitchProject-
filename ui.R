#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("The 1974 Motor Trend"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("bins",
                   "Miles/(US) gallon Divided by different bins",
                   max = 10,
                   min = 1,
                   value = 5),
       checkboxInput("show_cyl4","Cylinder 4", value = TRUE),
       checkboxInput("show_cyl6","Cylinder 6", value = TRUE),
       checkboxInput("show_cyl8","Cylinder 8", value = TRUE),
       selectInput(inputId="select", label = h3("Regression Parameter"), 
                   c("Disp",'HP','Wt','All'),
                   selected = 1)
    ),
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot"),
       textOutput("SelectValue"),

       fluidRow(
         splitLayout(cellWidths = c("50%", "50%"), 
                     plotlyOutput("plot_rvf"),
                     plotlyOutput("plot_sl")
         ),
         splitLayout(cellWidths = c("50%", "50%"), 
                     plotlyOutput("plot_qq"),
                     plotlyOutput("plot_lr")
                     )
)
       )
    )
  )
)
