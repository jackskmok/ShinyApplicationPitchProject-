#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram

RegressionMat <- function(fit) {
  
  # Fitted vs Residuals
  # For scatter plot smoother
  # Extract fitted values from lm() object
  Fitted.Values <-  fitted(fit)
  
  # Extract residuals from lm() object
  Residuals <-  resid(fit)
  
  # Extract standardized residuals from lm() object
  Standardized.Residuals <- MASS::stdres(fit)  
  
  # Extract fitted values for lm() object
  Theoretical.Quantiles <- qqnorm(Residuals, plot.it = F)$x
  
  # Square root of abs(residuals)
  Root.Residuals <- sqrt(abs(Standardized.Residuals))
  
  # Calculate Leverage
  Leverage <- lm.influence(fit)$hat
  
  # Create data frame 
  # Will be used as input to plot_ly
  
  return(data.frame(Fitted.Values, 
                       Residuals, 
                       Standardized.Residuals, 
                       Theoretical.Quantiles,
                       Root.Residuals,
                       Leverage));
  
}

Plotrvf <- function(Mat) {
  
  LOESS1 <- loess.smooth(Mat$Fitted.Values, Mat$Residuals)
  
  pltrvf <- 
    
    plot_ly(x = Mat$Fitted.Values, y = Mat$Residuals, 
            type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
            marker = list(size = 10, opacity = 0.5), showlegend = F) %>% 
    
    add_trace(x = LOESS1$x, y = LOESS1$y, type = "scatter", mode = "line", name = "Smooth",
              line = list(width = 2)) %>% 
    
    layout(title = "Residuals vs Fitted Values", plot_bgcolor = "#e6e6e6", width = 1000) 
  
  pltrvf
  
  return( pltrvf)
  
}


Plotqq <- function(Mat) {
  
  # QQ Pot
  qq <-  
    plot_ly(x = Mat$Theoretical.Quantiles, y = Mat$Standardized.Residuals, 
            type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
            marker = list(size = 10, opacity = 0.5), showlegend = F) %>% 
    
    add_trace(x = Mat$Theoretical.Quantiles, y = Mat$Theoretical.Quantiles, type = "scatter", mode = "line", name = "",
              line = list(width = 2)) %>% 
    
    layout(title = "Q-Q Plot", plot_bgcolor = "#e6e6e6")
  
  qq
  
  return(qq)
  
}

Plotsl <- function(Mat) {
  
  # Scale Location
  # For scatter plot smoother
  LOESS2 <- loess.smooth(Mat$Fitted.Values, Mat$Root.Residuals)
  
  sl <- 
    plot_ly(x = Mat$Fitted.Values, y = Mat$Root.Residuals, 
            type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
            marker = list(size = 10, opacity = 0.5), showlegend = F) %>% 
    
    add_trace(x = LOESS2$x, y = LOESS2$y, type = "scatter", mode = "line", name = "Smooth",
              line = list(width = 2)) %>% 
    
    layout(title = "Scale Location", plot_bgcolor = "#e6e6e6", width = 1000)
  
  return(sl)
  
}


Plotlr <- function(Mat) {
  

  # Residuals vs Leverage
  # For scatter plot smoother
  LOESS3 <- loess.smooth(Mat$Leverage, Mat$Residuals)
  
  lr <- 
    plot_ly(x = Mat$Leverage, y = Mat$Residuals, 
            type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
            marker = list(size = 10, opacity = 0.5), showlegend = F) %>% 
    
    add_trace(x = LOESS3$x, y = LOESS3$y, type = "scatter", mode = "line", name = "Smooth",
              line = list(width = 2)) %>% 
    
    layout(title = "Leverage vs Residuals", plot_bgcolor = "#e6e6e6")
  
  return(lr)
  
}

shinyServer(function(input, output) {

  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
 
    MPG <- mtcars[which(mtcars$cyl==ifelse(input$show_cyl4,4,0) |
                        mtcars$cyl==ifelse(input$show_cyl6,6,0) |
                        mtcars$cyl==ifelse(input$show_cyl8,8,0)   
                        ),1]
   
    
    MPG[is.na(MPG)] <- 0


    # draw the histogram with the specified number of bins
    ifelse(MPG > 0,
    hist(MPG, breaks = input$bins, col = 'blue', border = 'white', xlab = "Miles/(US) gallon", xlim = c(min(MPG)-5,max(MPG)+5)),
    "")

  })
  
  # Calculate fit
  #
  #fit_disp <- lm(mpg ~ disp,data = mtcars)
  #fit_hp <- lm(mpg ~ hp, data = mtcars)
  #fit_wt <- lm(mpg ~ wt, data = mtcars)
  #fit_all <- lm(mpg ~ disp + hp + wt, data = mtcars)
  
  SelectedFit <- reactive({input$select})
  
output$SelectValue <- renderPrint({
  SelectedFit()
  xx <- isolate(SelectedFit())
  
 if (xx == "Disp") {
   FitEqu <- lm(mpg ~ disp,data = mtcars)
 }
 else
   if (xx == "HP") {
     FitEqu <- lm(mpg ~ hp,data = mtcars)
   }
   else
     if (xx == "Wt") {
       FitEqu <- lm(mpg ~ wt,data = mtcars)
     }
     else
       if (xx == "All") {
           FitEqu <- lm(mpg ~ disp + hp + wt,data = mtcars)
       }

  Mat <- RegressionMat(FitEqu)
  output$plot_rvf = renderPlotly({Plotrvf(Mat)})
  output$plot_qq  = renderPlotly({Plotsl(Mat)})
  output$plot_sl  = renderPlotly({Plotqq(Mat)})
  output$plot_lr  = renderPlotly({Plotlr(Mat)})
})

})
