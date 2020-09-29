#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(zoo)
library(ggthemes)

# Load data ----
library(readr)

# county_data <- read_csv("~/SYE_Fall2020/data/us_counties_covid19_daily.csv")
county_data <- read_csv("~/SYE_Fall2020/data/us-counties.csv")
state_data <- read_csv("~/SYE_Fall2020/data/us-states.csv")

countyNames <- read_csv("~/SYE_Fall2020/data/countyNames.csv")
tryStates = c("California", "New York", "New Jersey")

# allStates = countyNames[, c("state")]
# allStates = unique(allStates)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
    
    stateName <- reactive({
      input$selectState
    })
    

    # variable name for the chosen county is "chooseCounty"
    output$selectCounty <- renderUI({
      selectInput("chooseCounty", h3("Select a county"), 
                  choices = c("Overall State" = "allState", subset(countyNames, state == stateName(), c("county"))))
    })
    
    countyName <- reactive({
      input$chooseCounty
    })
    
    # chosenData <-reactive({
    #   subset(county_data,state == stateName() & county == countyName())
    # })
    
    
    output$statePlot = renderPlot({
      if (countyName() == "allState"){
        plotData = subset(state_data,state == stateName())
      }
      else{
        plotData = subset(county_data,state == stateName() & county == countyName())
      }
      
      # adding "newcases" variable in case the user wants to render new cases plot
      plotData <- plotData %>%
        mutate(newcases = c(NA, diff(cases)))
      
      # adding "7 day rolling average" variable
      plotData <- plotData %>%
        mutate(case_7days = rollmean(cases, k = 7, fill = NA))
      
      
      # View(plotData)
      
      
      chooseCumulative <- input$selectTrend == "Cumulative Cases"
      chooseNew <- input$selectTrend == "New Cases"
      chooseRollingAverage <- input$selectTrend == "7-day Rolling Average"
      
    
      # getting cumulative data
      if(chooseCumulative){
      
        my_plot <- ggplot(plotData) +
          geom_line(mapping = aes(x = date, y = cases), size=1.5) +
          
          labs (y = "Positive cases", x = "Date")+
          xlim(input$dates[1], input$dates[2])
      }
      # getting new cases plot
      else if (chooseNew){
        my_plot <- ggplot(plotData) +
          geom_line(mapping = aes(x = date, y = newcases), size=1.5) +
          
          labs (y = "New cases", x = "Date")+
          xlim(input$dates[1], input$dates[2])
      }
      # getting rolling average plot
      else if (chooseRollingAverage){
        my_plot <- ggplot(plotData) +
          geom_line(mapping = aes(x = date, y = case_7days), size=1.5) +
          
          labs (y = "7-day Rolling average", x = "Date")+
          xlim(input$dates[1], input$dates[2])
      }
      
      
      
      slr <- "Simple linear regression" %in% input$chooseModel
      quad <- "Quadratic regression" %in% input$chooseModel
      cub <- "Cubic regression" %in% input$chooseModel
      
      
      # create a new data frame
      new <- data.frame(date = seq(input$dates[1], input$dates[2], by = "day"))
      
      
      if(slr){
        if(chooseCumulative){
          # simple linear model and CUMULATIVE CASES
          slr_ds <- lm(cases ~ as.Date(date), data = subset(plotData, as.Date(date) < input$futureDate))
        }else if(chooseNew){
          # simple linear model and NEW CASES
          slr_ds <- lm(newcases ~ as.Date(date), data = subset(plotData, as.Date(date) < input$futureDate))
        }
        pred.slr <- predict(slr_ds, new)
        
        pred.slrLoCI <- predict(slr_ds, new, 
                                interval = "confidence", 
                                level = 0.9)[, 2]
        pred.slrHiCI <- predict(slr_ds, new, 
                                interval = "confidence", 
                                level = 0.9)[, 3]
        
        new <- new %>%
          mutate(slr = pred.slr,
                 slrLoCI = pred.slrLoCI,
                 slrHiCI = pred.slrHiCI)
        
  
        my_plot <- 
          my_plot + 
          geom_line(data = new, aes(x = date, y = slr), colour = "steelblue", 
                    size=1.5, linetype = "dashed")+
          geom_ribbon(data = new, aes(x = date, ymin = slrLoCI, ymax = slrHiCI), 
                      fill = "olivedrab", alpha = 0.4, stat = "identity")
      }

      
      if(quad){
        if(chooseCumulative){
          # qudratic model and CUMULATIVE CASES
          quad_ds <- lm(cases ~ as.Date(date) + I(time(date) ^ 2), 
                        data = subset(plotData, as.Date(date) < input$futureDate))
        }else if(chooseNew){
          # qudratic model and NEW CASES
          quad_ds <- lm(newcases ~ as.Date(date) + I(time(date) ^ 2), 
                        data = subset(plotData, as.Date(date) < input$futureDate))
        }
        pred.quad <- predict(quad_ds, new)
        
        pred.quadLoCI <- predict(quad_ds, new, 
                                interval = "confidence", 
                                level = 0.9)[, 2]
        pred.quadHiCI <- predict(quad_ds, new, 
                                interval = "confidence", 
                                level = 0.9)[, 3]
        new <- new %>%
          mutate(quad = pred.quad,
                 quadLoCI = pred.quadLoCI,
                 quadHiCI = pred.quadHiCI)
        
        my_plot <- 
          my_plot + 
          geom_line(data = new, aes(x = date, y = quad), colour = "darkorange", 
                                       size=1.5, linetype = "dashed")+
          geom_ribbon(data = new, aes(x = date, ymin = quadLoCI, ymax = quadHiCI), 
                      fill = "hotpink2", alpha = 0.4, stat = "identity")
      }
      
       
      if(cub){
        if(chooseCumulative){
          # cubic model and CUMULATIVE CASES
          cubic_ds <- lm(cases ~ as.Date(date) + I(time(date) ^ 2) + I(time(date) ^ 3), 
                         data = subset(plotData, as.Date(date) < input$futureDate))
        }else if(chooseNew){
          # cubic model and NEW CASES
          cubic_ds <- lm(newcases ~ as.Date(date) + I(time(date) ^ 2) + I(time(date) ^ 3), 
                         data = subset(plotData, as.Date(date) < input$futureDate))
        }
        
        pred.cubic <- predict(cubic_ds, new)
        pred.cubicLoCI <- predict(cubic_ds, new, 
                                 interval = "confidence", 
                                 level = 0.9)[, 2]
        pred.cubicHiCI <- predict(cubic_ds, new, 
                                 interval = "confidence", 
                                 level = 0.9)[, 3]
    
        new <- new %>%
          mutate(cubic = pred.cubic,
                 cubicLoCI = pred.cubicLoCI,
                 cubicHiCI = pred.cubicHiCI)
        
        my_plot <- 
          my_plot + 
          geom_line(data = new, aes(x = date, y = cubic), colour = "royalblue", 
                    size=1.5, linetype = "dashed")+
          geom_ribbon(data = new, aes(x = date, ymin = cubicLoCI, ymax = cubicHiCI), 
                      fill = "salmon", alpha = 0.4, stat = "identity")
      }
      
      
      # View(new)
      
      # add vertical line
      my_plot <- my_plot +
        annotate("segment", colour = "purple", 
                 x = as.Date(input$futureDate), xend = as.Date(input$futureDate),
                 y = 0, yend = Inf)
      
      
      my_plot <- my_plot +
        theme_igray()
      
      my_plot
    })
    
   
    

    

  
    
    
    
  
    
    
    
}
