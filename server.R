#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# use this to debug
# shiny::runApp(display.mode="showcase")
library(shiny)
library(ggplot2)
library(dplyr)
library(zoo)
library(ggthemes)
library(forecast)

# Load data ----
library(readr)

# county_data <- read_csv("data/us_counties_covid19_daily.csv")
county_data <- read_csv("data/us-counties.csv")
state_data <- read_csv("data/us-states.csv")

countyNames <- read_csv("data/countyNames.csv")
#tryStates = c("California", "New York", "New Jersey")

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
  
  output$chooseModel <- renderUI({
    if (input$chooseModel == "Double Exponential Smoothing"){
      sliderInput("slider_alpha", h3("Alpha"),
                  min = 0, max = 1, value = 0.1)
      
      sliderInput("slider_beta", h3("Beta"),
                  min = 0, max = 1, value = 0.1)
    }
  })
  
  countyName <- reactive({
    input$chooseCounty
  })
  
  plotData <- reactive({
    if (countyName() == "allState"){
      temp = subset(state_data,state == stateName())
    }else{
      temp = subset(county_data,state == stateName() & county == countyName())
    }
    
    # adding "newcases" variable in case the user wants to render new cases plot
    temp <- temp %>%
      mutate(newcases = c(NA, diff(cases)))
    
    # adding "7 day rolling average" variable for cumulative cases
    temp <- temp %>%
      mutate(case_7days = rollmean(cases, k = 7, fill = NA))
    
    # adding "7 day rolling average" variable for new cases
    temp <- temp %>%
      mutate(case_7days_new = rollmean(newcases, k = 7, fill = NA))
    
    return(temp)
  })
  
  output$selected_var <- renderText({ 
    if (countyName() == "allState"){
      paste("You have selected", stateName(), "state")
    }else{
      paste("You have selected", countyName(), "county in", stateName(), "state")
    }
  })
  
  
  bigPlot <- function(dataSet, yvar, chooseMod, dateRange, fuDate){
    # plot the graph
    my_plot <- ggplot(dataSet) +
      geom_line(mapping = aes(x = date, y = cases), size=1.5) +
      labs (y = "Positive cases", x = "Date")+
      xlim(dateRange[1], dateRange[2])+
      ylim(0, max(dataSet $ cases, na.rm = TRUE) * 2)
    
    # plotting models
    slr <- "Simple linear regression" %in% chooseMod
    quad <- "Quadratic regression" %in% chooseMod
    cub <- "Cubic regression" %in% chooseMod
    doule_smo <- "Double Exponential Smoothing" %in% chooseMod
    
    new <- data.frame(date = seq(dateRange[1], dateRange[2], by = "day"))
    
    if(slr){
      # simple linear model
      slr_ds <- lm(cases ~ as.Date(date), data = subset(dataSet, as.Date(date) < fuDate))
      
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
      # qudratic model
      quad_ds <- lm(cases ~ as.Date(date) + I(time(date) ^ 2),
                    data = subset(dataSet, as.Date(date) < fuDate))
      
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
      # cubic model
      cubic_ds <- lm(cases ~ as.Date(date) + I(time(date) ^ 2) + I(time(date) ^ 3),
                     data = subset(dataSet, as.Date(date) < fuDate))
      
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
    
    
    if(doule_smo){
      print("yes1")
      pred.exp <- ts(dataSet $ cases, start = c(2020, 3, 29))
      
      #new <- new %>%
      #mutate(dou_exp = pred.exp)
      
      #mod=ses(double_exp,alpha=0.05,beta = 0.1,initial="simple")
      mod = HoltWinters(pred.exp, gamma = FALSE)
      my_plot <-
        ts.plot(mod$fitted,col=c("red","blue"))
      #ts.plot(mod)
      
      
    }
    
    # add vertical line
    my_plot <- my_plot +
      annotate("segment", colour = "purple",
               x = as.Date(fuDate), xend = as.Date(fuDate),
               y = 0, yend = Inf)
    
    my_plot <- my_plot +
      theme_igray()
    
    my_plot
  }
  
  
  output$cumulative_plot = renderPlot({

    bigPlot(dataSet = plotData(), yvar = "", chooseMod = input$chooseModel1, dateRange = input$dates, input$futureDate)
    
  })
  
  
  
  output$new_cases_plot = renderPlot({
    # plot the graph
    my_plot <- ggplot(plotData()) +
      geom_line(mapping = aes(x = date, y = newcases), size=1.5) +
      labs (y = "Positive cases", x = "Date")+
      xlim(input$dates[1], input$dates[2])
    
    #print(max(plotData() $ newcases, na.rm = TRUE) *2)
    
    # plotting models 
    slr <- "Simple linear regression" %in% input$chooseModel2
    quad <- "Quadratic regression" %in% input$chooseModel2
    cub <- "Cubic regression" %in% input$chooseModel2
    doule_smo <- "Double Exponential Smoothing" %in% input$chooseModel2
    
    new <- data.frame(date = seq(input$dates[1], input$dates[2], by = "day"))
    
    if(slr){
      # simple linear model
      slr_ds <- lm(newcases ~ as.Date(date), data = subset(plotData(), as.Date(date) < input$futureDate))
      
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
        ylim(0, (max(plotData()$case_7days_new, na.rm = TRUE) * 2))+
        geom_line(data = new, aes(x = date, y = slr), colour = "steelblue", 
                  size=1.5, linetype = "dashed")+
        geom_ribbon(data = new, aes(x = date, ymin = slrLoCI, ymax = slrHiCI), 
                    fill = "olivedrab", alpha = 0.4, stat = "identity")
    }
    
    if(quad){
      # qudratic model
      quad_ds <- lm(newcases ~ as.Date(date) + I(time(date) ^ 2), 
                    data = subset(plotData(), as.Date(date) < input$futureDate))
      
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
        ylim(0, (max(plotData()$case_7days_new, na.rm = TRUE) * 2))+
        geom_line(data = new, aes(x = date, y = quad), colour = "darkorange", 
                  size=1.5, linetype = "dashed")+
        geom_ribbon(data = new, aes(x = date, ymin = quadLoCI, ymax = quadHiCI), 
                    fill = "hotpink2", alpha = 0.4, stat = "identity")
    }
    
    if(cub){
      # cubic model
      cubic_ds <- lm(newcases ~ as.Date(date) + I(time(date) ^ 2) + I(time(date) ^ 3), 
                     data = subset(plotData(), as.Date(date) < input$futureDate))
      
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
        ylim(0, (max(plotData()$case_7days_new, na.rm = TRUE) * 2))+
        geom_line(data = new, aes(x = date, y = cubic), colour = "royalblue", 
                  size=1.5, linetype = "dashed")+
        geom_ribbon(data = new, aes(x = date, ymin = cubicLoCI, ymax = cubicHiCI), 
                    fill = "salmon", alpha = 0.4, stat = "identity")
    }
    
    
    
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
