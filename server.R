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
library(shinyjs) # for hidden 
library(ggplot2)
library(dplyr)
library(zoo)
library(ggthemes)
library(forecast)
library(deSolve)
library(reshape2)

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
  
  # if choose an alpha value
  # render 
  de_mod1 <- reactive({
    "Double Exponential Smoothing" %in% input$chooseModel1
  })

  
  observe({
    toggle(id = "alpha1", condition = "Double Exponential Smoothing" %in% input$chooseModel1)
    toggle(id = "beta1", condition = "Double Exponential Smoothing" %in% input$chooseModel1)
    toggle(id = "checkAlpha1", condition = "Double Exponential Smoothing" %in% input$chooseModel1)
    toggle(id = "checkBeta1", condition = "Double Exponential Smoothing" %in% input$chooseModel1)
    
    toggle(id = "cumulative_sir", condition = "Simple SIR" %in% input$chooseModel1)
    toggle(id = "Svalue1", condition = "Simple SIR" %in% input$chooseModel1)
    toggle(id = "Rvalue1", condition = "Simple SIR" %in% input$chooseModel1)
    toggle(id = "infection1", condition = "Simple SIR" %in% input$chooseModel1)
    toggle(id = "recovery1", condition = "Simple SIR" %in% input$chooseModel1)
    
    toggle(id = "Rate_value1", condition = "Logistic" %in% input$chooseModel1)
    toggle(id = "Kvalue1", condition = "Logistic" %in% input$chooseModel1)
  })
  
  observe({
    toggle(id = "alpha2", condition = "Double Exponential Smoothing" %in% input$chooseModel2)
    toggle(id = "beta2", condition = "Double Exponential Smoothing" %in% input$chooseModel2)
    toggle(id = "checkAlpha2", condition = "Double Exponential Smoothing" %in% input$chooseModel2)
    toggle(id = "checkBeta2", condition = "Double Exponential Smoothing" %in% input$chooseModel2)
    
    toggle(id = "new_sir", condition = "Simple SIR" %in% input$chooseModel2)
    
    toggle(id = "Svalue2", condition = "Simple SIR" %in% input$chooseModel2)
    toggle(id = "Ivalue2", condition = "Simple SIR" %in% input$chooseModel2)
    toggle(id = "Rvalue2", condition = "Simple SIR" %in% input$chooseModel2)
    toggle(id = "infection2", condition = "Simple SIR" %in% input$chooseModel2)
    toggle(id = "recovery2", condition = "Simple SIR" %in% input$chooseModel2)
    
    toggle(id = "Rate_value2", condition = "Logistic" %in% input$chooseModel2)
    toggle(id = "Kvalue2", condition = "Logistic" %in% input$chooseModel2)
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
  
  
  bigPlot <- function(dataSet, yvar, chooseMod, dateRange, future_date){
    
    if(yvar == "cases"){
      days_average = "case_7days"
      # for double exponential smoothing
      a = input$alpha1
      b = input$beta1
      checkA = input$checkAlpha1
      checkB = input$checkBeta1
      
      # for logistic 
      r = input$Rate_value1
      k = input$Kvalue1
    }else{
      days_average = "case_7days_new"
      # for double exponential smoothing
      a = input$alpha2
      b = input$beta2
      checkA = input$checkAlpha2
      checkB = input$checkBeta2
      
      # for logistic 
      r = input$Rate_value2
      k = input$Kvalue2
    }

    # plot the graph
    my_plot <- ggplot(dataSet) +
      geom_line(mapping = aes(x = date, y = get(yvar)), size=1.5) +
      labs (y = "Positive cases", x = "Date")+
      xlim(dateRange[1], dateRange[2])+
      ylim(0, max(dataSet[[yvar]], na.rm = TRUE))
    
    # plotting models
    slr <- "Simple linear regression" %in% chooseMod
    quad <- "Quadratic regression" %in% chooseMod
    cub <- "Cubic regression" %in% chooseMod
    doule_smo <- "Double Exponential Smoothing" %in% chooseMod
    ave <- "7-day Rolling Average" %in% chooseMod
    logi <- "Logistic" %in% chooseMod
    
    new <- data.frame(date = seq(dateRange[1], dateRange[2], by = "day"))
    
    if(slr){
      # simple linear model
      slr_ds <- lm(get(yvar) ~ as.Date(date), data = subset(dataSet, as.Date(date) < future_date))
      
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
      
      if (max(new$slr) > max(dataSet[[yvar]], na.rm = TRUE)){
        my_plot <- my_plot +
          ylim(0, max(dataSet[[yvar]], na.rm = TRUE) * 2)
      } 
    }
    
    if(quad){
      # qudratic model
      quad_ds <- lm(get(yvar) ~ as.Date(date) + I(time(date) ^ 2),
                    data = subset(dataSet, as.Date(date) < future_date))
      
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
      
      
      if (max(new$quad) > max(dataSet[[yvar]], na.rm = TRUE)){
        my_plot <- my_plot +
          ylim(0, max(dataSet[[yvar]], na.rm = TRUE) * 2)
      } 
    }
    
    if(cub){
      # cubic model
      cubic_ds <- lm(get(yvar) ~ as.Date(date) + I(time(date) ^ 2) + I(time(date) ^ 3),
                     data = subset(dataSet, as.Date(date) < future_date))
      
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
      
      if (max(new$cubic) > max(dataSet[[yvar]], na.rm = TRUE)){
        my_plot <- my_plot +
          ylim(0, max(dataSet[[yvar]], na.rm = TRUE) * 2)
      } 
      
    }
    
    
    if(doule_smo){
      pred.exp <- ts(dataSet[[yvar]])
      
      #new <- new %>%
      #mutate(dou_exp = pred.exp)
      
      #mod=ses(double_exp,alpha=0.05,beta = 0.1,initial="simple")
      mod = HoltWinters(pred.exp, alpha = a, beta = b, gamma = FALSE)
      if(checkA == TRUE & checkB == TRUE){
        mod = HoltWinters(pred.exp, gamma = FALSE)
      }else if(checkA == TRUE & checkB == FALSE){
        mod = HoltWinters(pred.exp, beta = b, gamma = FALSE)
      }else if(checkA == FALSE & checkB == TRUE){
        mod = HoltWinters(pred.exp, alpha = a, gamma = FALSE)
      }
      
      pred.de = c(0, 0, mod$fitted[, 1])
      
      #print(head(de_pred))
      #new <- new %>%
      #mutate(doubleExp = pred.de)
      
      
      my_plot <-
        my_plot +
        geom_line(data = dataSet, mapping = aes(x = date, y = pred.de), size=1, colour="plum2")
    }
    
    if (ave){
      my_plot <- my_plot +
        
        geom_line(data = dataSet, mapping = aes(x = date, y = get(days_average)), size=1, colour="orangered")+
        labs (y = "7-day Rolling average", x = "Date")+
        xlim(dateRange[1], dateRange[2])
    }
    
    if (logi){
      
      grow_logistic <- function(time, parms) {
        with(as.list(parms), {
          cases <- (K * P_0 * exp(R * time)) / (K + P_0 * (exp(R * time) - 1))
          data.frame(time = time, cases = cases)
        })
      }
      
      
      
      time_date = seq(dateRange[1], dateRange[2], by = "day")
      time = 1:length(time_date)
      
      p_value <- dataSet %>%
        filter(date == dateRange[1])
      
      p_0 = p_value[[yvar]][1]
      print(p_0)
      if (p_0 == 0){
        p_0 = 1
      }
      
      out <- grow_logistic(time, parms = list(P_0 = p_0, R = r, K = k))
      out <- bind_cols(out, time_date)
      names(out) = c("time_num", "cases", "time_date")
      View(out)
      my_plot <- my_plot +
        geom_line(data = out, aes(x = time_date, y = cases), colour="tan2", size = 1)
    }
    
    # add vertical line
    my_plot <- my_plot +
      annotate("segment", colour = "purple",
               x = as.Date(future_date), xend = as.Date(future_date),
               y = 0, yend = Inf)
    
    my_plot <- my_plot +
      theme_igray()
    
    my_plot
  }
  
  
  
  sirPlot <- function(dataSet, yvar, chooseMod, dateRange, future_date){
    
    # inde = beta value, reco = gamma value
    if(yvar == "cases"){
      s = input$Svalue1
      r = input$Rvalue1
      
      infe = input$infection1
      reco = input$recovery1
    }else{
      s = input$Svalue2
      r = input$Rvalue2
      
      infe = input$infection2
      reco = input$recovery2
    }
    
    sir <- "Simple SIR" %in% chooseMod

    # time sequence in date form
    
    time_date = seq(dateRange[1], dateRange[2], by = "day")
    
    
    # time sequence in numeric form
    time_number = 1:length(time_date)

    
    i_value <- dataSet %>%
      filter(date == dateRange[1])
    
    i = i_value[[yvar]][1]
    
    if (i == 0){
      i = 1
    }
    
    #ti = seq("2020-01-01", "2020-03-10", by = day)
    #ti=seq(from=1,to=100,by=1)
    para = c(beta = infe, gamma = reco)
    init = c(S = s, I = i, R = r)
    
    sir_eqn <- function(time, state, parameters){
      with(as.list(c(state, parameters)),{
        N = S + I + R
        lambda = beta * (I/N) 
        dS = -lambda * S
        dI = lambda * S - gamma * I
        dR = gamma * I
        return(list(c(dS, dI, dR)))
      })
    }
    
    # ode: solve for ordinary differential equation
    out<-as.data.frame(ode(y=init, times=time_number, func = sir_eqn, parms=para))
    
    out <- bind_cols(out, time_date)
    
    names(out) = c("time_num", "S", "I", "R", "time_dates")
    
    #out_long = melt(out,id="time_dates")
    #View(out_long)
    
    # my_plot_sir = ggplot(data = out_long, aes(x = time_dates, y = value, colour = variable, group = variable)) + 
    #   geom_line()+
    #   xlab("Date")+
    #   ylab("Positive cases")+
    #   theme_igray()
    
    my_plot_sir = ggplot(data = out, aes(x = as.Date(time_dates))) + 
      geom_line(aes(y = S, colour = "S"))+
      geom_line(aes(y = I, colour = "I"))+
      geom_line(aes(y = R, colour = "R"))+
      xlab("Date")+
      ylab("Positive cases")+
      theme_igray()
    
    my_plot_sir
  }
  
  output$cumulative_plot = renderPlot({

    bigPlot(dataSet = plotData(), yvar = "cases", chooseMod = input$chooseModel1, dateRange = input$dates, input$futureDate)
    
  })
  
  output$cumulative_sir = renderPlot({
    sirPlot(dataSet = plotData(), yvar = "cases", chooseMod = input$chooseModel1, dateRange = input$dates, input$futureDate)
  })
  
  
  
  output$new_cases_plot = renderPlot({
    
    bigPlot(dataSet = plotData(), yvar = "newcases", chooseMod = input$chooseModel2, dateRange = input$dates, input$futureDate)
    
  })
  
  output$new_sir = renderPlot({
    sirPlot(dataSet = plotData(), yvar = "newcases", chooseMod = input$chooseModel2, dateRange = input$dates, input$futureDate)
  })
  
}
