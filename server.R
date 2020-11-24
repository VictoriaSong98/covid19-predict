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
library(tidyverse)
library(zoo)
library(ggthemes)
library(forecast)
library(deSolve) # for SIR and logistic
library(reshape2) # for SIR
library(RCurl) # for viewing csv file from github

# Load data ----
library(readr)

county_data_first <- read_csv("data/us-counties.csv")
state_data_first <- read_csv("data/us-states.csv")
countyNames <- read_csv("data/countyNames.csv")



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  county_data <- reactiveValues(data = county_data_first)
  
  state_data <- reactiveValues(data = state_data_first)
  
  observeEvent(input$updateDataset, {
    # update the county data from the URL
    get_county <- getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
    county_data$data <- read.csv(text = get_county)
    county_data$data <- county_data$data %>%
      mutate(date = as.Date(date),
             county = as.character(county),
             state = as.character(state),
             fips = as.character(fips),
             cases = as.integer(cases),
             deaths = as.integer(deaths))
    
    # update the state data from the URL
    get_state <- getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
    state_data$data <- read.csv(text = get_state)
    state_data$data <- state_data$data %>%
      mutate(date = as.Date(date),
             state = as.character(state),
             fips = as.character(fips),
             cases = as.integer(cases),
             deaths = as.integer(deaths))
  })
  
  # A text input showing the latest date of the current dataset
  output$lateset_date <- renderText({ 
    paste("The latest date of the current dataset is: ", max(state_data$data$date))
  })
  
  # A text input showing which state or county the user is choosing
  output$selected_var <- renderText({ 
    if (countyName() == "allState"){
      paste("You have selected", stateName(), "state")
    }else{
      paste("You have selected", countyName(), "county in", stateName(), "state")
    }
  })
  
  # get the state name from the input box called "selectState" and make it to be dynamic
  stateName <- reactive({
    req(input$selectState)
  })
  
  
  # variable name for the chosen county is "chooseCounty"
  output$selectCounty <- renderUI({
    selectInput("chooseCounty", h3("Select a county"), 
                choices = c("Overall State" = "allState", subset(countyNames, state == stateName(), c("county"))))
  })
  
  
  # create a function that change value of K here (for logistic model)
  dynamic_k <- reactive({
    if (countyName() == "allState"){
      temp = subset(state_data$data,state == stateName())
    }else{
      temp = subset(county_data$data,state == stateName() & county == countyName())
    }
    
    # adding "newcases" variable in case the user wants to render new cases plot
    temp <- temp %>%
      mutate(newcases = c(NA, diff(cases)))
    maxK = 1
    if("Logistic" %in% input$chooseModel1 & input$tabs == "Cumulative Cases"){
      # case 1: when in cumulative tab
      maxK = max(temp$cases) * 2
    }else if("Logistic" %in% input$chooseModel2 & input$tabs == "Daily Cases"){
      # case 2: when in daily tab
      maxK = max(temp$newcases, na.rm = TRUE) * 2
    }
    return(maxK)
  })
  
  
  output$inputKvalue <- renderUI({
    #hidden(
      numericInput("K_value", "Maximum case volume (K):",
                   value = dynamic_k())
    #)
  })
  
  # create a function that change value of S here (for logistic model)
  dynamic_s <- reactive({
    if (countyName() == "allState"){
      temp = subset(state_data$data,state == stateName())
    }else{
      temp = subset(county_data$data,state == stateName() & county == countyName())
    }
    
    temp = subset(temp, date >= input$dates[1] & date <= input$dates[2])
    maxS = 1
    if("Simple SIR" %in% input$chooseModel1 | "Simple SIR" %in% input$chooseModel2 | input$tabs == "SIR Model"){
      # case 1: when in cumulative tab
      maxS = max(temp$cases) * 10
    }
    return(maxS)
  })
  
  output$inputSvalue <- renderUI({
    #hidden(
      numericInput("Svalue", h5("Input the S (Susceptible) value:"), value = dynamic_s())
    #)
  })
  
  # create a function that change value of I here (for SIR model)
  dynamic_i <- reactive({
    if (countyName() == "allState"){
      temp = subset(state_data$data,state == stateName())
    }else{
      temp = subset(county_data$data,state == stateName() & county == countyName())
    }
    temp = subset(temp, date >= input$dates[1] & date <= input$dates[2])

    maxI = 1
    if("Simple SIR" %in% input$chooseModel1 | "Simple SIR" %in% input$chooseModel2 | input$tabs == "SIR Model"){
      maxI = temp$cases[1]
    }
    return(maxI)
  })
  
  output$inputIvalue <- renderUI({
    #hidden(
      numericInput("Ivalue", h5("Input the I (Infected) value:"), value = dynamic_i())
    #)
  })
  
  # Use toggle function to show the input boxes if a model is chosen
  observe({
    toggle(id = "alpha1", condition = "Double Exponential Smoothing" %in% input$chooseModel1)
    toggle(id = "beta1", condition = "Double Exponential Smoothing" %in% input$chooseModel1)
    toggle(id = "checkAlpha1", condition = "Double Exponential Smoothing" %in% input$chooseModel1)
    toggle(id = "checkBeta1", condition = "Double Exponential Smoothing" %in% input$chooseModel1)
    
    toggle(id = "cumulative_sir", condition = "Simple SIR" %in% input$chooseModel1)
  })
  
  observe({
    toggle(id = "alpha2", condition = "Double Exponential Smoothing" %in% input$chooseModel2)
    toggle(id = "beta2", condition = "Double Exponential Smoothing" %in% input$chooseModel2)
    toggle(id = "checkAlpha2", condition = "Double Exponential Smoothing" %in% input$chooseModel2)
    toggle(id = "checkBeta2", condition = "Double Exponential Smoothing" %in% input$chooseModel2)
    
    toggle(id = "new_sir", condition = "Simple SIR" %in% input$chooseModel2)
  })
  
  observe({
    toggle(id = "Rate_value", condition = ("Logistic" %in% input$chooseModel1) | ("Logistic" %in% input$chooseModel2))
    toggle(id = "K_value", condition = ("Logistic" %in% input$chooseModel1) | ("Logistic" %in% input$chooseModel2))
    
    toggle(id = "Svalue", condition = ("Simple SIR" %in% input$chooseModel1) | ("Simple SIR" %in% input$chooseModel2) | (input$tabs == "SIR Model"))
    toggle(id = "Ivalue", condition = ("Simple SIR" %in% input$chooseModel1) | ("Simple SIR" %in% input$chooseModel2) | (input$tabs == "SIR Model"))
    toggle(id = "Rvalue", condition = ("Simple SIR" %in% input$chooseModel1) | ("Simple SIR" %in% input$chooseModel2) | (input$tabs == "SIR Model"))
    toggle(id = "infection", condition = ("Simple SIR" %in% input$chooseModel1) | ("Simple SIR" %in% input$chooseModel2) | (input$tabs == "SIR Model"))
    toggle(id = "recovery", condition = ("Simple SIR" %in% input$chooseModel1) | ("Simple SIR" %in% input$chooseModel2) | (input$tabs == "SIR Model"))
  })
  
  
  
  countyName <- reactive({
    req(input$chooseCounty)
  })
  
  # create a data frame that contains cumulative cases, daily cases and 7-day moving average data for 
  # the state or the county that the user is choosing
  plotData <- reactive({
    if (countyName() == "allState"){
      temp = subset(state_data$data,state == stateName())
    }else{
      temp = subset(county_data$data,state == stateName() & county == countyName())
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
  
  
  # a function that plot the line for actual data and any line for the model that the user is choosing
  bigPlot <- function(dataSet, yvar, chooseMod, dateRange, future_date){
    
    if(yvar == "cases"){
      days_average = "case_7days"
      # for double exponential smoothing
      a = input$alpha1
      b = input$beta1
      checkA = input$checkAlpha1
      checkB = input$checkBeta1
    }else{
      days_average = "case_7days_new"
      # for double exponential smoothing
      a = input$alpha2
      b = input$beta2
      checkA = input$checkAlpha2
      checkB = input$checkBeta2
    }
    
    # plot the graph for the actual data at the beginning
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
    sir <- "Simple SIR" %in% chooseMod
    
    # create a data frame called "new" to store the predicted data
    new <- data.frame(date = seq(dateRange[1], dateRange[2], by = "day"))
    
    # simple linear model
    if(slr){
      slr_ds <- lm(get(yvar) ~ as.Date(date), data = subset(dataSet, as.Date(date) < future_date))
      
      pred.slr <- predict(slr_ds, new)
      
      pred.slrLoCI <- predict(slr_ds, new,
                              interval = "confidence",
                              level = 0.9)[, 2]
      pred.slrHiCI <- predict(slr_ds, new,
                              interval = "confidence",
                              level = 0.9)[, 3]
      
      # store the data for simple linear model, also the lower bound and higher bound for the prediction 
      # with 90% confidence interval
      new <- new %>%
        mutate(slr = pred.slr,
               slrLoCI = pred.slrLoCI,
               slrHiCI = pred.slrHiCI)
      
      # plot the simple linear model line
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
      # double exponential smoothing model
      pred.exp <- ts(dataSet[[yvar]])
      
      # check if the users want to adjust the alpha value or beta value by themselves,
      # or use the optimized values 
      mod = HoltWinters(pred.exp, alpha = a, beta = b, gamma = FALSE)
      if(checkA == TRUE & checkB == TRUE){
        mod = HoltWinters(pred.exp, gamma = FALSE)
      }else if(checkA == TRUE & checkB == FALSE){
        mod = HoltWinters(pred.exp, beta = b, gamma = FALSE)
      }else if(checkA == FALSE & checkB == TRUE){
        mod = HoltWinters(pred.exp, alpha = a, gamma = FALSE)
      }
      
      pred.de = c(0, 0, mod$fitted[, 1])
      
      my_plot <-
        my_plot +
        geom_line(data = dataSet, mapping = aes(x = date, y = pred.de), size=1, colour="plum2")
    }
    
    if (ave){
      # 7-day moving average model
      my_plot <- my_plot +
        geom_line(data = dataSet, mapping = aes(x = date, y = get(days_average)), size=1, colour="orangered")+
        labs (y = "7-day Rolling average", x = "Date")+
        xlim(dateRange[1], dateRange[2])
    }
    
    if (logi){
      # logistic model
      logi_r = input$Rate_value
      k = input$K_value
      
      
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
      if (p_0 == 0){
        p_0 = 1
      }
      
      out <- grow_logistic(time, parms = list(P_0 = p_0, R = logi_r, K = k))
      out <- bind_cols(out, time_date)
      names(out) = c("time_num", "cases", "time_date")
      
      out <- out %>%
        mutate(newCases = c(0, diff(cases)))
      
      if (yvar == "cases"){
        my_plot <- my_plot +
          geom_line(data = out, aes(x = time_date, y = cases), colour="tan2", size = 1)
      }else if(yvar == "newcases"){
        my_plot <- my_plot +
          geom_line(data = out, aes(x = time_date, y = newCases), colour="tan2", size = 1)
      }
      
    }
    
    if (sir){
      # SIR model
      s = input$Svalue
      i = input$Ivalue
      sir_r = input$Rvalue
      infe = input$infection
      reco = input$recovery
      
      # create a sequence that contains dates from the start date to the end date
      sir_time_date = seq(dateRange[1], dateRange[2], by = "day")
      
      # time sequence in numeric form
      time_number = 1:length(sir_time_date)
      
      
      i_value <- dataSet %>%
        filter(date == dateRange[1])
      
      # pass in initial values and the parameters
      para = c(beta = infe, gamma = reco)
      init = c(S = s, I = i, R = sir_r)
      
      # create a function called "sir_eqn" that stores differential equations 
      # that are needed in SIR model
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
      # integrate the differential equations and get data for S, I and R
      out_sir <- as.data.frame(ode(y=init, times=time_number, func = sir_eqn, parms=para))
      
      out_sir <- bind_cols(out_sir, sir_time_date)
      
      names(out_sir) = c("time_num", "S", "I", "R", "time_dates")
      
      # combine I and R for plotting on cumulative window
      out_sir <- out_sir %>%
        mutate(I_R = I + R)
      
      # add new cases for I+R
      out_sir <- out_sir %>%
        mutate(newI_R = c(0, diff(I_R)))
      
      # plot the different I+R line if the user is choosing cumulative cases or daily cases
      if (yvar == "cases"){
        my_plot <- my_plot +
          geom_line(data = out_sir, aes(x = time_dates, y = I_R), colour="brown1", size = 1)
      }else if(yvar == "newcases"){
        my_plot <- my_plot +
          geom_line(data = out_sir, aes(x = time_dates, y = newI_R), colour="brown1", size = 1)
      }
    }
    
    # add vertical line
    my_plot <- my_plot +
      annotate("segment", colour = "purple",
               x = as.Date(future_date), xend = as.Date(future_date),
               y = 0, yend = Inf)
    
    # add a theme for the plot
    my_plot <- my_plot +
      theme_igray()
    
    my_plot
  }
  
  # this function is for plotting the three lines from SIR model 
  # in the SIR model tab
  sirPlot <- function(dataSet, dateRange){
    s = input$Svalue
    i = input$Ivalue
    sir_r = input$Rvalue
    infe = input$infection
    reco = input$recovery

    time_date = seq(dateRange[1], dateRange[2], by = "day")
    
    # time sequence in numeric form
    time_number = 1:length(time_date)
    
    para = c(beta = infe, gamma = reco)
    init = c(S = s, I = i, R = sir_r)
    
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
    
    my_plot_sir = ggplot(data = out, aes(x = as.Date(time_dates))) + 
      geom_line(aes(y = S, colour = "S"), size = 1)+
      geom_line(aes(y = I, colour = "I"), size = 1)+
      geom_line(aes(y = R, colour = "R"), size = 1)+
      xlab("Date")+
      ylab("Positive cases")+
      theme_igray()
    
    my_plot_sir
  }
  
  output$cumulative_plot = renderPlot({
    bigPlot(dataSet = plotData(), yvar = "cases", chooseMod = input$chooseModel1, dateRange = input$dates, input$futureDate)
  })
  
  
  
  output$new_cases_plot = renderPlot({
    bigPlot(dataSet = plotData(), yvar = "newcases", chooseMod = input$chooseModel2, dateRange = input$dates, input$futureDate)
  })
  
  
  output$sir_model_plot = renderPlot({
    sirPlot(dataSet = plotData(), dateRange = input$dates)
  })
}
