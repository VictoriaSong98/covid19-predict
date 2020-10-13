library(shiny)
library(ggplot2)
library(dplyr)
library(zoo)

# Load data ----
library(readr)
# county_data <- read_csv("data/us_counties_covid19_daily.csv")
county_data <- read_csv("data/us-counties.csv")
state_data <- read_csv("data/us-states.csv")
countyNames <- read_csv("data/countyNames.csv")


# tryStates = c("California", "New York", "New Jersey")

allStates = countyNames[, c("state")]
allStates = unique(allStates)





# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Predict Covid Data with Different Models"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            
            
            selectInput("selectState", label = h3("Select a state"), 
                        choices = allStates
                        
            ),
            
            
            
            uiOutput("selectCounty"),
            
            #selectInput("selectTrend", label = h3("Pick a trend"),
            #choices = list("Cumulative Cases",
            #"New Cases",
            #"7-day Rolling Average")),
            
            
            
            dateRangeInput("dates", h3("Date range"), start = as.Date("2020-03-29"),
                           end = as.Date("2020-08-29")),
            
            
            
            dateInput("futureDate", "Choose a date to forecast the data",
                      value = "2020-03-29")
            
            
            # submitButton("Submit")
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
            
            
            #plotOutput(outputId = "statePlot"),
            
            textOutput("selected_var"),
            
            
            navbarPage(tabPanel("selectTab"),
                       tabPanel("Cumulative Cases",
                                plotOutput("cumulative_plot"),
                                checkboxGroupInput("chooseModel1", "Select Model(s)",
                                                   choices = list("Simple linear regression",
                                                                  "Quadratic regression",
                                                                  "Cubic regression",
                                                                  "Double Exponential Smoothing", 
                                                                  "7-day Rolling Average"))),
                       
                       
                       
                       
                       tabPanel("Daily Cases",
                                plotOutput("new_cases_plot"),
                                checkboxGroupInput("chooseModel2", "Select Model(s)",
                                                   choices = list("Simple linear regression",
                                                                  "Quadratic regression",
                                                                  "Cubic regression",
                                                                  "Double Exponential Smoothing",
                                                                  "7-day Rolling Average")))
                       
            )
            
            
            
            
            
            
        )
    )
)