library(shiny)
library(shinyjs) # for hidden 
library(tidyverse)
library(zoo)
library(RCurl) # for viewing csv file from github

# Load data ----
library(readr)

#countyNames <- read_csv("data/countyNames.csv")
allStates <- read_csv("data/allStates.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Predict Covid Data with Different Models"),
  
  sidebarLayout(
    sidebarPanel(
      
      
      
      selectInput("selectState", label = h3("Select a state"), 
                  choices = allStates
      ),
      
      
      uiOutput("selectCounty"),
      
      
      dateRangeInput("dates", h3("Date range"), start = as.Date("2020-03-29"),
                     end = as.Date("2020-08-29")),
      
      
      
      dateInput("futureDate", "Choose a date to forecast the data",
                value = "2020-03-29"),
      
      # logistic model inputs
      hidden(
        sliderInput("Rate_value", "Continuous growth date (r):",
                    min = 0, max = 0.2, value = 0.001)
      ),
      
      uiOutput("inputKvalue"),
      
      # sir model inputs
      uiOutput("inputSvalue"),
      
      uiOutput("inputIvalue"),
      
      hidden(
        numericInput("Rvalue", h5("Input the R (Removal) value:"), value = 0)
      ),
      
      hidden(
        sliderInput("infection", "Infection rate (Beta):",
                    min = 0, max = 0.5, value = 0.1)
      ),
      
      hidden(
        sliderInput("recovery", "Recovery rate (Gamma):",
                    min = 0, max = 0.5, value = 0.1)
      ),
      
      
      actionButton("updateDataset", "Get the latest dataset"),
      
      helpText("Updating the latest datasets may take a while."),
      
      helpText("Covid-19 data in the Unites States from New York Times. Available at https://github.com/nytimes/covid-19-data")

      
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      
      # for hidden function
      shinyjs::useShinyjs(),
      
      
      textOutput("lateset_date"),
      
      textOutput("selected_var"),
      
      
      tabsetPanel(id = "tabs",
                  tabPanel("Cumulative Cases",
                           plotOutput("cumulative_plot"),
                           
                           fluidRow(
                             column(
                               width = 4,
                               checkboxGroupInput("chooseModel1", "Select Model(s)",
                                                  choices = list("Simple linear regression",
                                                                 "Quadratic regression",
                                                                 "Cubic regression",
                                                                 "Double Exponential Smoothing", 
                                                                 "7-day Rolling Average",
                                                                 "Logistic",
                                                                 "Simple SIR"))),
                             column(
                               width = 4,
                               hidden(
                                 sliderInput("alpha1", "Alpha",
                                             min = 0, max = 1, value = 0.1)
                               ),
                               hidden(
                                 checkboxInput("checkAlpha1", "Use the optimum Alpha value", FALSE)
                               )
                             ),
                             column(
                               width = 4,
                               hidden(
                                 sliderInput("beta1", "Beta",
                                             min = 0, max = 1, value = 0.1)
                               ),
                               hidden(
                                 checkboxInput("checkBeta1", "Use the optimum Beta value", FALSE)
                               )
                             )
                           ),
                           
                           hidden(
                             plotOutput("cumulative_sir")
                           )
                  ),
                  
                  
                  
                  
                  tabPanel("Daily Cases",
                           plotOutput("new_cases_plot"),
                           fluidRow(
                             column(
                               width = 4,
                               checkboxGroupInput("chooseModel2", "Select Model(s)",
                                                  choices = list("Simple linear regression",
                                                                 "Quadratic regression",
                                                                 "Cubic regression",
                                                                 "Double Exponential Smoothing", 
                                                                 "7-day Rolling Average",
                                                                 "Logistic",
                                                                 "Simple SIR"))),
                             column(
                               width = 4,
                               hidden(
                                 sliderInput("alpha2", "Alpha",
                                             min = 0, max = 1, value = 0.1)
                               ),
                               hidden(
                                 checkboxInput("checkAlpha2", "Use the optimum Alpha value", FALSE)
                               )
                               
                             ),
                             column(
                               width = 4,
                               hidden(
                                 sliderInput("beta2", "Beta",
                                             min = 0, max = 1, value = 0.1)
                               ),
                               hidden(
                                 checkboxInput("checkBeta2", "Use the optimum Beta value", FALSE)
                               )
                             )
                           ),
                           hidden(
                             plotOutput("new_sir")
                           )
                  ),
                  tabPanel("SIR Model",
                           plotOutput("sir_model_plot")
                  )
      )
    )
  )
)