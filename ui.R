library(shiny)
library(shinyjs) # for hidden 
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
    
    
    sidebarLayout(
        sidebarPanel(
            
            
            
            selectInput("selectState", label = h3("Select a state"), 
                        choices = allStates
                        
            ),
            
            
            
            uiOutput("selectCounty"),
            
            
            
            
            dateRangeInput("dates", h3("Date range"), start = as.Date("2020-03-29"),
                           end = as.Date("2020-08-29")),
            
            
            
            dateInput("futureDate", "Choose a date to forecast the data",
                      value = "2020-03-29")
            
            
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
            
            # for hidden function
            shinyjs::useShinyjs(),
            
            #plotOutput(outputId = "statePlot"),
            
            textOutput("selected_var"),
            
            
            navbarPage(tabPanel("selectTab"),
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
                                        div(id = "group1",
                                            hidden(
                                                sliderInput("alpha1", "Alpha",
                                                            min = 0, max = 1, value = 0.1)
                                            ),
                                            hidden(
                                                checkboxInput("checkAlpha1", "Use the optimum Alpha value", FALSE)
                                            ),
                                            hidden(
                                                sliderInput("beta1", "Beta",
                                                            min = 0, max = 1, value = 0.1)
                                            ),
                                            hidden(
                                                checkboxInput("checkBeta1", "Use the optimum Beta value", FALSE)
                                            )
                                            
                                            
                                        )
                                    ),
                                    column(
                                        width = 4,
                                        div(id = "group2",
                                            hidden(
                                                sliderInput("Rate_value1", "Continuous growth date (r):",
                                                            min = 0, max = 1, value = 0.1)
                                            ),
                                            hidden(
                                                numericInput("Kvalue1", "Maximum case volume (K):",
                                                            value = 1000)
                                            )
                                        )
                                    )
                                ),
                                
                                hidden(
                                    plotOutput("cumulative_sir")
                                ),
                                
                                fluidRow(
                                    column(
                                        width = 6, 
                                        div(id = "group5",
                                            hidden(
                                                numericInput("Svalue1", h5("Input the S (Susceptible) value:"), value = 1000)
                                            ),
                                            hidden(
                                                numericInput("Rvalue1", h5("Input the R (Removal) value:"), value = 0)
                                            )
                                        )
                                    ),
                                    column(
                                        width = 6, 
                                        div(id = "group6",
                                            hidden(
                                                sliderInput("infection1", "Infection rate (Beta):",
                                                            min = 0, max = 1, value = 0.1)
                                            ),
                                            hidden(
                                                sliderInput("recovery1", "Recovery rate (Gamma):",
                                                            min = 0, max = 1, value = 0.1)
                                            )
                                        )
                                    )
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
                                        div(id = "group3",
                                            hidden(
                                                sliderInput("alpha2", "Alpha",
                                                            min = 0, max = 1, value = 0.1)
                                            ),
                                            hidden(
                                                checkboxInput("checkAlpha2", "Use the optimum Alpha value", FALSE)
                                            ),
                                            hidden(
                                                sliderInput("beta2", "Beta",
                                                            min = 0, max = 1, value = 0.1)
                                            ),
                                            hidden(
                                                checkboxInput("checkBeta2", "Use the optimum Beta value", FALSE)
                                            )
                                        )
                                    ),
                                    column(
                                        width = 4,
                                        div(id = "group4",
                                            hidden(
                                                sliderInput("Rate_value2", "Continuous growth date (r):",
                                                            min = 0, max = 1, value = 0.1)
                                            ),
                                            hidden(
                                                numericInput("Kvalue2", "Maximum case volume (K):",
                                                             value = 1000)
                                            )
                                        )
                                    )
                                ),
                                
                                hidden(
                                    plotOutput("new_sir")
                                ),
                                
                                fluidRow(
                                    column(
                                        width = 6, 
                                        div(id = "group7",
                                            hidden(
                                                numericInput("Svalue2", h5("Input the S (Susceptible) value:"), value = 1000)
                                            ),
                                            hidden(
                                                numericInput("Rvalue2", h5("Input the R (Removal) value:"), value = 0)
                                            )
                                        )
                                    ),
                                    column(
                                        width = 6, 
                                        div(id = "group8",
                                            hidden(
                                                sliderInput("infection2", "Infection rate (Beta):",
                                                            min = 0, max = 1, value = 0.1)
                                            ),
                                            hidden(
                                                sliderInput("recovery2", "Recovery rate (Gamma):",
                                                            min = 0, max = 1, value = 0.1)
                                            )
                                        )
                                    )
                                )
                       )
            )
        )
    )
)