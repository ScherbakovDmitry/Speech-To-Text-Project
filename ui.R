#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidytext)
library(textreadr)
library(tidyverse)
library(tm)
library(Matrix)
library(textdata)
library(reshape2)
library(wordcloud)
library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(DT)
library(ggplot2)
data("stop_words")

#USER INTERFACE
shinyUI <- dashboardPage(
    #HEADER
    dashboardHeader(title = 'Text Analytics - Team 5'),
    
    #SIDEBAR
    dashboardSidebar(
        sidebarMenu(
            menuItem("Database", tabName = "database", icon = icon("database")),
            menuItem("Frequencies", tabName = "freq", icon = icon("chart-bar")),
            menuItem("Sentiment", tabName = "sentiment", icon = icon("heart")),
            menuItem("TF-IDF", tabName = "tf-idf", icon = icon("sort-amount-down")),
            menuItem("Map", tabName = "map", icon = icon("map"))
        )
    ),
    
    #BODY
    dashboardBody(
        tabItems(
            # 1st tab content
            tabItem(tabName = "database",
                    fluidRow(
                        box(
                            title = "Database", 
                            width = 12,
                            tableOutput('survey'))
                        
                    )
            ),
            
            # 2nd tab content
            tabItem(tabName = "freq",
                    fluidRow(
                        box(
                            title = "Where do you live?",
                            plotOutput("q1freq"),
                        ),
                        box(
                            title = "How much time do you spend in your commute per day?",
                            plotOutput("q2freq")
                        )
                    ),
                    fluidRow(
                        box(
                            title = "What is your opinion about working remotely?",
                            width = 12,
                            plotOutput("q3freq")
                        )
                    )
            ),
            
            # 3rd tab content
            tabItem(tabName = "sentiment",
                    fluidRow(  
                        box(
                            title = "What is your opinion about working remotely? - nrc Word Cloud",
                            width = 6,
                            plotOutput("q3nrc"),
                        ),
                        box(
                            title = "What is your opinion about working remotely? - bing Word Cloud",
                            width = 6,
                            plotOutput("q3bing")
                        )
                    ),
                    fluidRow(
                        infoBoxOutput('q3afinn'),
                        infoBoxOutput('q4afinn')
                    ),
                    fluidRow(  
                        box(
                            title = "Would you like to work at the San Jose office at least once per week? Why? - nrc Word Cloud",
                            width = 6,
                            plotOutput("q4nrc"),
                        ),
                        box(
                            title = "Would you like to work at the San Jose office at least once per week? Why? - bing Word Cloud",
                            width = 6,
                            plotOutput("q4bing")
                        )
                    )
            ),
            
            # 4th tab content
            tabItem(tabName = "tf-idf",
                    column(
                        width = 12,
                        plotOutput("tfidf")
                    ),
                    fluidRow(
                        box(
                            title = "Tokens order by tf-idf (Descending)", 
                            width = 12,
                            align = "center",
                            tableOutput('tftable'))
                        
                    )
                    
            ),
            
            # Last tab content
            tabItem(tabName = "map",
                    
                    fluidRow(
                        box(title = "Explore our Interactive Map", 
                            width = 12, 
                            leafletOutput("coolmap")
                        )
                    )#,
                    
                    #fluidRow(
                    # box(title = "Predictive Model", 
                    #    width = 12 
                    #)
                    #)
            )
            
        ) #Closing tab items
    ) #Closing Dashboard Body
) ##Closing Dashboard Page
