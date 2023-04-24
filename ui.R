#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  navbarPage("CMAH - Danger ratings",
             tabPanel("Single problem", 
                     sidebarLayout(
                         sidebarPanel(
                           selectInput("agency",
                                       "Agency",
                                       c("Generic" = "Gen", 
                                         "Avalanche Canada" = "AvCan", 
                                         "Parks Canada" = "PkCan")),
                           selectInput("avProb",
                                       "Avalanche problem",
                                       list("Generic" = "Gen", 
                                            "Wind slabs" = "WIND",
                                            "Storm slabs" = "STORM",
                                            "Persistent slabs" = "PERS",
                                            "Deep persisient slabs" = "DPERS", 
                                            "Wet slabs" = "WET",
                                            "Loose dry avalanches" = "LDRY",
                                            "Loose wet avalanches" = "LWET", 
                                            "Cornices" = "CORN")),
                           selectInput("plotType",
                                       "Type of plot",
                                       list("Obs", 
                                            "Pred - Most common" = "PredMost", 
                                            "Pred - Product-sum" = "PredProd", 
                                            "Pred - Probability" = "PredProb")),
                           selectInput("probDR",
                                       "DR for probility plot",
                                       list("Low", "Moderate", "Considerable", "High/Extreme")),
                           radioButtons("withElev",
                                        "Individual elevation bands",
                                        list("No", "Yes"),
                                        inline = TRUE),
                           submitButton("Update")),
                         mainPanel(
                           plotOutput("hzdPlot"),
                           plotOutput("hzdPlot2"),
                           plotOutput("hzdPlot3")
                         ))),
             tabPanel("Two problems"),
             tabPanel("Assessment aid",
                      sidebarLayout(
                         sidebarPanel(
                           selectInput("agency3",
                                       "Agency",
                                       c("Generic" = "Gen", 
                                         "Avalanche Canada" = "AvCan", 
                                         "Parks Canada" = "PkCan")),
                           selectInput("avProb3",
                                       "Avalanche problem",
                                       list("Generic" = "Gen", 
                                            "Wind slabs" = "WIND",
                                            "Storm slabs" = "STORM",
                                            "Persistent slabs" = "PERS",
                                            "Deep persisient slabs" = "DPERS", 
                                            "Wet slabs" = "WET",
                                            "Loose dry avalanches" = "LDRY",
                                            "Loose wet avalanches" = "LWET", 
                                            "Cornices" = "CORN")),
                           sliderInput("LH", 
                                       "Likelihood of avalanches", 
                                       min = 1, max = 9, value = c(3, 4), round = TRUE),
                           sliderInput("SZ", "Destructive size", min = 1, max = 9, value = c(3, 4), round = TRUE),
                           submitButton("Update")),
                           mainPanel(
                             plotOutput("hzdChartProb"),
                             plotOutput("histPred")
                           )))
                         )
  
))
