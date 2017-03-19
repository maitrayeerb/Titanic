#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# shinyUI(bootstrapPage(
#   
#     plotOutput(outputId = "main_plot"),
#     
#     plotOutput(outputId = "main_plot2")
#   
#   
# ))

shinyUI(fluidPage(
  titlePanel("Titanic Survivor Prediction"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Survival by feature"),
      
      radioButtons("dataSet", "Data set type:",
                   c("Train" = "train",
                     "Test" = "test")),
      selectInput("var",
                  label = "Choose a feature to display",
                  choices = c("Importance","Title","Gender & Age", "PClass",
                              "Family", "Embarked" ),
                  selected = "Importance")
      
      ),
    
    mainPanel(plotOutput("sub_plot")) #plotOutput("main_plot"),
  )
))