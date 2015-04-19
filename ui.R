#
# Developing Data Products
# Course Project
#
# Jose Albornoz
# April 2014
#
# ui.R

library(shiny)

# loads required library 
library(rCharts)

shinyUI(
  
    navbarPage("A Look at 2007 US Cocaine Seizures",
               
               tabPanel("The Plots",
                        
                        sidebarPanel(
                            sliderInput("theMonth", label = h3("Month"), min = 1, 
                                        max = 12, value = 6)
                        ),
                        
                        mainPanel(
                                
                            plotOutput("purity"),
                            plotOutput("weight"), 
                            plotOutput("value")
                                
                        )
                        
               ),
               
               tabPanel("The Skinny",
                        mainPanel(
                            includeMarkdown("include.md")
                        )
               )
    )
)