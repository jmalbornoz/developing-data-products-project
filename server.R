#
# Developing Data Products
# Course Project
#
# Jose Albornoz
# April 2014
#
# server.R

# clear everything
rm(list=ls(all=TRUE))

##############################
# invokes libraries and data #
##############################

library(ggvis)
data(cocaine)

# more libraries
library(shiny)
library(ggplot2)
library(mapproj)
library(maps)
library(data.table)
library(markdown)

# loads map data
states_map <- map_data("state")

# list of US states
statesList <- unique(states_map$region)

#########################
#  AUXILIARY FUNCTIONS  #
#########################

# prepares data tables for plotting
# 
#' @param df data frame
#' @param theName name of 2nd column in return data frame
#
completeDF <- function(df, theName) {
  comp <- statesList[is.na(pmatch(statesList,df$state))]
  theName <- names(df)[2]
  newdf <- data.frame(comp, rep(0, length(comp)))
  names(newdf) <- c("state", theName)
  rbind(df, newdf)
}

# map plotting function
# 
#' @param dt  data.table
#' @param states_map data.frame returned from map_data("state")
#' @param fill character name of the variable
#' @param title character
#' @param low character hex
#' @param high character hex
#' @return ggplot
# 
plotByState <- function (dt, states_map, fill, title, low = "#fff5eb", high = "#d94801") {
  p <- ggplot(dt, aes(map_id = state)) +
    geom_map(aes_string(fill = fill), map = states_map, colour='black') +
    expand_limits(x = states_map$long, y = states_map$lat) +
    coord_map() + theme_bw() + 
    labs(x = "Long", y = "Lat", title = title) + 
    scale_fill_gradient(low = low, high = high)
}

# converts state abbreviations to full state name in lowecase
#' @param abb state abbreviation 
#
convertName <- function(abb) {
  tolower(state.name[grep(abb, state.abb)])
}

prepData1 <- function(theMonth) {
  theDataTable <- data.table(subset(cocaine, month == theMonth))
  theDataTable <- theDataTable[,list(meanPurity=mean(potency)),by=state]
  avgPurityByState <- completeDF(theDataTable, names(theDataTable)[2])
}

prepData2 <- function(theMonth) {
  theDataTable <- data.table(subset(cocaine, month == theMonth))
  theDataTable <- theDataTable[,list(totalWeight=sum(weight)),by=state]
  totalWeightByState <- completeDF(theDataTable, names(theDataTable)[2])
}

prepData3 <- function(theMonth) {
  theDataTable <- data.table(subset(cocaine, month == theMonth))
  theDataTable <- theDataTable[,list(totalValue=sum(price)),by=state]
  totalValueByState <- completeDF(theDataTable, names(theDataTable)[2])
}

# converts all state abbreviations in the data set to lowercase full state names
cocaine$state <- as.character(sapply(cocaine$state, convertName)) 

########################################################
########################################################

# Shiny server 
shinyServer(function(input, output, session) {
  
   # prepares dataset with average cocaine purity by state
    avgPurityByState <- reactive({
       prepData1(input$theMonth)
    })

    # prepares dataset with total weigth by state
    totalWeightByState <- reactive({
       prepData2(input$theMonth)
    })
    
    # prepares dataset with total value by state
    totalValueByState <- reactive({
       prepData3(input$theMonth)
    })
    
    
    # render plots
    
    # plots average cocaine purity by state
    output$purity <- renderPlot({
       print(plotByState (
          dt =  avgPurityByState(),
          states_map = states_map, 
          title = "Average purity (%) by state",
          fill = "meanPurity"
       ))
    })
    
    # total weigth by state
    output$weight  <- renderPlot({
       print(plotByState (
         dt =  totalWeightByState(),
         states_map = states_map, 
         title = "Total seizure weight (grams) by state",
         fill = "totalWeight"
       ))
    })
    
    # total total value by state
    output$value <- renderPlot({
       print(plotByState (
         dt =  totalValueByState(),
         states_map = states_map, 
         title = "Total seizure value (US$) by state",
         fill = "totalValue"
       ))
    })

})
