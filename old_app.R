library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(shinycssloaders)
library(rsconnect)
library(xts)



source('myUI.R', local = TRUE)
source('myServer.R')

shinyApp(ui, server)