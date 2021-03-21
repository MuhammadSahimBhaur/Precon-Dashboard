library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(shinycssloaders)


options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

ui <- dashboardPage(
  dashboardHeader(title = "Precon"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      tabItem(tabName = "dashboard",
              
              box(
                title = "Controls",
                sliderInput("slider", "Number of observations:", min = 1, max = 1000, value = c(1,500) ),
                
                selectInput("variable", "Variable:",
                            c("House 1" = "1",
                              "House 2" = "2",
                              "House 3" = "3")),
                h4(textOutput("q")),
              ),
              
              
              
              fluidRow(
                
                box(
                  title = h1("Energy Consumption"),
                  width = 12,
                  withSpinner(plotlyOutput("trend"), type = 5)
                ),
                
                # box(
                #   title = h1("Statistics Energy Consumption"),
                #   plotlyOutput("boxplot"), width = 12
                # ),
                # box(plotlyOutput("boxplot"))
                # plotlyOutput("piechart")
                
                tabBox(
                  title = "Electricty Usage Statistics",
                  id = "tabset1", width = 12,
                  tabPanel("Hourly", "12 Hour Electricity Usage",withSpinner(plotlyOutput("boxplot_hourly"), type = 5)),
                  tabPanel("Daily", "Daily electricty usage",withSpinner(plotlyOutput("boxplot_Daily"), type = 5))
                )
              )
      ), #closing dashbaord tab
      
      
      
      tabItem(tabName = "widgets",h2("Widgets tab content"))
      
    )
  )
)