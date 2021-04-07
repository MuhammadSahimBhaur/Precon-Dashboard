library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(shinycssloaders)


options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=1)

ui <- dashboardPage(
  dashboardHeader(title = "Precon DataSet"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")), 
      menuItem("Precon Dashbaord", tabName = "dashboard", icon = icon("chart-area",class=("fas fa")) ), #<i class="fas fa-chart-area"></i>
      menuItem("Clustering", tabName = "widgets", icon = icon("th")),
      menuItem("About Us", tabName = "widgets", icon = icon("users", class=("fas fa")) ), #<i class="fas fa-users"></i>
      menuItem("Contact Us", tabName = "widgets", icon = icon("address-card",class=("far fa"))) #<i class="far fa-address-card"></i>
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      tabItem(tabName = "dashboard",
              
        #### Controls ####   
        fluidRow(
          
          
          box(
            selectInput("variable", label = h4(textOutput("q")),
                        c("House 1" = "1",
                          "House 2" = "2",
                          "House 3" = "3")),
            ),
          
          box(
            title = "Control Observations",
            sliderInput("slider",label=NULL, min = 1, max = 10000, value = c(1,500) ),
            ),
              

              
        ),
              
        #### Controls End ####      
            
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
          ),
          box(
            title = h1("Monthly Energy Consumption"),
            width = 12,
            withSpinner(plotlyOutput("barchart"), type = 5)
          ),
          # box(
          #   title = h1("Locations"),
          #   width = 12,
          #   withSpinner(plotlyOutput("location"), type = 5)
          # ),
              )
              
      ), #closing dashbaord tab
      
      
      tabItem(tabName = "widgets",h2("Widgets tab content")),
      
      
      tabItem(tabName = "home",(
        box(
        htmlOutput("w"))
          )
                                  )
      
    )
  )
)

