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
      menuItem("Clustering", tabName = "cluster", icon = icon("th")),
      menuItem("About Us", tabName = "aboutus", icon = icon("users", class=("fas fa")) ), #<i class="fas fa-users"></i>
      menuItem("Contact Us", tabName = "contactus", icon = icon("address-card",class=("far fa"))) #<i class="far fa-address-card"></i>
    )
  ),
  
  dashboardBody(
    
    # tags$style(
    #   HTML('
    #      #center {
    #      display: flex;
    #      align-items: center;
    #      justify-content: center;
    #      }
    # 
    #      #fluidrow1 {
    #     height:50px;
    #      }
    # 
    #      ')
    # ),
    
    tabItems(
      
      tabItem(tabName = "home",(
        fluidPage(
          fluidRow(
            column(width=12,offset = 3,
                   box(
                     htmlOutput("picture1")
                   )
            )),

          fluidRow(
            column(width=12,offset = 3,
                   box(
                     htmlOutput("w"))
            )
          )
        )

      )
      ),
      
      
      tabItem(tabName = "dashboard",
              
        #### Controls #### 
        column(width=8,
        fluidRow(
          
          
          box(
            selectInput("variable", label = h4("Select a house"),
                        c("House 1" = "1",
                          "House 2" = "2",
                          "House 3" = "3")),
            # ),

          # box(
            selectInput("gran", label = h4("Granularity"),selected="hrs",
                        c("Minutes" = "mins",
                          "Hours" = "hrs",
                          "Days" = "days",
                        "Months" = "mon")),
          ),
          # ),
        

        
          box(
            title = "Control Observations",
            # sliderInput("slider",label=NULL, min = 1, max = 10000, value = c(1,500) ),
            # dateInput("start_date", label = ("Start Date input"), value = "2018-01-01"),
            # dateInput("end_date", label = ("End Date input"), value = "2020-01-01")
            
            dateRangeInput('dateRange',
                           label = NULL,
                           start = as.POSIXct("2018-07-02"), end = as.POSIXct("2018-12-10")
            ),
            
            actionButton("run", "Run")
            
            ),
          
          
         
              

              
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
      
      
      # tabItem(tabName = "widgets",h2("Widgets tab content"))
      
      tabItem(tabName = "cluster",(
        fluidPage(
          fluidRow(
            # column(width=12,offset = 3,
                   # box(
                     htmlOutput("clustering_picture")
                   # )
            # )
          ),

          # fluidRow(
          #   column(width=12,offset = 3,
          #          box(
          #            htmlOutput("w"))
          #   )
          # )
        )
        
      )
      )
      
      

                                  )
      
    )
  )


