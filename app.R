library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(shinycssloaders)
library(rsconnect)

# options(  spinner.color = "#0275D8",spinner.color.background = "#ffffff",  spinner.size = 0.5)


ui <- dashboardPage(
  

  skin = "black",
  dashboardHeader(title = "Precon DataSet"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem(
        "Precon Dashbaord",
        tabName = "dashboard",
        icon = icon("chart-area", class = ("fas fa"))
      ),
      menuItem("Clustering", tabName = "cluster", icon = icon("th")),
      menuItem(
        "About Us",
        tabName = "aboutus",
        icon = icon("users", class = ("fas fa"))
      ),
      menuItem(
        "Contact Us",
        tabName = "contactus",
        icon = icon("address-card", class = ("far fa"))
      )
    )
  ),
  
  dashboardBody(tabItems(
    # tabItem(tabName = "home", (fluidPage(
    #   fluidRow(column(
    #     width = 12, offset = 3,
    #     box(htmlOutput("picture1"))
    #   )),
    #   
    #   fluidRow(column(
    #     width = 12, offset = 3,
    #     box(htmlOutput("w"))
    #   ))
    # ))),
    
    
    tabItem(tabName = "home", (fluidPage(
      fluidRow(column(
        width = 12, offset = 2, box(width= 8,height = 720,htmlOutput("picture1"),
      # fluidRow(column(
      #   width = 12, offset = 3,
        htmlOutput("w")
      ))),
      ))
      
    ),
    
    tabItem(tabName = "dashboard",
            
            
            fluidRow(
              # column(12,
              box(
                width = 4,
                selectInput(
                  "variable",
                  label = "Select a house",
                  c(
                    "House 1" = "1",
                    "House 2" = "2",
                    "House 3" = "3"
                  )
                ),
                selectInput(
                  "gran",
                  label = "Granularity",
                  selected = "hrs",
                  c(
                    "Minutes" = "mins",
                    "Hours" = "hrs",
                    "Days" = "days",
                    "Months" = "mon"
                  )
                ),
                dateRangeInput(
                  'dateRange',
                  label = "Date Range",
                  start = as.POSIXct("2018-07-02"),
                  end = as.POSIXct("2018-12-10")
                ),
                
                actionButton("run", "Run")
              ),
              
              
              box(
                title = "Energy Consumption",
                height = 290,
                width = 8,
                plotlyOutput("trend")
              )
            ),),
    
    
    tabItem(tabName = "cluster", (fluidPage(fluidRow(
      column(
        width = 12,
        offset = 2,
        box(
          width = 8,
          height = 750,
          htmlOutput("clustering_picture"),
          htmlOutput("e")
        )
      )
    ))))
    
)
)
  
)

server <- function(input, output, session) {
  r <- reactiveValues(dataframetemp = NULL,
                      dataframe = NULL,
                      doPlot = FALSE)
  
  output$picture1 <-
    renderText({
      c(' <img src="locale.png" , width = "100%", height = "100%" > <h2>Introduction<h2>')
      
    })
  
  output$w = renderUI({
    HTML(
      "<p>The PRECON data set contains electricity
                consumption patterns of 42 households over a
                time period of one year. The data of the whole
                house is recorded including the consumption of
                high-powered devices and major areas of the
                house. The aim of collecting this data is to
                deeply. understand the residential electricity
                consumption profiles of households in
                developing countries where the energy market is
                flourishing.</p>
                <p>This dataset presents electricity consumption
                data of 42 households, recorded at a minute
                interval. For each day, data is stored in an
                individual CSV file for each household. Each
                file contains 1440 rows corresponding to each
                minute of the day and a varying number of
                columns. The number of columns varies because
                for each different household a different number
                of appliances are selected for monitoring. Other
                than electricity consumption data, several
                attributes related to the households are also
                recorded.</p>
                <p>PRECON dataset is the first attempt to collect
                extensive residential energy consumption
                information from South Asia in particular
                Pakistan, and hence can be used in
                understanding important facts about the energy
                market.</p>"
    )
  })
  
  output$e = renderUI({
    HTML(
      "
            
      <p>
      We replicated the paper \"Clustering of Electricity
Consumption Behavior Dynamics Toward Big
Data Applications,\" which is used as the parent paper
for this senior year project since it proposes a
distributive clustering approach for large data
sets. The paper uses a publicly available
electricity consumption data set compiled
Research Perspective Ltd. and proposes an
algorithm to improve the computation of Fast
search and density peak clustering (CFSFDP) on
large (electricity) data sets. 
      
      </p>
      
      <p> The proposed
algorithm uses a Distributed approach and
couples adaptive k-means with existing
CFSFDP model. The authors compare the time
taken by applying CFSFDP on the complete data
set independently with using the distributive
approach. Results show that applying CFSFDP
directly on the data set is computationally 
expensive and more time consuming as
compared to the distributive algorithm. Since the
results produced by both the algorithms are
similar using the authors recommend using the
distributive approach on large electricity
datasets. </p>
      
      <p>
      
      
      </p>
      
      <p>The number of
clusters generated are 15 as shown by the
decision graph. The colored dots represent the
cluster centers while the remaining grey data
points are the other neighbouring points. The
cluster centers are chosen depending on the local
density and minimum distance of the points. A
point having a higher local density and
minimum distance is the cluster center. The total
time taken for running the clustering algorithm
on the whole data was 13.31368637084961
seconds.</p>
      
      <p>
      
      
      </p>
      "
    )
  })
  
  
  output$clustering_picture <-
    renderText({
      c('<img src="clustering.jpeg" , width = "100%", height = "100%"> <h2>Implemention of Distributive clustering</h2> ')
    })
  
  observeEvent(input$run, {
    # 0 will be coerced to FALSE
    # 1+ will be coerced to TRUE
    temp <- input$run
    r$doPlot <- input$run
  })
  
  observe({
    filename = paste("House" , input$variable , ".csv", sep = "")
    r$dataframetemp = read.csv(file = filename, sep = ",")[, 1:2]
  })
  
  observe({
    if (r$doPlot == FALSE)
      return()
    isolate({
      dataframe = r$dataframetemp
      dataframe$Date_Time = as.POSIXct(dataframe$Date_Time)
      dataframe = dataframe[dataframe$Date_Time >= as.POSIXct(format(input$dateRange[1])) &
                              dataframe$Date_Time <= as.POSIXct(format(input$dateRange[2])), ]
      df_xts = xts(dataframe, order.by = dataframe$Date_Time)
      df_xts$Date_Time = NULL
      
      
      tempdf = NULL
      switch(
        input$gran,
        mins = {
          tempdf = df_xts
        },
        hrs = {
          tempdf = period.apply(df_xts, endpoints(df_xts, "hours"), mean)
        },
        days = {
          tempdf = apply.daily(df_xts, FUN = mean)
        },
        mon = {
          tempdf = apply.monthly(df_xts, FUN = mean)
        }
      )
      
      df = as.data.frame(tempdf)
      df = cbind(Date_Time = rownames(df), df)
      rownames(df) <- 1:nrow(df)
      r$dataframe = df
    })
  })
  
  output$trend = renderPlotly({
    if (r$doPlot == FALSE)
      return()
    
    isolate({
      data = r$dataframe
      # str(data)
      fig = plot_ly(
        x = as.POSIXct(data$Date_Time),
        y = data$Usage_kW,
        type = 'scatter',
        mode = 'lines',
        height = 230
      )
      fig = fig %>% layout(
        xaxis = list(title = 'Date Time',
                     zeroline = TRUE),
        yaxis = list(title = 'Electrictiy Consumptin (kW)')
      )
      
      fig
      
    })
  })
  
}

shinyApp(ui, server)