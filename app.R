library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(shinycssloaders)
library(rsconnect)
library(leaflet)
library(xts)

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
      )
    )
  ),
  
  dashboardBody(tabItems(

    tabItem(tabName = "home", (fluidPage(
      fluidRow(column(
        width = 12, offset = 2, box(width= 8,height = 920,htmlOutput("picture1"),
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
                  start = as.POSIXct("2018-07-16"),
                  end = as.POSIXct("2018-09-20")
                ),
                
                actionButton("run", "Run")
              ),
              
              
              box(
                title = "Energy Consumption",
                height = 290,
                width = 8,
                plotlyOutput("trend")
              )
            ),
            
            fluidRow(
              box(title = "Daily Consumption Boxplot",width = 12,withSpinner( plotlyOutput("boxplot_daily") )
            ),
            ),
            fluidRow(
              box(width = 12,title = "Map Locations",leafletOutput("location")
              )
            )
            
            ),
    # ),
    
    tabItem(tabName = "cluster", (fluidPage(fluidRow(
      column(
        width = 12,
        offset = 2,
        box(
          width = 8,
          height = 1100,
          htmlOutput("clustering_picture"),
          htmlOutput("e")
        )
      )
    )))),
    
    tabItem(tabName = "aboutus", (fluidPage(fluidRow(
      column(
        width = 12,
        offset = 2,
        box(
          width = 8,
          height = 750,
          # htmlOutput(""),
          htmlOutput("r")
        )
      )
    ))))
    
)
)
  
)

server <- function(input, output, session) {
  r <- reactiveValues(dataframetemp = NULL,
                      dataframe_daterange = NULL,
                      dataframe = NULL,
                      doPlot = FALSE)
  
  output$picture1 <-
    renderText({
      c(' <img src="locale.jpg" , width = "100%", height = "100%" > ')
      
    })
  
  output$w = renderUI({
    HTML(
      "
      <h2>Introduction to Precon</h2>
      <p>The PRECON data set contains electricity
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
                market.</p>
            <h2> Precon Dashboard </h2>
            <p> This dashboard intends to help present the precon data 
            more clearly by making it easily available and accessible in an interactive manner. </p>
      
      "

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

<h2> Distributive Approach </h2>
<p> the distributive algorithm
was applied on the data set at hand. In order to
apply the distributive approach, the data set was
clustered using 15-means and the violating
clusters were found. The threshold set for
violating cluster was 10% of the minimum
squared distance of any point to its cluster. 2-
means was applied to the violating clusters and

the total clusters were updated accordingly. As a
result, a total of 30 clusters were formed with
the cluster centers representation all the other
points. Using the cluster centers, CFSFPD
clustering was performed and the corresponding
plots were generated. </p>

<p> The final results generated matched the results
produced by merely applying CFSFDP on the
whole data set. The number of clusters created
by the CFSFDP were 24 and the total time taken
was 11.537638664245605 seconds. The number
of clusters can be controlled by changing the
threshold of local density and minimum distance
which was set to their respective mean values.
</p>
      <h2>
      Limitations and Future Work
      
      </h2>
      
      <p> Due to the small size of the PRECON dataset it becomes difficult to apply the distrubtive algorithm and observe significant changes in time taken to compute the clustering.
     In the future we would like to implement the Distributive CFSFDP algorithm on a larger Dataset that is from the developing regions of the world. 
      </p>
      "
    )
  })
  
  output$r = renderUI({
    HTML(
      "
      <h2> Electricity Informatics Group @ LUMS</h2>
      
      <p>
      The Energy Informatics Group (EIG) carries out interdisciplinary research in
						the area of renewable energy analytics, smart grids, and energy efficiency.
					</p>	
				<p>		The broader goal of EIG is to help Pakistan use 100% renewable sources for
						generating electricity by 2050. To this end, the group carries out research in
						short, medium and long term forecasting of energy demand, renewable energy
						generation forecasting for wind and solar resources, demand side management
						in agricultural, residential and industrial sectors, energy efficiency and
						renewable energy integration in already built environments, improving energy
						distribution through soft load shedding, detecting non-technical losses in
						energy distribution systems and other related topics. EIG also works with the
						governmental agencies to develop evidence-based policy recommendations for
						long term renewable energy plan for Pakistan.
      </p>
      
      <h2>
      Precon Dataset
      </h2>
      
      <p>
      PRECON is a first of its kind extensive dataset of electricity consumption patterns of users in developing countries. The dataset has been collected through smart meters over a period of one year and comprise of data of users belonging to different demographics and different social and financial backgrounds. It provides details of electricity consumption data and meta-data of houses opted for data collection. Also, we have provided details on high electricity consumption devices and the load profile of the whole house. The aim of this data collection and processing exercise is to understand the electricity consumption patterns of users in the developing world. A sound realization of consumption patterns can help in the development of intelligent smart grids and better demand-side management tools.</p>

<p> PRECON is unique from the datasets of its kind because it is only extensive dataset collected over a period of one year which is publically available. Other than that it is different in terms that it monitors a greater number of households than any other similar dataset of developing countries.
      </p>
      
      <h2>
      Contact Us:
      </h2>
      Muhammad Sahim Bhaur: <a href=\"m.sahimbhaur@gmail.com\">m.sahimbhaur@gmail.com</a><br>
      Taha Razzaq: <a href=\"taharazzaq091@gmail.com\">taharazzaq091@gmail.com</a>
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
      r$dataframe_daterange = dataframe
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
  
  output$boxplot_daily = renderPlotly({
    if (r$doPlot == FALSE) return()
    
    isolate({
      
      data = r$dataframe_daterange
      # print(head(data))
      
      data$Date_Time = as.Date(data$Date_Time)
      
      ggplot(data, aes(x = Date_Time, y = Usage_kW, group= 1)) + geom_boxplot() + labs(x = "Date", y = "Daily Electrcity consumption (kW)")
    
    })
    
  })
  
  output$location = renderLeaflet({
     
    latitude=c(31.582045,31.470435)
    longitude=c(74.329376,74.409170)
    
    # geocode <- data.frame(latitude,longitude)
    
    geocode <- read.csv(file = "longlat.csv", sep = ",")[,3:4]
    
    
    
    
    m <- leaflet(option=leafletOptions(maxZoom = 15)) %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      
      addCircles(lng= geocode$Lng, lat=geocode$Lat,
        radius = 250,
        stroke = FALSE, fillOpacity = 0.5
      )
    m
    
  })
  
}

shinyApp(ui, server)