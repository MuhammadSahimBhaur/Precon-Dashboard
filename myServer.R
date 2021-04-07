library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(shinycssloaders)




server <- function(input, output, session){
  
  r <- reactiveValues(
    dataframe = NULL,
    start = 1,
    end = 500
  )
  
  output$q = renderText({ paste("House",input$variable,"is selected!") })
  
  output$w = renderUI({ HTML("<p>The PRECON data set contains electricity
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
market.</p>") })
  
  observe({
    start = input$slider[1]
    end = input$slider[2]
    filename = paste("House" , input$variable , ".csv",sep="")
    r$dataframe = read.csv(file=filename, sep=",")[start:end,1:2]
  })
  
  
  output$trend = renderPlotly({
    
    data = r$dataframe
    # str(data)
    fig = plot_ly(x = as.POSIXct(data$Date_Time), y = data$Usage_kW, type = 'scatter', mode = 'lines+markers')
    fig = fig %>% layout(xaxis = list(title = 'Date Time',
                                      zeroline = TRUE
    ),
    yaxis = list(title = 'Electrictiy Consumptin (kW)'
    ))
    
    fig
  })
  
  
  
  output$bubbleplot = renderPlotly({
    
    data = r$dataframe
    # str(data)
    
    
    fig = plot_ly(data,x = as.POSIXct(data$Date_Time), y = data$Usage_kW, type = 'scatter', mode = 'markers',   colors = 'Reds',
                  marker = list(size = data$Usage_kW, opacity = 0.5))
    fig = fig %>% layout(title = 'Electricity Usage',
                         xaxis = list(showgrid = FALSE),
                         yaxis = list(showgrid = FALSE))
    
    fig
    
  })
  
  output$boxplot_Daily = renderPlotly({
    
    data = r$dataframe
    
    grp_by_date = data %>% group_by(Date_Time)
    data$Date_Time = as.Date(data$Date_Time)
    # data$Date_Time = as.POSIXct(data$Date_Time)
    print(class(data$Date_Time))
    ggplot(data, aes(x = Date_Time, y = Usage_kW, group= 1)) + geom_boxplot() +
      labs(x = "Date", y = "Daily Electrcity consumption (kW)")
    
  })
  
  output$boxplot_hourly = renderPlotly({
    
    data = r$dataframe
    
    grp_by_date = data %>% group_by(Date_Time)
    # data$Date_Time = as.Date(data$Date_Time)
    data$Date_Time = as.POSIXct(data$Date_Time)
    print(class(data$Date_Time))
    ggplot(data, aes(x = Date_Time, y = Usage_kW, group= 1)) + geom_boxplot() +
      labs(title = "Daily Electrcity Usage",
           x = "Date", y = "Daily Electrcity consumption (kW)")
    
  })
  
  
  output$piechart = renderPlotly({
    
    data = r$dataframe
    data$Date_Time = as.Date(data$Date_Time)
    month_data = data %>%
      mutate(month = format(Date_Time, "%m")) %>%
      group_by(month)
    # print(unique(month_data['month']))
    
    data = data %>%
      mutate(percentage_use = (Usage_kW/sum(data$Usage_kW)) * 100 ) %>%
      mutate(months = format(Date_Time, "%m")) %>%
      mutate(MonthName = month.name[as.integer(format(Date_Time, "%m"))])
    # str(data)
    
    ggplot(data, aes(x="", y=percentage_use, fill =MonthName ))+
      geom_bar(stat="identity", width = 1)+
      cord_plot("y")
   
    
  })
  
  output$barchart = renderPlotly({
    data = r$dataframe
    data$Date_Time = as.Date(data$Date_Time)
    month_data = data %>%
      mutate(month = format(Date_Time, "%m")) %>%
      group_by(month)
    # print(unique(month_data['month']))
    
    data = data %>%
      mutate(percentage_use = (Usage_kW/sum(data$Usage_kW)) * 100 ) %>%
      mutate(total = sum(data$Usage_kW) ) %>%
      mutate(months = format(Date_Time, "%m")) %>%
      mutate(MonthName = month.name[as.integer(format(Date_Time, "%m"))])
    # str(data)
     p <- ggplot(data, aes(x=MonthName, y=total ))+
        geom_bar(stat="identity", fill ="steelblue")+
        theme_minimal()
     
     p
  }
    
  )
  
  output$location = renderPlotly({
    data <- data.frame(latitude=c(37.78,24.77,21.56,24.77,21.56,21.56,21.47,21.48,26.39,24.77),
                       longitude=c(-100,46.74,39.19,46.74,39.2,39.2,39.23,39.19,49.98,46.74),
                       )
    
    pal <- colorFactor(
      palette = 'Blues')
    
    leaflet(data) %>% addTiles() %>%
      addCircleMarkers(lat = ~latitude, lng = ~longitude,)
  })
  
  
}