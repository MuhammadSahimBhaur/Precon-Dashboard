library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(shinycssloaders)




server <- function(input, output, session){
  
  r <- reactiveValues(
    doPlot = FALSE,
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
  
  # output$picture1 <-
  #   renderText({
  #     c(
  #       '<img src="',
  #       "data:image/jpeg;base64,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",
  #       '">'
  #     )
  #   })
  
  output$picture1 <-
    renderText({
      c(
        '<img src="locale.png" , width = "100%", height = "100%" >'
      )
    })
  
  output$clustering_picture <-
    renderText({
      c(
        '<img src="clustering.jpeg" , width = "60%", height = "60%">'
      )
    })

  
  # 
  # output$picture2 <-
  #   renderText({
  #     c(
  #       '<img src="',
  #       "data:image/jpeg;base64,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",
  #       '">'
  #     )
  #   })

  
  observeEvent(input$run, {
    # 0 will be coerced to FALSE
    # 1+ will be coerced to TRUE
    temp <<- input$run
    r$doPlot <- input$run
  })
  
  # observeEvent(input$tabset, {
  #   r$doPlot <- FALSE
  # })  
  
  
 
  
  
  observe({
    start = input$slider[1]
    end = input$slider[2]
    filename = paste("House" , input$variable , ".csv",sep="")
    dataframe = read.csv(file=filename, sep=",")[,1:2]
    
    # 
    dataframe$Date_Time = as.POSIXct(dataframe$Date_Time)
    # print(input$dateRange)
    dataframe = dataframe[dataframe$Date_Time>=as.POSIXct(format(input$dateRange[1])) & dataframe$Date_Time<=as.POSIXct(format(input$dateRange[2])),]
    df_xts = xts(dataframe, order.by = dataframe$Date_Time)
    # df_xts = df_xts[input$start_date:input$end_date]
    df_xts$Date_Time = NULL
    
    
    tempdf = NULL
    switch(input$gran,
           mins={
             tempdf = df_xts
           },
           hrs={
             tempdf = period.apply(df_xts, endpoints(df_xts, "hours"), mean)
           },
           days={
             tempdf = apply.daily(df_xts,FUN=mean)
           },
           mon={
             tempdf = apply.monthly(df_xts,FUN=mean)
           })
    
    df = as.data.frame(tempdf)
    df = cbind(Date_Time = rownames(df), df)
    rownames(df) <- 1:nrow(df)
    
    # r$finaldf = as.data.frame(df)
    # 
    # print(head( r$finaldf ))
    
    r$dataframe = df
    
    
    
  })
  
  
  output$trend = renderPlotly({
    if (r$doPlot == FALSE) return()
    
    isolate({
    
    data = r$dataframe
    # str(data)
    fig = plot_ly(x = as.POSIXct(data$Date_Time), y = data$Usage_kW, type = 'scatter', mode = 'lines')
    fig = fig %>% layout(xaxis = list(title = 'Date Time',
                                      zeroline = TRUE
    ),
    yaxis = list(title = 'Electrictiy Consumptin (kW)'
    ))
    
    fig
    
    })
  })
  
  
  
  output$bubbleplot = renderPlotly({
    if (r$doPlot == FALSE) return()
    
    isolate({
    
    data = r$dataframe
    # str(data)
    
    
    fig = plot_ly(data,x = as.POSIXct(data$Date_Time), y = data$Usage_kW, type = 'scatter', mode = 'markers',   colors = 'Reds',
                  marker = list(size = data$Usage_kW, opacity = 0.5))
    fig = fig %>% layout(title = 'Electricity Usage',
                         xaxis = list(showgrid = FALSE),
                         yaxis = list(showgrid = FALSE))
    
    fig
    
    })
    
  })
  
  output$boxplot_Daily = renderPlotly({
    if (r$doPlot == FALSE) return()
    
    data = r$dataframe
    
    grp_by_date = data %>% group_by(Date_Time)
    data$Date_Time = as.Date(data$Date_Time)
    # data$Date_Time = as.POSIXct(data$Date_Time)
    # print(class(data$Date_Time))
    ggplot(data, aes(x = Date_Time, y = Usage_kW, group= 1)) + geom_boxplot() +
      labs(x = "Date", y = "Daily Electrcity consumption (kW)")
    
  })
  
  output$boxplot_hourly = renderPlotly({
    if (r$doPlot == FALSE) return()
    
    data = r$dataframe
    
    grp_by_date = data %>% group_by(Date_Time)
    # data$Date_Time = as.Date(data$Date_Time)
    data$Date_Time = as.POSIXct(data$Date_Time)
    # print(class(data$Date_Time))
    ggplot(data, aes(x = Date_Time, y = Usage_kW, group= 1)) + geom_boxplot() +
      labs(title = "Daily Electrcity Usage",
           x = "Date", y = "Daily Electrcity consumption (kW)")
    
  })
  
  
  # output$piechart = renderPlotly({
  #   if (r$doPlot == FALSE) return()
  #   
  #   data = r$dataframe
  #   data$Date_Time = as.Date(data$Date_Time)
  #   month_data = data %>%
  #     mutate(month = format(Date_Time, "%m")) %>%
  #     group_by(month)
  #   # print(unique(month_data['month']))
  #   
  #   data = data %>%
  #     mutate(percentage_use = (Usage_kW/sum(data$Usage_kW)) * 100 ) %>%
  #     mutate(months = format(Date_Time, "%m")) %>%
  #     mutate(MonthName = month.name[as.integer(format(Date_Time, "%m"))])
  #   # str(data)
  #   
  #   ggplot(data, aes(x="", y=percentage_use, fill =MonthName ))+
  #     geom_bar(stat="identity", width = 1)+
  #     cord_plot("y")
  #  
  #   
  # })

  output$barchart = renderPlotly({
    if (r$doPlot == FALSE) return()
    
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
    if (r$doPlot == FALSE) return()
    
    
    data <- data.frame(latitude=c(37.78,24.77,21.56,24.77,21.56,21.56,21.47,21.48,26.39,24.77),
                       longitude=c(-100,46.74,39.19,46.74,39.2,39.2,39.23,39.19,49.98,46.74),
                       )
    
    pal <- colorFactor(
      palette = 'Blues')
    
    leaflet(data) %>% addTiles() %>%
      addCircleMarkers(lat = ~latitude, lng = ~longitude,)
  })
  
  
}