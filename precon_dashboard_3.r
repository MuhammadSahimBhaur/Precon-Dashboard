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

server <- function(input, output, session){
  
  r <- reactiveValues(
    dataframe = NULL,
    start = 1,
    end = 500
  )
  
  output$q = renderText({ paste("House",input$variable,"is selected!") })
  
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
    # str(data)
    
    
    fig = plot_ly(data, type = 'pie')
    fig = fig %>% layout(title = 'Electricity Usage',
                         xaxis = list(showgrid = FALSE),
                         yaxis = list(showgrid = FALSE))
    
    fig
    
  })
  
  
}

shinyApp(ui, server)