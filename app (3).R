library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(h2o)
library(corrplot)
bikeDataSet <- "./SeoulBikeData.csv"
bikeDf <- read.table(file = bikeDataSet, header = T, sep = ',')
bikeDf$Seasons <- as.factor(bikeDf$Seasons)
bikeDf$Holiday <- as.factor(bikeDf$Holiday)
bikeDf$Functioning.Day <- as.factor(bikeDf$Functioning.Day)
bikeDf$Hour <- as.factor(bikeDf$Hour)
#plot(10:1)
names(bikeDf) <- c("Date","RentedBikeCount","Hour","Temperature","Humidity","WindSpeed",
                   "Visibility","DewPointTemperature","SolarRadiation","Rainfall","Snowfall",
                   "Seasons","Holiday","FunctioningDay")
bikeDf <- cbind(bikeDf,Weekday=weekdays(as.POSIXlt(bikeDf$Date),abbreviate=FALSE))
bikeDf <- cbind(bikeDf,Month=months(as.POSIXlt(bikeDf$Date),abbreviate=FALSE))
bikeDf$Year <- format(as.Date(bikeDf$Date, format="%d/%m/%Y"),"%Y")

#load model
h2o.init(nthreads=1, max_mem_size="4g")
model_path <- "./StackedEnsemble_AllModels_AutoML_20210507_013659"
h2o_model <- h2o.loadModel(model_path)

ui <- dashboardPage(
  
  #Dashboard title
  dashboardHeader(title = 'BIKE SHARING EXPLORER', titleWidth = 290),
  
  #Sidebar layout
  dashboardSidebar(width = 290,
                   sidebarMenu(
                     menuItem("Plots", tabName = "plots", icon = icon('poll')),
                     menuItem("Dashboard", tabName = "dash", icon = icon('tachometer-alt')),
                     menuItem("Prediction", tabName = "pred", icon = icon('search')))),
  #Tabs layout
  dashboardBody(tags$head(tags$style(HTML('.main-header .logo {font-weight: bold;}'))),
                #Plots tab content
                tabItems( tabItem('plots', 
                                  #Histogram filter
                                  box(status = 'primary', title = 'Filter for the histogram plot',
                                      selectInput('num', "Numerical variables:", c("Temperature","Humidity","WindSpeed",
                                                                                   "Visibility","DewPointTemperature","SolarRadiation","Rainfall","Snowfall")),
                                      footer = 'Histogram plot for numerical variables'),
                                  #Frecuency plot filter
                                  box(status = 'primary', title = 'Filter for the frequency plot',
                                      selectInput('cat', 'Categorical variables:', c("Seasons","Holiday","FunctioningDay","Hour")),
                                      footer = 'Frequency plot for categorical variables'),
                                  #Boxes to display the plots
                                  fluidRow(
                                    box(title = "Frequency plot of numeric variables"        ,
                                        status = "primary"        ,
                                        solidHeader = TRUE        ,
                                        collapsible = TRUE        ,plotOutput('histPlot',height="400px")),
                                    box(title = "Frequency plot of Categorical variables"        ,
                                        status = "primary"        ,
                                        solidHeader = TRUE        ,
                                        collapsible = TRUE        ,plotOutput('freqPlot',height="400px"))),
                                  fluidRow(
                                    box(width=12,title = "Correlation Matrix"        ,
                                        status = "primary"        ,
                                        solidHeader = TRUE        ,
                                        collapsible = TRUE        ,plotOutput('corrPlot',height = "700px"))),
                                  fluidRow(
                                    box(width=12,title = "Monthly avg demand(hourly)"        ,
                                        status = "primary"        ,
                                        solidHeader = TRUE        ,
                                        collapsible = TRUE        ,plotOutput('monthPlot',height = "700px")))
                ),
                #Dashboard tab content
                tabItem('dash',
                        #Dashboard filters
                        fluidRow(
                          box(title = 'Hourly Bike Sharing demand', 
                              status = 'primary', width = 12,
                              splitLayout(cellWidths = c('4%', '42%', '40%'),
                                          div(),
                                          radioButtons( 'year', 'Year:', c('2017 and 2018', '2017', '2018')),
                                          radioButtons( 'split', 'split data by:', c('Season', 'Week', 'Month','Holiday'))))),
                        #Boxes to display the plots
                        fluidRow(
                          box(options(repr.plot.width = 14, repr.plot.height = 8),
                              plotOutput('linePlot',height=500),height=600,width=12))
                ),
                
                #Prediction tab content
                tabItem('pred',
                        #Filters for categorical variables
                        fluidPage(
                          box(title = 'Categorical variables', 
                              status = 'primary', width = 12, 
                              splitLayout(
                                tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                                cellWidths = c('0%', '22%', '5%', '22%', '5%', '22%', '5%', '14%', '5%'),
                                selectInput( 'p_mnth', 'Month', c("January", "February", "March", "April", "May", "June", "July",
                                                                  "August", "September", "October", "November", "December")),
                                div(),
                                selectInput('p_hr', 'Hour', c('0', '1', '2', '3', '4', '5', '6', '7', '8','9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23')),
                                div(),
                                selectInput( 'p_weekd', 'Weekday', c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')),
                                #div(),
                                #selectInput( 'p_weather', 'Weather', c('Good', 'Fair', 'Bad', 'Very Bad')),
                                #div(),
                                radioButtons( 'p_holid', 'Holiday', c('Yes', 'No')),radioButtons( 'p_func', 'FunctioningDay', c('Yes', 'No')) ))),
                        #Filters for numeric variables
                        fluidPage(
                          box(title = 'Numerical variables',
                              status = 'primary', width = 12,
                              splitLayout(cellWidths = c('22%', '4%','21%', '4%', '21%', '4%', '21%','4%', '21%', '4%', '21%','4%', '21%'),
                                          sliderInput( 'p_hum', 'Humidity (%)', min = 0, max = 100, value = 0),
                                          div(),
                                          numericInput( 'p_temp', 'Temperature (Celsius)', 0),
                                          div(),
                                          numericInput( 'p_visib', 'Visibility(10m)', 0),
                                          div(),
                                          numericInput( 'p_wind', 'Wind speed (mps)', 0)),
                              div(),
                              numericInput( 'p_solar', 'SolarRadiation', 0),
                              #sliderInput( 'p_solar', 'SolarRadiation', min = 0, max = 4, value = 0),
                              div(),
                              numericInput( 'p_rain', 'Rainfall', 0),
                              div(),
                              numericInput( 'p_snow', 'Snowfall', 0)))
                        ,
                        #Box to display the prediction results
                        fluidPage(
                          box(title = 'Prediction result',
                              status = 'success', 
                              solidHeader = TRUE, 
                              width = 4, height = 270,
                              div(h5('Total number of bikes to be rented:')),
                              verbatimTextOutput("value", placeholder = TRUE),
                              actionButton('cal','Calculate', icon = icon('calculator'))),
                        #Box to display information about the model
                          box(title = 'Model explanation',
                              status = 'success', 
                              width = 8, height = 270,
                              helpText('The following model will predict the total number of bikes rented on a specific day of the week, hour, and weather conditions.'),
                              helpText('The name of the dataset used to train the model is "Seoul Bike Sharing Dataset", taken from the UCI Machine Learning Repository website. The data contains 8760 observations and 14 attributes related to time and weather conditions.'),
                              helpText('The prediction is based on a gradient boosting regression supervised machine learning model.'))))
                )))



server <- function(input, output) {
  output$histPlot <- renderPlot({
    #ggplot(data=bikeDf, aes(x=bikeDf[[input$num]]))+geom_histogram(color="black", fill="steelblue3")+geom_text(aes(x = input$num,y = "Frequency"))
    ggplot(data = bikeDf, aes(x = bikeDf[[input$num]]))+ 
      geom_histogram(stat = "bin", fill = 'steelblue3', 
                     color = 'lightgrey')+
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 16, face = 'bold'))+
      labs(title = sprintf('Histogram plot of the variable %s', input$num),
           x = sprintf('%s', input$num),y = 'Frequency')+
      stat_bin(geom = 'text', 
               aes(label = ifelse(..count.. == max(..count..), as.character(max(..count..)), '')),
               vjust = -0.6)
  })
  
  output$freqPlot <- renderPlot({
    
    ggplot(data = bikeDf, aes(x = bikeDf[[input$cat]]))+
      geom_bar(stat = 'count', fill = 'mediumseagreen', 
               width = 0.5)+
      stat_count(geom = 'text', size = 4,
                 aes(label = ..count..),
                 position = position_stack(vjust = 1.03))+
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 16, face="bold"))+
      labs(title = sprintf('Frecuency plot of the variable %s', input$cat),
           x = sprintf('%s', input$cat), y = 'Count')
    
  })
  
  output$corrPlot <- renderPlot({
    
    numeric_df <- bikeDf[,c("RentedBikeCount","Hour","Temperature","Humidity","WindSpeed",
                            "Visibility","SolarRadiation","Rainfall","Snowfall","DewPointTemperature")]
    numeric_df$Hour <- as.numeric(numeric_df$Hour)
    
    bikeDf.corr <- cor(numeric_df)
    corrplot(bikeDf.corr)
    
  })
  
  output$monthPlot <- renderPlot({
    
    month_df <- group_by(bikeDf, Month) %>%
      summarize(m=mean(RentedBikeCount,na.rm=T))
    ggplot(data = month_df, aes(x =factor(Month,levels=month.name), y = m))+
      geom_bar(stat = 'identity', fill = 'mediumseagreen', 
               width = 0.5)+
      labs(title = sprintf('Monthly average demand (per hour'),
           x = sprintf('Month'), y = 'Average Hourly demand')
  })
  
  output$linePlot <- renderPlot({
    library(dplyr)
    if (input$split == "Week"){
      filteredBikeDf <- bikeDf %>%
        filter(Year == input$year | input$year == "2017 and 2018") %>%
        group_by(Hour,Weekday) %>%
        summarize(mean_demand = mean(RentedBikeCount))
    }
    else if (input$split == "Season"){
      filteredBikeDf <- bikeDf %>%
        filter(Year == input$year | input$year == "2017 and 2018") %>%
        group_by(Hour,Seasons) %>%
        summarize(mean_demand = mean(RentedBikeCount))
    }
    else if (input$split == "Holiday"){
      filteredBikeDf <- bikeDf %>%
        filter(Year == input$year | input$year == "2017 and 2018") %>%
        group_by(Hour,Holiday) %>%
        summarize(mean_demand = mean(RentedBikeCount))
    }
    else {
      filteredBikeDf <- bikeDf %>%
        filter(Year == input$year | input$year == "2017 and 2018") %>%
        group_by(Hour,Month) %>%
        summarize(mean_demand = mean(RentedBikeCount))
    }
    names(filteredBikeDf) <- c("Hour","Split","mean_demand")
    
    ggplot(data=filteredBikeDf, aes(x=as.numeric(Hour)-1,y=mean_demand,color=Split))+geom_point(size=2)+geom_line(size=1)+scale_x_continuous(breaks = seq(0,23,by=1),expand = c(0, 0))+scale_y_continuous(breaks = seq(250,2000,by=250),expand = c(0, 0))+
      # theme(axis.text.y = element_blank(),
      #      axis.ticks.y = element_blank(),
      #     axis.text = element_text(size = 12),
      #    axis.title = element_text(size = 14),
      #   plot.title = element_text(size = 16, face="bold"))+
      labs(title = sprintf('Hourly Demand for Bike Sharing'),
           x = sprintf('Hour of the Day'), y = 'Average Demand')
  })
  
  a <- reactiveValues(result = NULL)
  
  observeEvent(input$cal,{
    values = data.frame(Hour = input$p_hr,
                        Temperature = input$p_temp,
                        Humidity = input$p_hum,
                        WindSpeed = input$p_wind,
                        Visibility = input$p_visib,
                        SolarRadiation = input$p_solar,
                        Rainfall = input$p_rain,
                        Snowfall = input$p_snow,
                        Holiday = ifelse(input$p_holid == "No","No Holiday","Holiday"),
                        Weekday = input$p_weekd,
                        FunctioningDay = input$p_func,
                        Month = input$p_mnth)
    
    a$result <- round(h2o.predict(h2o_model,as.h2o(values)))
    
  })
  
  output$value <- renderText({
    #Display the prediction value
    paste(as.data.frame(a$result))
  })
}

shinyApp(ui = ui, server = server)
