library(shiny)
library(dplyr)
library(shinydashboard)
library(tidyr)
library(shiny)
library(plotly)
library(forecast) # For forecasting
library(shinythemes)
library(bslib)
library(rsconnect)



# UI
ui <- fluidPage(
  
  theme = bs_theme(bootswatch = "minty"), # Apply a modern Shiny theme
  titlePanel("Interactive Air Quality and Weather Data Analysis"),
  tabsetPanel(
    # Graz
    tabPanel(
      "Graz",
      fluidRow(
        column(6, plotlyOutput("api_boxplot_graz")),
        column(6, plotlyOutput("thi_trend_graz"))
      ),
      fluidRow(
        column(6, plotlyOutput("temp_trend_graz")),
        column(6, plotlyOutput("snowfall_yearly_graz"))
      ),
      fluidRow(
        column(6, plotlyOutput("snowfall_monthly_graz")),
        column(6, plotlyOutput("api_forecast_graz"))
      )
    ),
    # Innsbruck
    tabPanel(
      "Innsbruck",
      fluidRow(
        column(6, plotlyOutput("api_boxplot_innsbruck")),
        column(6, plotlyOutput("thi_trend_innsbruck"))
      ),
      fluidRow(
        column(6, plotlyOutput("temp_trend_innsbruck")),
        column(6, plotlyOutput("snowfall_yearly_innsbruck"))
      ),
      fluidRow(
        column(6, plotlyOutput("snowfall_monthly_innsbruck")),
        column(6, plotlyOutput("api_forecast_innsbruck"))
      )
    ),
    # Linz
    tabPanel(
      "Linz",
      fluidRow(
        column(6, plotlyOutput("api_boxplot_linz")),
        column(6, plotlyOutput("thi_trend_linz"))
      ),
      fluidRow(
        column(6, plotlyOutput("temp_trend_linz")),
        column(6, plotlyOutput("snowfall_yearly_linz"))
      ),
      fluidRow(
        column(6, plotlyOutput("snowfall_monthly_linz")),
        column(6, plotlyOutput("api_forecast_linz"))
      )
    ),
    # Salzburg
    tabPanel(
      "Salzburg",
      fluidRow(
        column(6, plotlyOutput("api_boxplot_salzburg")),
        column(6, plotlyOutput("thi_trend_salzburg"))
      ),
      fluidRow(
        column(6, plotlyOutput("temp_trend_salzburg")),
        column(6, plotlyOutput("snowfall_yearly_salzburg"))
      ),
      fluidRow(
        column(6, plotlyOutput("snowfall_monthly_salzburg")),
        column(6, plotlyOutput("api_forecast_salzburg"))
      )
    ),
    # Vienna
    tabPanel(
      "Vienna",
      fluidRow(
        column(6, plotlyOutput("api_boxplot_vienna")),
        column(6, plotlyOutput("thi_trend_vienna"))
      ),
      fluidRow(
        column(6, plotlyOutput("temp_trend_vienna")),
        column(6, plotlyOutput("snowfall_yearly_vienna"))
      ),
      fluidRow(
        column(6, plotlyOutput("snowfall_monthly_vienna")),
        column(6, plotlyOutput("api_forecast_vienna"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  # Load the data
  
  data <- read.csv("dataTableEU.csv")
  # Filter data for each city
  data_graz <- data %>% filter(City == "Graz")
  data_innsbruck <- data %>% filter(City == "Innsbruck")
  data_linz <- data %>% filter(City == "Linz")
  data_salzburg <- data %>% filter(City == "Salzburg")
  data_vienna <- data %>% filter(City == "Vienna")
  # Define helper function to generate plots
  create_plots <- function(city_data, city_name) {
    # API Box Plot
    output[[paste0("api_boxplot_", city_name)]] <- renderPlotly({
      plot_ly(city_data, y = ~API, type = "box", name = "API") %>%
        layout(title = paste("API Box Plot for", city_name),
               yaxis = list(title = "API Value"))
    })
    
    # THI Linear Plot
    output[[paste0("thi_trend_", city_name)]] <- renderPlotly({
      plot_ly(city_data, x = ~as.Date(Date), y = ~THI, type = "scatter", mode = "lines", name = "THI") %>%
        layout(title = paste("THI Trend for", city_name),
               xaxis = list(title = "Date"),
               yaxis = list(title = "THI Value"))
    })
    
    # Temperature Trend
    output[[paste0("temp_trend_", city_name)]] <- renderPlotly({
      plot_ly(city_data, x = ~as.Date(Date), y = ~temperature, type = "scatter", mode = "lines", name = "Temperature") %>%
        layout(title = paste("Temperature Trend for", city_name),
               xaxis = list(title = "Date"),
               yaxis = list(title = "Temperature (°C)"))
    })
    
    # Yearly Snowfall
    output[[paste0("snowfall_yearly_", city_name)]] <- renderPlotly({
      yearly_data <- city_data %>%
        filter(temperature <= 0, precipitation > 0) %>%
        mutate(Year = format(as.Date(Date), "%Y")) %>%
        group_by(Year) %>%
        summarise(Snowfall = sum(precipitation, na.rm = TRUE))
      
      plot_ly(yearly_data, x = ~Year, y = ~Snowfall, type = "bar") %>%
        layout(title = paste("Yearly Snowfall in", city_name),
               xaxis = list(title = "Year"),
               yaxis = list(title = "Snowfall (mm)"))
    })
    
    # Monthly Snowfall
    output[[paste0("snowfall_monthly_", city_name)]] <- renderPlotly({
      monthly_data <- city_data %>%
        filter(temperature <= 0, precipitation > 0) %>%
        mutate(Month = format(as.Date(Date), "%Y-%m"),
               Month = factor(Month, levels = unique(format(as.Date(Date), "%Y-%m")))) %>%
        group_by(Month) %>%
        summarise(Snowfall = sum(precipitation, na.rm = TRUE))
      
      plot_ly(monthly_data, x = ~Month, y = ~Snowfall, type = "bar") %>%
        layout(title = paste("Monthly Snowfall in", city_name),
               xaxis = list(title = "Month"),
               yaxis = list(title = "Snowfall (mm)"))
    })
    
    # API Forecast (3 Months)
    output[[paste0("api_forecast_", city_name)]] <- renderPlotly({
      api_data <- city_data %>%
        select(Date, API) %>%
        na.omit() %>%
        mutate(Date = as.Date(Date))
      
      if (nrow(api_data) > 10) {
        ts_api <- ts(api_data$API, frequency = 365)
        forecast_model <- auto.arima(ts_api)
        forecast_api <- forecast(forecast_model, h = 90)
        
        forecast_dates <- seq(max(api_data$Date), by = "day", length.out = 90)
        forecast_df <- data.frame(Date = forecast_dates, Forecast = forecast_api$mean)
        
        plot_ly() %>%
          add_lines(data = api_data, x = ~Date, y = ~API, name = "Actual API", line = list(color = "blue")) %>%
          add_lines(data = forecast_df, x = ~Date, y = ~Forecast, name = "Forecast API", line = list(color = "red")) %>%
          layout(title = paste("3-Month API Forecast for", city_name),
                 xaxis = list(title = "Date"),
                 yaxis = list(title = "API Value"))
      } else {
        plot_ly() %>%
          layout(title = paste("Insufficient Data for API Forecast in", city_name))
      }
    })
  }
  
  # Generate plots for each city
  create_plots(data_graz, "graz")
  create_plots(data_innsbruck, "innsbruck")
  create_plots(data_linz, "linz")
  create_plots(data_salzburg, "salzburg")
  create_plots(data_vienna, "vienna")
}

shinyApp(ui, server)

rsconnect::setAccountInfo(name='q9zjb0-emilija-rudyte',token='DBEBC5DCAFE19BDD47ADDCBDE421EB88',secret='UbNRMhwzxOp8bpCpUEo/20a24Kui8tFU/wcOpztQ')
rsconnect::deployApp()
