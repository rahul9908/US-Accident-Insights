# Load necessary libraries
library(dplyr)
library(lubridate)

# Load the dataset
accidents <- read.csv("US_Accidents.csv", stringsAsFactors = FALSE)

# Selecting expanded relevant columns
accidents_filtered <- accidents %>%
  select(
    ID, Severity, Start_Time, End_Time, Start_Lat, Start_Lng, State, City,
    Weather_Condition, Distance.mi., Traffic_Signal, Temperature.F., Humidity...,
    Wind_Speed.mph., Visibility.mi.
  )

accidents_filtered <- accidents_filtered %>%
  filter(!is.na(Severity) & !is.na(Start_Time) & !is.na(End_Time) & !is.na(State))

# Load necessary libraries
library(shiny)
library(shinythemes)
library(leaflet)
library(ggplot2)
library(dplyr)
library(lubridate)
library(plotly)



# Ensure Year column exists
accidents <- accidents %>%
  mutate(
    Start_Time = as.POSIXct(Start_Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    Year = year(Start_Time)
  )

# Filter and process data
accidents_filtered <- accidents %>%
  filter(!is.na(Start_Time) & !is.na(End_Time) & !is.na(State)) %>%
  mutate(
    Weather_Condition = ifelse(is.na(Weather_Condition), "Unknown", Weather_Condition),
    Temperature.F. = ifelse(is.na(Temperature.F.), mean(Temperature.F., na.rm = TRUE), Temperature.F.),
    Humidity... = ifelse(is.na(Humidity...), mean(Humidity..., na.rm = TRUE), Humidity...),
    Wind_Speed.mph. = ifelse(is.na(Wind_Speed.mph.), mean(Wind_Speed.mph., na.rm = TRUE), Wind_Speed.mph.),
    Visibility.mi. = ifelse(is.na(Visibility.mi.), mean(Visibility.mi., na.rm = TRUE), Visibility.mi.)
  )

# UI definition
ui <- navbarPage(
  theme = shinytheme("flatly"),
  title = "US Accident Insights",
  
  # Home Page
  tabPanel("Home",
           fluidPage(
             tags$head(
               tags$style(HTML("
          body {
            background-color: #f4f8fb;
            font-family: Arial, sans-serif;
          }
          .well {
            background-color: #ffffff;
            box-shadow: 0px 2px 5px rgba(0,0,0,0.2);
            border-radius: 10px;
          }
          h1, h3 {
            color: #2c3e50;
          }
          .navbar {
            background-color: #2c3e50 !important;
            border-color: #2c3e50 !important;
          }
          .navbar-default .navbar-nav > li > a {
            color = #ffffff !important;
          }
          .navbar-default .navbar-brand {
            color = #ffffff !important;
          }
        "))
             ),
             titlePanel(h1("Welcome to US Accident Insights", align = "center")),
             br(),
             fluidRow(
               column(4, wellPanel(
                 h3("Total Accidents"),
                 textOutput("total_accidents"),
                 icon("car-crash", lib = "font-awesome")
               )),
               column(4, wellPanel(
                 h3("Average Severity"),
                 textOutput("avg_severity"),
                 icon("chart-line", lib = "font-awesome")
               )),
               column(4, wellPanel(
                 h3("Most Affected State"),
                 textOutput("most_affected_state"),
                 icon("map-marker-alt", lib = "font-awesome")
               ))
             ),
             br(),
             fluidRow(
               column(12,
                      plotlyOutput("home_bar_chart")
               )
             )
           )
  ),
  
  # Accident Trends Page
  tabPanel("Trends",
           sidebarLayout(
             sidebarPanel(
               sliderInput("date_slider", "Select Year Range:", 
                           min = min(accidents$Year, na.rm = TRUE), 
                           max = max(accidents$Year, na.rm = TRUE), 
                           value = c(min(accidents$Year, na.rm = TRUE), max(accidents$Year, na.rm = TRUE)), 
                           sep = ""),
               selectInput("state", "Select State:", 
                           choices = unique(accidents$State), 
                           selected = "CA"),
               checkboxGroupInput("severity", "Select Severity of Accident:", 
                                  choices = sort(unique(accidents$Severity)), 
                                  selected = sort(unique(accidents$Severity)))
             ),
             mainPanel(
               plotlyOutput("trend_plot"),
               tableOutput("summary_stats")
             )
           )
  ),
  
  # Geospatial Analysis Page
  tabPanel("Geospatial Analysis",
           sidebarLayout(
             sidebarPanel(
               dateRangeInput("geo_date_range", "Select Date Range:", 
                              start = min(accidents$Start_Time), 
                              end = max(accidents$Start_Time)),
               selectInput("weather", "Select Weather Condition:", 
                           choices = unique(accidents$Weather_Condition), 
                           selected = "Clear"),
               sliderInput("severity_slider", "Select Severity Range:", 
                           min = min(as.numeric(accidents$Severity), na.rm = TRUE), 
                           max = max(as.numeric(accidents$Severity), na.rm = TRUE), 
                           value = c(2, 4))
             ),
             mainPanel(
               leafletOutput("accident_map"),
               p("Hover over points to view accident details.")
             )
           )
  )
)

# Server definition
server <- function(input, output) {
  # Home Page Stats
  output$total_accidents <- renderText({
    nrow(accidents)
  })
  
  output$avg_severity <- renderText({
    round(mean(as.numeric(accidents$Severity), na.rm = TRUE), 2)
  })
  
  output$most_affected_state <- renderText({
    names(which.max(table(accidents$State)))
  })
  
  # Home Page Bar Chart
  output$home_bar_chart <- renderPlotly({
    state_counts <- accidents %>%
      count(State) %>%
      arrange(desc(n)) %>%
      top_n(10, n)
    
    plot_ly(state_counts, x = ~reorder(State, n), y = ~n, type = "bar", 
            marker = list(color = 'rgba(52, 152, 219, 0.8)')) %>%
      layout(
        title = "Top 10 States with Most Accidents",
        xaxis = list(title = "State"),
        yaxis = list(title = "Number of Accidents")
      )
  })
  
  # Trends Plot
  output$trend_plot <- renderPlotly({
    filtered_data <- accidents %>%
      filter(State == input$state,
             Severity %in% input$severity,
             Year >= input$date_slider[1],
             Year <= input$date_slider[2])
    
    if (nrow(filtered_data) == 0) {
      return(plot_ly() %>% 
               layout(
                 title = "No Data Available for Selected Filters",
                 xaxis = list(title = ""),
                 yaxis = list(title = "")
               ))
    }
    
    gg <- ggplot(filtered_data, aes(x = Start_Time, fill = Severity)) +
      geom_histogram(binwidth = 86400, color = "black", aes(y = ..count..)) +
      labs(
        title = "Accident Trends Over Time",
        x = "Date",
        y = "Number of Accidents",
        fill = "Severity"
      ) +
      theme_minimal()
    
    ggplotly(gg, tooltip = c("count")) %>% 
      layout(
        legend = list(orientation = "h", x = 0, y = -0.2)
      ) %>%
      style(hoverlabel = list(bgcolor = "white"))
  })
  
  
  
  # Summary Stats
  output$summary_stats <- renderTable({
    filtered_data <- accidents %>%
      filter(State == input$state,
             Severity %in% input$severity,
             Year >= input$date_slider[1],
             Year <= input$date_slider[2])
    
    if (nrow(filtered_data) == 0) {
      return(data.frame("Message" = "No data available for selected filters"))
    }
    
    filtered_data %>%
      summarize(
        Total_Accidents = n(),
        
      )
  })
  
  
  
  # Geospatial Map
  output$accident_map <- renderLeaflet({
    filtered_data <- accidents %>%
      filter(Weather_Condition == input$weather,
             as.numeric(Severity) >= input$severity_slider[1],
             as.numeric(Severity) <= input$severity_slider[2],
             Start_Time >= as.Date(input$geo_date_range[1]),
             Start_Time <= as.Date(input$geo_date_range[2]))
    
    if (nrow(filtered_data) == 0) {
      return(leaflet() %>% addTiles() %>% addControl("No data available for selected filters", position = "topright"))
    }
    
    leaflet(filtered_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Start_Lng, lat = ~Start_Lat,
        radius = ~as.numeric(Severity) * 2,
        popup = ~paste("<strong>Severity:</strong>", Severity, "<br>",
                       "<strong>Weather:</strong>", Weather_Condition, "<br>",
                       "<strong>Date:</strong>", format(Start_Time, "%Y-%m-%d %H:%M:%S"))
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)