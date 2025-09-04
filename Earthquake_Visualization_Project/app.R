# Load required libraries
library(shiny)       # For building interactive web applications
library(ggplot2)     # For plotting
library(plotly)      # For makinf the plots interactive
library(leaflet)     # For interactive map visualizations
library(dplyr)       # For data manipulation
library(lubridate)   # For handling and manipulating date and time
library(viridis)     # For color scales
library(countrycode) # For mapping countries to continents
library(stringr)     # For normalizing to lower case

# Load and preprocess earthquake data
earthquakes <- read.csv("earthquakes.csv") %>%
  mutate(
    time = ymd_hms(time),                                  # Convert time to datetime format
    type = str_to_lower(type),                             # Convert to lowercase
    depth = as.numeric(depth),                             # Ensure depth is numeric
    radius = (((mag - 2.6) / (7.8 - 2.6))^3 * 15) + 2      # Scale magnitude to marker radius
  ) %>%
  filter(!is.na(longitude), !is.na(latitude),              # Remove rows with missing values
         !is.na(mag), !is.na(depth), !is.na(time))

#################
# PREPROCESSING #
# 1. Create year-month column
# 2. Get continent based on latitude and longitude
#################

## 1. Create a year-month column
#earthquakes$time <- ymd_hms(earthquakes$time)
earthquakes$date <- as.Date(earthquakes$time)
# Extract year, month, and day
earthquakes$year <- year(earthquakes$time)
earthquakes$month <- month(earthquakes$time)
earthquakes$day <- day(earthquakes$time)
earthquakes$year_month <- format(earthquakes$time, "%Y-%m")

## 2. Get continent based on latitude and longitude
get_continent <- function(lat, lon) {
  if (is.na(lat) || is.na(lon)) {return(NA)}
  # North America
  else if (lat > 15 & lat <= 90 & lon >= -180 & lon <= -30) {return("North America")}
  # South America
  else if (lat >= -55 & lat <= 15 & lon >= -160 & lon <= -20) {return("South America")}
  # Europe
  else if (lat > 35 & lat <= 90 & lon > -30 & lon <= 55) {return("Europe")}
  # Africa
  else if (lat >= 0 & lat <= 35 & lon > -30 & lon <= 55.      # Box for the top part of Africa
           | lat >= -55 & lat <= 0 & lon > -20 & lon <= 70) { # Box for the bottom part of Africa
    return("Africa")}
  # Asia
  else if (lat > -5 & lat <= 90 & lon > 55 & lon <= 180) {return("Asia")}
  # Oceania
  else if (lat >= -55& lat <= -5 & 
           (lon > 70 & lon <= 180 # Most East side of the Map
            | lon >= -180 & lon < -160)) { # Most West side of the Map
    return("Oceania")}
  # Antarctica
  else if (lat < -55 & lon >= -180 & lon <= 180) {return("Antarctica")}
  else {return('Other')}
}

substitute_continent <- function(continent, continent2) {
  result <- ifelse(is.na(continent2), continent, ifelse(continent2 == 'Americas', continent, continent2))
  return(result)
}

# Add continent
earthquakes <- earthquakes %>%
  mutate(continent = mapply(get_continent, latitude, longitude))

earthquakes <- earthquakes %>% 
  mutate(country = sub(".*, ", "", place))
earthquakes <- earthquakes %>% 
  mutate(continent2 = countrycode(sourcevar = earthquakes$country, origin = "country.name", destination = "continent")) %>%
  mutate(continent2 = substitute_continent(continent, continent2))

earthquakes <- earthquakes %>%
  filter(!is.na(mag) & !is.na(depth) & !is.na(continent) 
         & !is.na(gap) & !is.na(dmin) & !is.na(nst) 
         & !is.na(horizontalError)& !is.na(depthError)& !is.na(magError) 
         & !is.null(place) & !is.na(place))

# earthquakes <- earthquakes %>%
#   filter(!is.na(mag) & !is.na(depth) & !is.na(continent))

##################
# USER INTERFACE #
##################
ui <- navbarPage(
  "Data Visualization Group 11",
  tabPanel("Introduction",
           sidebarLayout(
             sidebarPanel(
               h2("Data Visualization Group 11"),
               p("UPM - Masters in Digital Innovation"),
               p("EIT Digital - Data Science"),
               p("As data analysts, we assist seismologists and geophysicists by uncovering patterns in earthquake data through geographic clustering 
                 and temporal analysis. Our interactive Shiny app enables them to explore earthquake frequency, magnitude, and regional trends, 
                 helping to identify high-risk zones and improve seismic risk assessment."),
               br(),
               br("This app was developed by:"),
               p("02585548A", a("Álvaro Honrubia Genilloud", href = "https://github.com/Drainkhan"),"150138"),
               p("24408593G", a("Jose Antonio Ruiz Heredia", href="https://github.com/JoseRuiz01"), "240532"),
               p("X4817538G", a("Xiya Sun", href = "https://github.com/xiyaa"), "c20c030"),
               p("ZRK6T66H7", a("Joseph Tartivel", href="https://github.com/Bilk11"), "245582"),
               br("Powered by"),
               "Shiny, a product of ", 
               span("RStudio", style = "color:blue")
             ),
             mainPanel(
               h1("Global Earthquake Trends and Interactive Analysis"),
               p("This project aims to explore global earthquake patterns through interactive data visualizations and geographic clustering techniques. 
               By analyzing earthquake frequency, magnitude, depth, and type across different regions, we seek to uncover hidden patterns and regional differences in seismic activity. 
               These insights can help improve seismic risk assessment and early warning systems, aiding seismologists and researchers in understanding the behavior of earthquakes worldwide."),
               p('The interface leverages clustering algorithms to identify natural groupings of earthquakes based on their geographic proximity, providing valuable insights into fault lines and high-risk zones. 
                 Additionally, the tool allows users to dynamically filter and explore data to address key research questions.'),
               
               br(),
               p('The analysis is based on the Global Earthquakes 2023 dataset obtained from Kaggle. The dataset contains detailed information about earthquakes recorded globally in 2023, 
               including variables such as time, latitude, longitude, depth, magnitude, type, and seismic network coverage.'),

               p('The dataset provides a comprehensive view of seismic activity across the world and serves as the foundation for the visualizations and analyses in this project. The dataset can be accessed via the following link:', 
                 a("Dataset in Kaggle", 
                   href = "https://www.kaggle.com/datasets/mustafakeser4/earthquakes-2023-global")),
               br(),
               p('This project addresses the following four core questions through a series of interactive visualizations:'),
               p(strong('1.- How do the key earthquake characteristics change across different regions?')),
               p(strong('2.- Are there temporal patterns in the types of earthquakes in terms of frequency?' )),
                        # Specifically, can we observe any temporal trends or patterns in the occurrence and intensity of each earthquake type?')),
               p(strong('3.- What is the distribution of key earthquake characteristics (such as magnitude and depth) across different regions, and what geographic patterns can be observed?')),
               p(strong('4.- How does the coverage of seismic stations (Gap) of each network vary across different regions, and what impact does this have on earthquake detection and reporting errors?')),
               p(),
               br(),
             )
           )
  ),
  tabPanel(
    "Characteristics",
    
    h2("How do key earthquake characteristics (such as magnitude and depth) change over region and time?"),
    p("This visualization focuses on studying changes in magnitude and depth grouped by continent or year-month."),
    br(),
    
    # Main panel with sidebarLayout
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "measurement",
          h4("Select Measurement"),
          choices = c("Magnitude", "Depth")
        ),
        
        # Conditional panel to show region or date input based on tab selection
        conditionalPanel(
          condition = "input.tabs == 'By Continent'",
          h4("Filter by Region"),
          checkboxGroupInput(
            "regions",
            "Select Continents:",
            choices = sort(unique(earthquakes$continent)),
            selected = sort(unique(earthquakes$continent))
          ),
          actionButton("select_all", "Select All"),
          actionButton("deselect_all", "Deselect All")
        ),
        
        conditionalPanel(
          condition = "input.tabs == 'By Year-Month'",
          h4("Filter by Time"),
          dateRangeInput(
            inputId = "date_range",
            label = "Select Date Range:",
            start = min(earthquakes$date),
            end = max(earthquakes$date)
          )
        ),
        
        # Output earthquake count
        textOutput("earthquake_count")
      ),
      
      mainPanel(
        tabsetPanel(
          id = "tabs",  # Give the tabset an id to reference for conditionalPanel
          
          # Tab 1: By Continent
          tabPanel(
            "By Continent",
            h3("Average analysis per continent"),
            p("In this bar chart we aim to highlight relations in the evolution of earthquakes characteristics based on their spatial location."),
            plotOutput("barChart", width = "500px", height = "300px"),
            h3("Map of earthquakes by continent"),
            p("This map focuses on filtering the earthquakes based on the continent location."),
            leafletOutput("mapContinent", width = "700px", height = "300px")
          ),
          
          # Tab 2: By Year-Month (with date range input)
          tabPanel(
            "By Year-Month",
            h3("Average analysis per year-month"),
            p("In this line chart we focus to obtain insights from the evolution of earthquakes characteristics in temporal intervals."),
            plotOutput("lineChartYear", width = "500px", height = "300px"),
            h3("Map of earthquakes by year-month" ),
            p("This map focuses on filtering the earthquakes in terms on a selected time range."),
            leafletOutput("mapYear", width = "700px", height = "300px")
          )
        )
      )
    )),
  tabPanel("Frequency",
           h2("Are there temporal patterns in the types of earthquakes in terms of frequency?"),
           p('This visualization focuses on identifying temporal trends in the occurrence and intensity of different types of earthquakes over time.'),
           br(),
           sidebarLayout(
             sidebarPanel(
               dateRangeInput(
                 inputId = "date_range", 
                 label = "Select Date Range:",
                 start = min(earthquakes$date),
                 end = max(earthquakes$date)),
               sliderInput(
                 inputId = "magnitude_filter",
                 label = "Select Magnitude Range:",
                 min = min(earthquakes$mag, na.rm = TRUE),
                 max = max(earthquakes$mag, na.rm = TRUE),
                 value = c(3 ,7)
               ),
               
               checkboxGroupInput(
                 inputId = "type_filter",
                 label = "Select Earthquake Types:",
                 choices = sort(unique(earthquakes$type)),
                 selected = sort(unique(earthquakes$type))),
               
               selectizeInput(
                 "regionContinent",
                 "Select the continents you want to compare:",
                 choices = unique(earthquakes$continent),
                 selected = unique(earthquakes$continent),
                 multiple = TRUE,)
             ),
             
             mainPanel(
               h3("Earthquakes types frequency over time"),
               p("We observe the frequency trends over time of different types of earthquakes and 
                  we can compare different continents."),
               br(),
               
               tabsetPanel(
                 tabPanel("Earthquake Types Frequency Over Time", 
                          p('This line chart displays the frequency of earthquakes over time based on the selected date range, magnitude range, and earthquake types. 
                            The chart allows users to observe temporal trends in seismic activity and identify periods of higher or lower earthquake occurrences. 
                            The interactive feature enables users to dynamically adjust the filters and explore specific time periods or earthquake types.'),
                          plotlyOutput("line_chart"), 
                          br(),
                          p("This bar chart provides a summary of the total number of earthquakes for each selected type within the chosen time period."),
                          plotlyOutput("bar_chart")),
                 tabPanel("Continent Comparison", 
                          p('This interactive chart allows users to compare earthquake frequency across different continents over a selected time range. 
                            By filtering the date range, magnitude, and earthquake type, users can observe regional differences in seismic activity and identify which continents experience more frequent earthquakes.'),
                          plotlyOutput("region_chart")),
               )
             )
           
  )),
  # TabPanel for Geographic Distribution
  tabPanel("Geographic Distribution",
           h2("What is the distribution of key earthquake characteristics across different regions, and what geographic patterns can be observed?"),
           p('This question investigates the geographic distribution of magnitude and depth, highlighting patterns that can reveal active fault lines and areas with frequent seismic events.'),
           br(),
           sidebarLayout(
             sidebarPanel(
               # Date range input for filtering earthquakes by time
               dateRangeInput("timeRange", "Select Date Range:",
                              start = "2023-01-01",
                              end = "2023-12-31",
                              min = "2023-01-01",
                              max = "2023-12-31",
                              format = "dd/mm/yyyy"),
               
               # Slider input for filtering earthquakes by magnitude
               sliderInput("magnitude", "Select Magnitude Range:",
                           min = 2, 
                           max = 9, 
                           value = c(2, 9),
                           step = 0.1),
               
               # Slider input for filtering earthquakes by depth
               sliderInput("depth", "Select Depth Range (km):",
                           min = 0, 
                           max = 700, 
                           value = c(0, 700),
                           step = 10),
               
               # Add clustering options
               checkboxInput("enableClustering", "Enable Clustering", value = FALSE)
             ),
             
             mainPanel(
               h3("2023 Earthquake: Magnitude & Depth Distribution"),
               leafletOutput("map2", height = "600px")
             )
           )
  ), #tabpanel closure
  tabPanel("Coverage & Error",
           h2("How does the coverage of seismic stations (Gap) of each network vary across different regions, and what impact does this have on earthquake detection and reporting errors?"),
           p('This analysis examines the network coverage of seismic stations and its effect on detection accuracy, exploring how gaps in coverage can lead to reporting errors in earthquake monitoring.'),
           br(),
           sidebarLayout(
             # SELECTOR
             sidebarPanel(
               checkboxGroupInput(
                 "regionsCov", 
                 "Select Continents:", 
                 choices = sort(unique(earthquakes$continent)),
                 selected = unique(earthquakes$continent)
               ),
               actionButton("select_all_continentCov", "Select All"),
               actionButton("deselect_all_continentCov", "Deselect All"),
               checkboxGroupInput(
                 "netsCov", 
                 "Select Nets:", 
                 choiceNames =list( 
                   "United States Geological Survey (USGS) - us", 
                   "Puerto Rico Seismic Network - pr", 
                   "Northern California Seismic System - nc", 
                   "University of Texas at Austin - tx", 
                   "Caltech/USGS Southern California Seismic Network - ci", 
                   "University of Utah Seismograph Stations - uu", 
                   "European-Mediterranean Seismological Centre (EMSC) - se", 
                   "New Madrid Seismic Zone - nm", 
                   "University of Washington, Pacific Northwest Seismic Network - uw"
                 ),
                 choiceValues =list( "us","pr","nc","tx","ci","uu","se","nm","uw"),
                 selected = unique(earthquakes$net)
               ),
               actionButton("select_all_netCov", "Select All"),
               actionButton("deselect_all_netCov", "Deselect All"),
               
               # Slider input for filtering earthquakes by GAP
               sliderInput("gap", "Select GAP Range:",
                           min = min(earthquakes$gap), 
                           max = max(earthquakes$gap), 
                           value = c(min(earthquakes$gap), max(earthquakes$gap)),
                           step = 0.1),
               # Slider input for filtering earthquakes by DMin
               sliderInput("dmin", "Select DMin Range:",
                           min = min(earthquakes$dmin), 
                           max = max(earthquakes$dmin), 
                           value = c(min(earthquakes$dmin), max(earthquakes$dmin)),
                           step = 0.1),
               selectInput("errorType", "Select Error:",
                           choices = c("Horizontal", "Depth", "Magnitude")),
               width = 3
             ),
             # BAR CHART
             mainPanel(
               h3("Coverage of seismic stations networks across regions of 2023"),
               plotlyOutput("bubbleChart"), 
               
               h3("Errors of each network across regions of 2023"),
               plotlyOutput("heatmap")
             )
           )
  ) #tabpanel closure
  ) #final closure

##########
# SERVER #
##########
server <- function(input, output, session) {
  # Filter the data based on user inputs
  filtered_data <- reactive({
    earthquakes %>%
      filter(
        date >= input$date_range[1],
        date <= input$date_range[2],
        type %in% input$type_filter,
        mag >= input$magnitude_filter[1],
        mag <= input$magnitude_filter[2]
      ) %>%
      group_by(type, date) %>%
      summarise(freq = n(),avg_mag = mean(mag, na.rm = TRUE), .groups="drop")
  })
  

  type_counts <- reactive({
    filtered_data() %>%
      group_by(type) %>%
      summarise(count = sum(freq))
  })
  
  
  ## FILTER WITH CONTINENTS
  regionByContinents <- reactive({
    
    req(input$regionContinent)
    
    earthquakes %>%
      filter(
        date >= input$date_range[1],
        date <= input$date_range[2],
        type %in% input$type_filter,
        continent %in% input$regionContinent,
        mag >= input$magnitude_filter[1],
        mag <= input$magnitude_filter[2]
      ) %>%
      group_by(continent, type, date) %>%
      summarise(
        freq = n(),
        avg_mag = mean(mag, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  output$line_chart <- renderPlotly({
    data <- filtered_data()
    req(nrow(data)>0)  # ensure data is not empty
    
    p <- ggplot(data, aes(x = date)) +
      geom_line(aes(y = freq, color = type), size = 0.5) +  # Frequency Line
      scale_y_continuous(name = "Frequency") +
      labs(
        title = "Earthquake Frequency Over Time",
        x = "Date",
        color = "Earthquake Type"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12)
      )

    # Convert to Plotly for interactivity
    ggplotly(p, tooltip = c("x", "y", "color")) %>%
      layout(
        hovermode = "x unified",
        legend = list(title = list(text = "Earthquake Types"))
      )
  })
  
  output$bar_chart <- renderPlotly({
    data <- type_counts()
    
    p <- ggplot(type_counts(), aes(x = count, y = type, fill = type)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = count), vjust = -0.5, size = 5) +
      labs(
        title = "Total Earthquake Counts by Type",
        x = "Count",
        y = "Earthquake Type"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    ggplotly(p)
  })
  
  
  
  
  output$region_chart <- renderPlotly({
    data <- regionByContinents()
    # if (is.null(data)) return()
    
    p <- ggplot(data, aes(x = date, y = freq)) +
      geom_line(size = 1, color = "blue") +
      facet_wrap(~ continent, ncol = 2) +
      labs(
        title = "Earthquake Frequency by Continent Over Time",
        x = "Date",
        y = "Frequency"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)
      )
    ggplotly(p)
  })
  
  #######################
  #### EXERCISE 1    ####
  #######################
  
  # Observe events for region selection
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session, "regions", selected = unique(earthquakes$continent))
  })
  
  observeEvent(input$deselect_all, {
    updateCheckboxGroupInput(session, "regions", selected = character(0))
  })
  
  # Reactive data for By Continent
  dataByContinents <- reactive({
    earthquakes %>%
      filter(continent %in% input$regions)
  })
  
  # Reactive data for By Year-Month (using date range input)
  dataByYearMonth <- reactive({
    earthquakes %>%
      filter(
        date >= input$date_range[1], # Filter by the selected date range
        date <= input$date_range[2]  # Filter by the selected date range
      )
  })
  
  # Reactive for earthquake count
  output$earthquake_count <- renderText({
    # Check which tab is selected to determine which data to use
    count <- if (input$tabs == "By Continent") {
      nrow(dataByContinents())
    } else {
      nrow(dataByYearMonth())
    }
    paste("N. of Earthquakes: ", count)
  })
  
  ##############################
  # BAR CHART BY CONTINENT     #
  ##############################
  
  output$barChart <- renderPlot({
    avg_data <- dataByContinents() %>%
      group_by(continent) %>%
      summarise(
        avg_value = mean(ifelse(input$measurement == "Magnitude", mag, depth), na.rm = TRUE)
      )
    
    y_label <- ifelse(input$measurement == "Magnitude",
                      paste("Average", input$measurement),
                      paste("Average", input$measurement, "(km)"))
    fill_color <- ifelse(input$measurement == "Magnitude", "red", "blue")
    
    ggplot(avg_data, aes(x = reorder(continent, -avg_value), y = avg_value)) +
      geom_bar(stat = "identity", fill = fill_color, alpha = 0.5) +
      coord_flip() +
      labs(
        title = paste("Average Earthquake", input$measurement, "by Continent"),
        x = "Continent",
        y = y_label
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(size = 12),
        plot.title = element_text(face = "bold", size = 16)
      ) +
      geom_text(
        aes(label = round(avg_value, 2)),
        hjust = 0.5,
        vjust = -0.5,
        color = "black",
        size = 4,
        fontface = "bold"
      )
  })
  
  ##############################
  # HEAT MAP BY CONTINENT      #
  ##############################
  
  output$mapContinent <- renderLeaflet({
    selected_variable <- if (input$measurement == "Magnitude") {
      earthquakes$mag
    } else {
      earthquakes$depth
    }
    
    filtered_data <- dataByContinents()
    
    color_palette <- colorNumeric(
      palette = c("lightyellow", "yellow", "orange", "red", "darkred", "purple", "black"),
      domain = selected_variable
    )
    
    leaflet(data = filtered_data) %>%
      addTiles() %>%
      addCircleMarkers(
        ~longitude, ~latitude,
        radius = 6,
        color = ~color_palette(selected_variable),
        stroke = FALSE,
        fillOpacity = 0.6
      ) %>%
      addLegend(
        position = "bottomright",
        pal = color_palette,
        values = selected_variable,
        title = ifelse(input$measurement == "Magnitude", "Earthquake Magnitude", "Earthquake Depth (km)"),
        opacity = 0.7,
        labFormat = labelFormat(suffix = "")
      )
  })
  
  ##############################
  # LINE CHART BY YEAR-MONTH   #
  ##############################
  
  output$lineChartYear <- renderPlot({
    avg_data <- dataByYearMonth() %>%
      group_by(year_month) %>%
      summarise(
        avg_value = mean(ifelse(input$measurement == "Magnitude", mag, depth), na.rm = TRUE)
      )
    
    avg_data$year_month <- ym(avg_data$year_month) # Convert to Date (YYYY-MM)
    avg_data$formatted_year_month <- format(avg_data$year_month, "%Y-%b") # YYYY-Month (Jan, Feb, Mar)
    avg_data$formatted_year_month <- factor(avg_data$formatted_year_month, levels = avg_data$formatted_year_month[order(avg_data$year_month)])
    
    y_label <- ifelse(input$measurement == "Magnitude",
                      paste("Average", input$measurement),
                      paste("Average", input$measurement, "(km)"))
    line_color <- ifelse(input$measurement == "Magnitude", "red", "blue")
    
    ggplot(avg_data, aes(x = formatted_year_month, y = avg_value, group = 1)) +
      geom_line(color = line_color, size = 1) + # Line chart
      geom_point(color = line_color, size = 3) + # Add points for emphasis
      labs(
        title = paste("Average Earthquake", input$measurement, "by Year-Month"),
        x = "Year-Month",
        y = y_label
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(size = 12, angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", size = 16),
        plot.margin = margin(10, 10, 10, 30) # Increase plot margin for long labels
      ) +
      geom_text(
        aes(label = round(avg_value, 2)),
        vjust = -0.5,
        color = "black",
        size = 4,
        fontface = "bold"
      )
  })
  
  ##############################
  # HEAT MAP BY YEAR-MONTH     #
  ##############################
  
  output$mapYear <- renderLeaflet({
    selected_variable <- if (input$measurement == "Magnitude") {
      earthquakes$mag
    } else {
      earthquakes$depth
    }
    
    filtered_data <- dataByYearMonth()
    
    color_palette <- colorNumeric(
      palette = c("lightyellow", "yellow", "orange", "red", "darkred", "purple", "black"),
      domain = selected_variable
    )
    
    leaflet(data = filtered_data) %>%
      addTiles() %>%
      addCircleMarkers(
        ~longitude, ~latitude,
        radius = 6,
        color = ~color_palette(selected_variable),
        stroke = FALSE,
        fillOpacity = 0.6
      ) %>%
      addLegend(
        position = "bottomright",
        pal = color_palette,
        values = selected_variable,
        title = ifelse(input$measurement == "Magnitude", "Earthquake Magnitude", "Earthquake Depth (km)"),
        opacity = 0.7,
        labFormat = labelFormat(suffix = "")
      )
  })
  
  
  
  
  #####EXERCISE 3
  # Color palette for depth
  depthPalette <- colorNumeric(
    palette = colorRampPalette(c("#edbb99", "#873600"))(10),
    domain = c(0, 700)  # Depth range (0-700 km)
  )
  
  # Reactive data filtering based on user input
  filteredData <- reactive({
    req(input$magnitude, input$depth, input$timeRange)  # Ensure inputs are available
    
    data <- earthquakes %>%
      filter(
        mag >= input$magnitude[1], 
        mag <= input$magnitude[2],
        depth >= input$depth[1], 
        depth <= input$depth[2],
        date(time) >= input$timeRange[1],
        date(time) <= input$timeRange[2]
      ) %>%
      mutate(
        color = depthPalette(depth),
        popup = paste(
          "Time:", format(time, "%d/%m/%Y %H:%M"), "<br>",
          "Place:", place, "<br>",
          "Magnitude:", mag, "<br>",
          "Depth:", depth, "km<br>"
        )
      )
    return(data)
  })
  
  # Initial rendering of the map with default data
  output$map2 <- renderLeaflet({
    initialData <- earthquakes %>%
      filter(
        mag >= 2, 
        mag <= 9,
        depth >= 0, 
        depth <= 700,
        date(time) >= as.Date("2023-01-01"),
        date(time) <= as.Date("2023-12-31")
      ) %>%
      mutate(
        color = depthPalette(depth),
        popup = paste(
          "Time:", format(time, "%d/%m/%Y %H:%M"), "<br>",
          "Place:", place, "<br>",
          "Magnitude:", mag, "<br>",
          "Depth:", depth, "km<br>"
        )
      )
    
    leaflet(options = leafletOptions(
      worldCopyJump = FALSE,
      minZoom = 2,
      maxZoom = 5
    )) %>%
      addTiles() %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      setMaxBounds(lng1 = -170, lat1 = -60, lng2 = 170, lat2 = 75) %>%
      addLegend(
        position = "bottomright",
        pal = depthPalette,
        values = c(0, 700),
        title = "Depth (km)",
        opacity = 1
      ) %>%
      addCircleMarkers(
        data = initialData,
        lng = ~longitude, 
        lat = ~latitude,
        radius = 5,
        color = ~color,
        stroke = TRUE,
        weight = 1,
        fillOpacity = 0.6,
        popup = ~popup
      )
  })
  
  # Update the map when filters change
  observe({
    req(input$magnitude, input$depth, input$timeRange)
    
    data <- filteredData()
    proxy <- leafletProxy("map2")
    
    proxy %>%
      clearMarkers() %>%
      clearMarkerClusters()
    
    if (input$enableClustering) {
      proxy %>%
        addMarkers(
          data = data,
          lng = ~longitude, 
          lat = ~latitude,
          popup = ~popup,
          clusterOptions = markerClusterOptions()
        )
    } else {
      proxy %>%
        addCircleMarkers(
          data = data,
          lng = ~longitude, 
          lat = ~latitude,
          radius = 5,
          color = ~color,
          stroke = TRUE,
          weight = 1,
          fillOpacity = 0.6,
          popup = ~popup
        )
    }
  })
  #####EXERCISE 4
  
  # Select All 
  observeEvent(input$select_all_continentCov, {
    updateCheckboxGroupInput(session, "regionsCov", selected = unique(earthquakes$continent))
  })
  
  observeEvent(input$select_all_netCov, {
    updateCheckboxGroupInput(session, "netsCov", selected = unique(earthquakes$net))
  })
  
  # Deselect All
  observeEvent(input$deselect_all_continentCov, {
    updateCheckboxGroupInput(session, "regionsCov", selected = character(0))
  })
  
  observeEvent(input$deselect_all_netCov, {
    updateCheckboxGroupInput(session, "netsCov", selected = character(0))
  })
  
  # Reactive data by continent & net
  avg_data <- reactive({
    earthquakes %>%
      filter(continent2 %in% input$regionsCov) %>%
      filter(net %in% input$netsCov) %>%
      filter(gap >= input$gap[1],                         # Filter by gap range
             gap <= input$gap[2],
             dmin >= input$dmin[1],                           # Filter by dmin range
             dmin <= input$dmin[2]) %>%
      group_by(continent2, net) %>%
      summarise(
        mean_gap = mean(gap, na.rm = TRUE), 
        mean_dmin = mean(dmin, na.rm = TRUE),
        mean_nst = mean(nst, na.rm = TRUE),
        Horizontal= mean(horizontalError, na.rm = TRUE),
        Depth= mean(depthError, na.rm = TRUE),
        Magnitude= mean(magError, na.rm = TRUE),
        numberStations = n()
      )%>% 
      mutate(text = paste(
        "&nbsp;Net:", net, "<br>",
        "Continent:", continent2, "<br>",
        "Mean Gap:", round(mean_gap, 2), "<br>",
        "Mean Dmin:", round(mean_dmin, 2), "<br>",
        "Mean Nst:", round(mean_nst, 2), "<br>",
        "Number of stations:",numberStations
      )
      )
  })
  
  output$bubbleChart <- renderPlotly({
    ggplotly(ggplot(avg_data(), aes(x = mean_dmin , y = mean_gap , size = mean_nst ,color = continent2, label = net, text = text)) + 
               geom_point(alpha = 0.5)+
               labs(x = "Mean Minimum Distance - Dmin (km)",
                    y = "Mean Gap (km)",
                    size = "Mean Number of Stations - Nst ",
                    color= ""
               )+
               scale_size_continuous(range = c(1, 10), trans = "log10") + # Adjust bubble size progression
               #scale_x_continuous(limits = c(min_value_x, NA)) + # Set min value for x-axis 
               scale_y_continuous(limits = c(0, NA)) + # Set min value for y-axis
               theme_minimal(), tooltip = "text"
    )
    
    
  })
  
  output$heatmap <- renderPlotly({ 
    
    
    ggplotly(ggplot(avg_data(), 
           aes(x = continent2, 
               y = net, 
               fill = get(input$errorType),
               text = paste("Value:", get(input$errorType), "<br>Net:", net, "<br>Continent:", continent2, "<br>Number of stations:",numberStations),
               hoverinfo = "text"
           ))  +
      geom_tile() + 
      scale_fill_viridis(option = "D")+
      labs(title = paste("Heatmap of", input$errorType), 
           x = "Continents", 
           y = "Nets", 
           fill = input$errorType) +
      theme_minimal(), tooltip = "text" )
  })
}  #END SERVER




# Run the Shiny app
shinyApp(ui = ui, server = server)

