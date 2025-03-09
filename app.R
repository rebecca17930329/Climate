library(shiny)
library(httr) 
library(leaflet) 
library(jsonlite) 
library(rsconnect)
library(lubridate)
library(osrm)
library(sf)

locations <- data.frame(
  name = c("Dallas", "San Diego", "San Francisco", "Denver", "Los Angeles", 
           "Atlanta", "Chicago", "Boston", "Phoenix", "New York", 
           "Houston", "Miami", "Seattle", "Las Vegas", "Philadelphia", "Austin",
           "Washington D.C.", "Detroit"),
  lat = c(32.7767, 32.7157, 37.7749, 39.7392, 34.0522, 33.7490, 41.8781, 
          42.3601, 33.4484, 40.7128, 29.7604, 25.7617, 47.6062, 36.1699, 
          39.9526, 30.2672, 38.9072, 42.3314),
  lon = c(-96.7970, -117.1611, -122.4194, -104.9903, -118.2437, -84.3880, 
          -87.6298, -71.0589, -112.0740, -74.0060, -95.3698, -80.1918,
          -122.3321, -115.1398, -75.1652, -97.7431, -77.0369, -83.0458)
)

get_weather_forecast <- function(city, cnt, selected_day) {
  api_key <- "05652b4942294328de9622b2ba85c51a"
  url <- paste0("https://api.openweathermap.org/data/2.5/forecast/daily?q=", 
                URLencode(city), 
                "&cnt=", cnt, 
                "&appid=", api_key)
  
  response <- GET(url) 
  
  if (status_code(response) == 200) {
    data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
    city_name <- data$city$name
    country_code <- data$city$country
    
    if (!is.null(selected_day) && (selected_day < 1 || selected_day > cnt)) {
      stop(":x: Selected day is out of range. Please choose a value between 1 and ",
           cnt, "!")
    }
    
    weather_array <- list()
    
    get_weather_data <- function(i) {
      timestamp <- data$list$dt[i]
      date <- as_datetime(timestamp, tz = "America/Los_Angeles")
      formatted_date <- format(date, "%Y-%m-%d")
      
      temp_min <- (data$list$temp$min[i] - 273.15) * 9/5 + 32  
      temp_max <- (data$list$temp$max[i] - 273.15) * 9/5 + 32  
      
      weather_main <- data$list$weather[[i]]$main
      weather_desc <- data$list$weather[[i]]$description
      
      wind_speed <- data$list$speed[i] * 2.237  
      humidity <- data$list$humidity[i]
      
      weather_data <- list(
        date = formatted_date,
        min_temp = round(temp_min, 1),
        max_temp = round(temp_max, 1),
        weather_main = weather_main,
        weather_desc = weather_desc,
        wind_speed = round(wind_speed, 1),
        humidity = humidity
      )
      
      return(weather_data)
    }
    
    if (is.null(selected_day)) {
      for (i in 1:cnt) {
        weather_array[[i]] <- get_weather_data(i)
      }
    } else {
      weather_array <- get_weather_data(selected_day)
    }
    
    return(weather_array)
    
  } else {
    stop(":x: Request failed, status code:", status_code(response))
  }
}

plot_weather_layers <- function(lat, lon, zoom, date = Sys.Date()) {
  
  api_key <- "05652b4942294328de9622b2ba85c51a"
  
  unix_timestamp <- as.numeric(as.POSIXct(date, origin = "1970-01-01", 
                                          tz = "UTC"))
  
  layers <- list(
    "WND" = "Wind Speed & Direction",
    "TA2" = "Temperature (2m)",
    "PA0" = "Precipitation",
    "APM" = "Atmospheric Pressure",
    "CL"  = "Cloud Cover"
  )
  
  map <- leaflet() %>%
    setView(lng = lon, lat = lat, zoom = zoom) %>%
    addTiles() 
  
  for (layer in names(layers)) {
    map <- map %>%
      addTiles(
        urlTemplate = paste0("http://maps.openweathermap.org/maps/2.0/weather/",
                             layer, "/{z}/{x}/{y}?date=", unix_timestamp,
                             "&appid=", api_key),
        options = tileOptions(noWrap = TRUE),
        group = layers[[layer]] 
      )
  }
  
  map <- map %>%
    addLayersControl(
      overlayGroups = unname(layers),
      options = layersControlOptions(collapsed = FALSE)
    )
  return(map)
}

get_city_name <- function(lat, lon) {
  url <- paste0("https://nominatim.openstreetmap.org/reverse?lat=", lat, 
                "&lon=", lon, "&format=json")
  res <- GET(url)
  if (status_code(res) == 200) {
    data <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
    return(data$address$city)  # Extract city name
  }
  return("Unknown City")
}


ui <- navbarPage("Climate Assistant",
                 
                 tabPanel("Current Location Climate",
                          sidebarLayout(
                            sidebarPanel(
                              h3("Get Current Location and Choose the Date"),
                              tags$img(src = "https://i.postimg.cc/15RKfLc1/qrcode-climateassistant-shinyapps-io.png", class = "sidebar-photo"),
                              
                              selectInput("day", "Select Day", 
                                          choices = setNames(c(1,2,3,4,5,6,7), 
                                                             format(Sys.Date() + 0:6, 
                                                                    "%Y-%m-%d")),
                                          selected = 1),
                              actionButton("get_location", "Get My Location"),
                            ),
                            mainPanel(
                              h3("Climate Assistant for Current Location"),
                              p("(Response time around 12 seconds and Please use in web browser)"),
                              leafletOutput("plot_map"),
                              tags$br(),
                              verbatimTextOutput("loc_climate_data"),
                              h3("Mistral AI: Tips on Dressing"),
                              verbatimTextOutput("ai_response")
                              
                            )
                          ),
                          tags$head(
                            tags$style(HTML("
      .sidebar-photo {
        position: fixed;
        bottom: 10px;
        left: 10px;
        width: 150px; 
        z-index: 1000;
      }
    "))
                          ),
                          tags$script(HTML("
        document.getElementById('get_location').addEventListener('click', function() {
            if (navigator.geolocation) {
                navigator.geolocation.getCurrentPosition(
                    function(position) {
                        Shiny.setInputValue('user_location', {
                            lat: position.coords.latitude,
                            lon: position.coords.longitude
                        }, { priority: 'event' });
                    },
                    function(error) {
                        console.error('Error obtaining location:', error);
                        Shiny.setInputValue('user_location', { error: 'Location access denied!!!' });
                    }
                );
            } else {
                Shiny.setInputValue('user_location', { error: 'Geolocation X' });
            }
        });
    "))
                 ),
                 tabPanel("Road Trip Climate Assistant",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("loc1", "Select Origin:", choices = locations$name,
                                          selected = "Los Angeles"),
                              selectInput("day_loc1", "Select Day", choices = c(1,2,3,4,5,6,7),
                                          selected = 1),
                              selectInput("loc2", "Select Rest Stop 1:", 
                                          choices = locations$name, selected = "Denver"),
                              selectInput("day_loc2", "Select Day", choices = c(1,2,3,4,5,6,7),
                                          selected = 2),
                              selectInput("loc3", "Select Rest Stop 2:", 
                                          choices = locations$name, selected = "Atlanta"),
                              selectInput("day_loc3", "Select Day", choices = c(1,2,3,4,5,6,7),
                                          selected = 3),
                              selectInput("loc4", "Select Destination:",
                                          choices = locations$name, selected = "Miami"),
                              selectInput("day_loc4", "Select Day", choices = c(1,2,3,4,5,6,7), 
                                          selected = 4),
                              actionButton("go", "Find Route"),
                              tags$img(src = "https://i.postimg.cc/15RKfLc1/qrcode-climateassistant-shinyapps-io.png", class = "sidebar-photo")
                            ),
                            
                            
                            mainPanel(
                              leafletOutput("map", height = "400px"),
                              tags$br(),
                              p("(Response time around 15 seconds)"),
                              leafletOutput("plot1"),
                              verbatimTextOutput("loc_climate_data1"),
                              leafletOutput("plot2"),
                              verbatimTextOutput("loc_climate_data2"),
                              leafletOutput("plot3"),
                              verbatimTextOutput("loc_climate_data3"),
                              leafletOutput("plot4"),
                              verbatimTextOutput("loc_climate_data4"),
                              h3("Mistral AI: Tips on Planning (What to take for the trip)"),
                              verbatimTextOutput("gpt_response")
                            )
                          )
                 )
                 
                 
)


server <- function(input, output, session) {
  
  
  output$map <- renderLeaflet(
    plot_weather_layers(lat = 37.0902,
                        lon = -95.7129, zoom = 3.5, date = Sys.Date())
  )
  
  response_text <- reactiveVal("")
  
  
  observeEvent(input$user_location, {
    req(input$user_location)
    
    if (!is.null(input$user_location$error)) {
      output$location <- renderText({ input$user_location$error })
      output$city <- renderText({ "" })
      return()
    }
    
    lat <- input$user_location$lat
    lon <- input$user_location$lon
    
    
    
    # Reverse geocode to get city name
    url <- paste0("https://nominatim.openstreetmap.org/reverse?format=json&lat=",
                  lat, "&lon=", lon, "&zoom=10")
    
    city <- tryCatch({
      res <- jsonlite::fromJSON(url)
      res$address$city %||% res$address$town %||% res$address$village %||% "Unknown"
    }, error = function(e) "Unknown")
    
    output$plot_map <- renderLeaflet((plot_weather_layers(
      lat = lat , lon = lon, zoom = 15, date = Sys.Date())))
    
    l <- get_weather_forecast(city, 7, as.integer(input$day))
    output$loc_climate_data <- renderPrint(l) 
    
    api_key_ai <- "KD0FNuhXXgCoRcdmVewjFrVyfwI3fums"
    model_ai <- "mistral-large-latest"
    
    
    prompt <- paste("give me dressing recommendation based on these climates: start with on",
                    Sys.Date() + as.integer(input$day) - 1,"min tempreture",
                    l$min_temp, "max tempreture:", l$max_temp, "main weather", 
                    l$weather_main, "wind speed is", l$wind_speed,
                    "humidity is", l$humidity,"at",city)
    
    response <- POST(
      url = "https://api.mistral.ai/v1/chat/completions",
      add_headers(
        "Authorization" = paste("Bearer", api_key_ai),
        "Content-Type" = "application/json"
      ),
      body = toJSON(list(
        model = model_ai,
        messages = list(
          list(role = "user", content = prompt)
        )
      ), auto_unbox = TRUE),
      encode = "json"
    )
    
    result <- content(response, as = "parsed")
    message_content <- result$choices[[1]]$message$content
    
    output$ai_response <- renderText({message_content})
    
  })
  
  observeEvent(input$go, {
    day1 <- input$day_loc1
    
    loc1 <- locations[locations$name == input$loc1, ]
    loc2 <- locations[locations$name == input$loc2, ]
    loc3 <- locations[locations$name == input$loc3, ]
    loc4 <- locations[locations$name == input$loc4, ]
    
    if (nrow(loc1) == 0 | nrow(loc2) == 0 | nrow(loc3) == 0 | nrow(loc4) == 0) 
      return()
    
    route1 <- osrmRoute(src = c(loc1$lon, loc1$lat), 
                        dst = c(loc2$lon, loc2$lat), 
                        returnclass = "sf")
    
    route2 <- osrmRoute(src = c(loc2$lon, loc2$lat), 
                        dst = c(loc3$lon, loc3$lat), 
                        returnclass = "sf")
    
    route3 <- osrmRoute(src = c(loc3$lon, loc3$lat), 
                        dst = c(loc4$lon, loc4$lat), 
                        returnclass = "sf")
    
    leafletProxy("map") %>%
      clearShapes() %>%
      clearMarkers() %>%
      addTiles() %>%
      addMarkers(lng = loc1$lon, lat = loc1$lat, popup = input$loc1) %>%
      addMarkers(lng = loc2$lon, lat = loc2$lat, popup = input$loc2) %>%
      addMarkers(lng = loc3$lon, lat = loc3$lat, popup = input$loc3) %>%
      addMarkers(lng = loc4$lon, lat = loc4$lat, popup = input$loc4) %>%
      addPolylines(data = route1, color = "purple", weight = 5) %>%
      addPolylines(data = route2, color = "purple", weight = 5) %>%
      addPolylines(data = route3, color = "purple", weight = 5)
    
    
    api_key_ai <- "KD0FNuhXXgCoRcdmVewjFrVyfwI3fums" 
    model_ai <- "mistral-large-latest"
    
    output$plot1 <- renderLeaflet((plot_weather_layers(
      lat = loc1$lat, lon = loc1$lon, zoom = 10, date = Sys.Date())))
    
    l1 <- get_weather_forecast(loc1$name, 7, as.integer(input$day_loc1))
    
    output$loc_climate_data1 <- renderPrint(get_weather_forecast(loc1$name,
                                                                 7, as.integer(input$day_loc1)))
    
    output$plot2 <- renderLeaflet((plot_weather_layers(
      lat = loc2$lat, lon = loc2$lon, zoom = 10, date = Sys.Date())))
    
    l2 <- get_weather_forecast(loc2$name, 7, as.integer(input$day_loc2))
    
    output$loc_climate_data2 <- renderPrint(get_weather_forecast(loc2$name, 
                                                                 7, as.integer(input$day_loc2)))
    
    output$plot3 <- renderLeaflet((plot_weather_layers(
      lat = loc3$lat, lon = loc3$lon, zoom = 10, date = Sys.Date())))
    
    l3 <- get_weather_forecast(loc3$name, 7, as.integer(input$day_loc3))
    
    output$loc_climate_data3 <- renderPrint(get_weather_forecast(loc3$name, 
                                                                 7, as.integer(input$day_loc3)))
    
    output$plot4 <- renderLeaflet((plot_weather_layers(
      lat = loc4$lat, lon = loc4$lon, zoom = 10, date = Sys.Date())))
    
    l4 <- get_weather_forecast(loc4$name, 7, as.integer(input$day_loc4))
    
    output$loc_climate_data4 <- renderPrint(get_weather_forecast(loc4$name, 
                                                                 7, as.integer(input$day_loc4)))
    
    prompt <- paste("give me trip plan(plan what cloths to take) based on these climates: min tempreture:", 
                    l1$min_temp, "and",l2$min_temp,"and", l3$min_temp, "and",
                    l4$min_temp , "max tempretuee:", l1$max_temp,"and", 
                    l2$max_temp, "and",l3$max_temp,"and", l4$max_temp,
                    "main weather", l1$weather_main,"and", l2$weather_main,"and",
                    l3$weather_main,"and", l4$weather_main,   "wind speed is",
                    l1$wind_speed,"and", l2$wind_speed, "and",l3$wind_speed,
                    "and",l4$wind_speed, "humidity is", l1$humidity, "and", 
                    l2$humidity,"and", l3$humidity, "and",l4$humidity)
    
    response <- POST(
      url = "https://api.mistral.ai/v1/chat/completions",
      add_headers(
        "Authorization" = paste("Bearer", api_key_ai),
        "Content-Type" = "application/json"
      ),
      body = toJSON(list(
        model = model_ai,
        messages = list(
          list(role = "user", content = prompt)
        )
      ), auto_unbox = TRUE),
      encode = "json"
    )
    
    result <- content(response, as = "parsed")
    message_content <- result$choices[[1]]$message$content
    
    output$gpt_response <- renderText({message_content})
    
  }) # end observeEvent(input$go
  
  
}

shinyApp(ui = ui, server = server)
