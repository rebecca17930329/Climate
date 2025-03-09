library(shiny)
library(httr) #
library(leaflet) #
library(jsonlite) #
library(rsconnect)
library(lubridate)

plot_weather_layers <- function(lat, lon, zoom, date = Sys.Date()) {
  
  api_key <- "05652b4942294328de9622b2ba85c51a"
  
  unix_timestamp <- as.numeric(as.POSIXct(date, origin = "1970-01-01", tz = "UTC"))
  
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
                             layer, "/{z}/{x}/{y}?date=", unix_timestamp, "&appid=", api_key),
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
      stop(":x: Selected day is out of range. Please choose a value between 1 and ", cnt, "!")
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

message <- "Based on the given weather conditions for Los Angeles on 2025-03-09, here's a dressing recommendation:

### Weather Summary:
- **Min Temperature:** 54.3°F (12.4°C)
- **Max Temperature:** 70.8°F (21.6°C)
- **Main Weather:** Clear
- **Wind Speed:** 9.1 mph
- **Humidity:** 26%

### Dressing Recommendation:

#### Morning (Cooler Temperatures):
- **Layering:** Start with a lightweight base layer, such as a t-shirt or a long-sleeved shirt.
- **Outerwear:** A light jacket or a sweater would be appropriate. A denim jacket or a light fleece could work well.
- **Bottoms:** Pair with jeans or lightweight pants.
- **Footwear:** Comfortable sneakers or ankle boots.
- **Accessories:** Consider a scarf or a light hat to add a bit of warmth.

#### Afternoon (Warmer Temperatures):
- **Layering:** You might want to shed the outer layer as the temperature rises. A t-shirt or a short-sleeved shirt would be comfortable.
- **Outerwear:** If you need a layer, a light cardigan or a hoodie could be useful.
- **Bottoms:** Lightweight pants or even shorts if you're comfortable.
- **Footwear:** Sneakers or sandals would be suitable.
- **Accessories:** Sunglasses and a light cap or hat to protect from the sun.

#### Evening (Cooling Down):
- **Layering:** As the temperature drops, you might want to put on a light jacket or a sweater again.
- **Outerwear:** A light jacket or a sweater.
- **Bottoms:** Pants or jeans.
- **Footwear:** Comfortable shoes or boots.
- **Accessories:** A scarf or a light hat.

### General Tips:
- **Hydration:** Despite the low humidity, make sure to stay hydrated.
- **Sun Protection:** Use sunscreen and wear sunglasses, especially during the afternoon.
- **Wind Protection:** Although the wind speed is moderate, it might feel cooler, so a light jacket can help.

### Example Outfits:
1. **Morning:**
   - **Top:** Long-sleeved shirt + light fleece
   - **Bottom:** Jeans
   - **Footwear:** Sneakers
   - **Accessories:** Scarf, sunglasses

2. **Afternoon:**
   - **Top:** T-shirt
   - **Bottom:** Lightweight pants or shorts
   - **Footwear:** Sneakers or sandals
   - **Accessories:** Sunglasses, cap

3. **Evening:**
   - **Top:** Light sweater + t-shirt
   - **Bottom:** Jeans
   - **Footwear:** Ankle boots
   - **Accessories:** Scarf

This should keep you comfortable throughout the day in Los Angeles. Enjoy your day!"


ui <- navbarPage("Climate Assistant",
                 tabPanel("Climate Forecasting Assistant",
                          
                          mainPanel(
                            leafletOutput("map", height = "600px"),
                            tags$br(),
                            leafletOutput("plot1"),
                            tags$br(),
                            h5("10 days climate forcasting for Los Angeles"),
                            verbatimTextOutput("loc_climate_data1"),
                            verbatimTextOutput("loc_climate_data2"),
                            verbatimTextOutput("loc_climate_data3"),
                            verbatimTextOutput("loc_climate_data4"),
                            verbatimTextOutput("loc_climate_data5"),
                            verbatimTextOutput("loc_climate_data6"),
                            verbatimTextOutput("loc_climate_data7"),
                            verbatimTextOutput("loc_climate_data8"),
                            verbatimTextOutput("loc_climate_data9"),
                            verbatimTextOutput("loc_climate_data10"),
                            p(message)
                          )
                 )
                 
)

server <- function(input, output, session) {
  
  output$map <- renderLeaflet(
    plot_weather_layers(lat = 37.0902,
                        lon = -95.7129, zoom = 3.5, date = Sys.Date())
  )
  output$plot1 <- renderLeaflet((plot_weather_layers(
    lat = 34.0522, lon = -118.2437, zoom = 10, date = Sys.Date())))
  
  output$loc_climate_data1 <- renderPrint(get_weather_forecast("Los Angeles", 10, 1)) 
  output$loc_climate_data2 <- renderPrint(get_weather_forecast("Los Angeles", 10, 2)) 
  output$loc_climate_data3 <- renderPrint(get_weather_forecast("Los Angeles", 10, 3)) 
  output$loc_climate_data4 <- renderPrint(get_weather_forecast("Los Angeles", 10, 4)) 
  output$loc_climate_data5 <- renderPrint(get_weather_forecast("Los Angeles", 10, 5)) 
  output$loc_climate_data6 <- renderPrint(get_weather_forecast("Los Angeles", 10, 6)) 
  output$loc_climate_data7 <- renderPrint(get_weather_forecast("Los Angeles", 10, 7)) 
  output$loc_climate_data8 <- renderPrint(get_weather_forecast("Los Angeles", 10, 8)) 
  output$loc_climate_data9 <- renderPrint(get_weather_forecast("Los Angeles", 10, 9)) 
  output$loc_climate_data10 <- renderPrint(get_weather_forecast("Los Angeles", 10, 10)) 
  
  
  
  
  
}

shinyApp(ui = ui, server = server)


