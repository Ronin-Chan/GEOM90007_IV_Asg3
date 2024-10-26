# Maps for the app                                                             

# Import required libraries for map rendering and data handling
library(dplyr)         # For data manipulation tasks (filtering, selecting)
library(leaflet)       # For creating interactive maps
library(htmltools)     # For HTML rendering in leaflet maps
library(glue)          # For string interpolation

#' Generates a custom map symbol (icon) for a parking bay
#'
#' This function creates a custom icon to represent parking bays on the map. 
#' The icon type can vary depending on the status (e.g., filled, unfilled) 
#' or other visual indicators like position markers.
#'
#' @param type Specifies the type of icon to be generated. Options include:
#'        - "filled" for occupied parking bays
#'        - "unfilled" for available parking bays
#'        - "placeholder", "marker", "pin", and others for custom markers
#' @param zoom Determines the zoom level of the map (default is 1).
#'
#' @return A leaflet icon object that can be added to the map.
map_symbol <- function(type = "filled", zoom = 1) {
  # Define the image file paths for different types of icons
  img <- case_when(
    type == "filled" ~ "./www/filled.svg",           # Occupied parking bay icon
    type == "unfilled" ~ "./www/unfilled.svg",       # Available parking bay icon
    type == "placeholder" ~ "./www/placeholder.svg", # Placeholder icon
    type == "marker" ~ "./www/marker.svg",           # Generic marker icon
    type == "pin" ~ "./www/pin.png",                 # Pin marker icon
    type == "pos_1" ~ "./www/pos_1.svg",             # Custom position marker 1
    type == "pos_2" ~ "./www/pos_2.svg"              # Custom position marker 2
  )
  
  size <- 24  # Set the size of the icon (width and height)
  
  # Create the icon object using leaflet's makeIcon function
  icon <- makeIcon(img, NULL, size, size, className = glue("marker {type}"))
  
  # Return the generated icon object
  return(icon)
}

#' Generates a dynamic map symbol based on zoom level, cost, and maximum stay
#'
#' This function creates dynamic map icons based on parameters such as 
#' zoom level, cost per hour, and maximum stay time. Icons change based 
#' on these conditions to provide a more intuitive map interface.
#'
#' @param zoom Current zoom level of the map.
#' @param cost_per_hour The cost for parking per hour, used to change the icon color.
#' @param maximum_stay The maximum allowed parking time in minutes, used to change the icon type.
#'
#' @return A leaflet icon object that dynamically adjusts based on the input parameters.
map_symbol_dynamic <- function(zoom, cost_per_hour, maximum_stay) {
  # Determine the type of icon based on zoom level and maximum stay time
  type <- case_when(
    (zoom <= 18) ~ "filled",                # Filled icon at lower zoom levels
    (zoom > 18) & (maximum_stay == 60) ~ "1P",  # 1-hour parking at higher zoom
    (zoom > 18) & (maximum_stay == 120) ~ "2P", # 2-hour parking at higher zoom
    (zoom > 18) & (maximum_stay == 180) ~ "3P", # 3-hour parking at higher zoom
    (zoom > 18) & (maximum_stay == 240) ~ "4P", # 4-hour parking at higher zoom
    TRUE ~ "P"                                # Default parking icon
  )

  # Determine the color of the icon based on cost (green for free parking)
  color <- case_when(
    is.na(cost_per_hour) ~ "_green",         # Green if cost is not available (assumed free)
    (cost_per_hour == 0) ~ "_green",         # Green for free parking
    TRUE ~ ""                                # Default color for paid parking
  )

  # Construct the file path for the icon image
  img <- paste0("./www/", type, color, ".svg")

  # Adjust icon size based on the zoom level (larger icons at higher zoom)
  size <- ifelse(zoom <= 18, 14, 20)
  
  # Create and return the dynamic icon
  icon <- makeIcon(img, NULL, size, size, className = glue("marker"))
  return(icon)
}

#' Renders a leaflet map with parking bay data and interactive features
#'
#' This function generates a leaflet map based on parking bay data and user-selected 
#' state parameters. It integrates a tile layer from Mapbox, clusters the parking bay 
#' markers, and includes various map controls like a radar circle and a scale bar.
#'
#' @param map_data The dataset containing parking bay information with spatial data.
#' @param state A reactive state object that includes user filters such as location and radius.
#'
#' @return A leaflet map widget ready to be rendered in the shiny app.
map_renderer <- function(map_data, state) {
  # Unpack filter parameters from the state object (e.g., radius of interest)
  filter_radius <- state$filter_radius

  # Define the template URL for Mapbox tiles, which are used as the base layer for the map
  mapbox_template <- paste0(
    "https://api.mapbox.com/styles/v1/expsuperdope/cm2jyrd2q008p01plhzqsek71/tiles/",
    "{z}/{x}/{y}?access_token=",
    "pk.eyJ1IjoiZXhwc3VwZXJkb3BlIiwiYSI6ImNtMmp5bGo5NTBhMGoycW92b2k4bjJ4N3EifQ.EkaXg8v7lN6iaJyJfxtptw"
  )

  # Generate the map using the leaflet package
  map <- map_data %>%
    # Initialize the leaflet map with zoom level constraints
    leaflet::leaflet(
      options = leaflet::leafletOptions(
        minZoom = 15,   # Minimum zoom level
        maxZoom = 20,   # Maximum zoom level
      ),
      # Set sizing options to ensure the map adapts to container size
      sizingPolicy = leaflet::leafletSizingPolicy(
        defaultWidth = "100%",
        defaultHeight = "100%"
      )
    ) %>%
    # Add the Mapbox tile layer as the base map
    leaflet::addTiles(
      urlTemplate = mapbox_template,
      # Options for the Mapbox tile layer
      options = tileOptions(
        minZoom = 15,   # Match the zoom constraints of the map
        maxZoom = 20,
        maxNativeZoom = 20
      )
    ) %>%
    # Add markers for parking bays, using map symbols for icons
    leaflet::addMarkers(
      ~longitude, ~latitude,                 # Longitude and latitude columns from data
      icon = map_symbol("filled"),            # Default icon (filled parking bay)
      clusterOptions = leaflet::markerClusterOptions(
        disableClusteringAtZoom = 18,         # Disable clustering at high zoom levels
        spiderfyOnMaxZoom = FALSE,            # Prevent spiderfying when zoomed in
        removeOutsideVisibleBounds = TRUE     # Remove clusters outside visible bounds
      ),
      clusterId = "clusters"                  # Cluster identifier for marker groups
    ) %>%
    # Add a circle (radar) centered on the user's location
    leaflet::addCircles(
      lat = state$filter_loc[1],              # Latitude of the user's location
      lng = state$filter_loc[2],              # Longitude of the user's location
      radius = unlist(state$radar_info[1]),   # Radius for the radar circle
      color = "#FFA500",                      # Orange color for the radar circle
      fillOpacity = unlist(state$radar_info[2]),  # Opacity for the radar circle fill
      weight = 0.2,                           # Circle border weight
      stroke = TRUE                           # Show circle border (stroke)
    ) %>%
    # Add a scale bar to the bottom left of the map for reference
    leaflet::addScaleBar(
      position = "bottomleft",                # Position of the scale bar
      options = scaleBarOptions(
        metric = TRUE,                        # Use metric units (e.g., meters)
        imperial = FALSE                      # Do not use imperial units
      )
    ) %>%
    # Add a simple north arrow to indicate direction on the map
    leaflet::addControl(
      html = htmltools::tags$div(style = "font-size: 32px;", "👆"), # North arrow icon
      position = "bottomright",              # Position of the north arrow
      className = "leaflet-control-north-arrow"  # Custom class for the north arrow control
    )
  
  # Return the generated map object for rendering
  return(map)
}

