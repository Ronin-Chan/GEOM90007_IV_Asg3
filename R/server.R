# Server components for the dashboard                                          

# Load necessary libraries for data manipulation, rendering, and interactive features
library(dplyr)       # For data manipulation
library(shiny)       # For building the Shiny app
library(leaflet)     # For interactive map visualization
library(plotly)      # For creating interactive plots
library(glue)        # For string interpolation
library(shinyalert)  # For pop-up alerts in Shiny

#' The server function to handle the backend logic of the Shiny dashboard
#'
#' This function defines all the server-side logic for the Shiny app, including event 
#' handling, state management, and rendering outputs. It manages user interactions 
#' with the UI and updates the dashboard components dynamically.
server <- function(input, output, session) {

  #' Event handlers
  # These observers handle user input changes and update the reactive state of the app.

  # Handle incoming location data from JavaScript
  observeEvent(input$js_set_loc, {
    print("Set incoming location from JS client")
    lat <- as.numeric(input$js_set_loc$lat)  # Extract latitude from JS input
    lon <- as.numeric(input$js_set_loc$lon)  # Extract longitude from JS input
    state$filter_loc <- c(lat, lon)          # Update the location in the reactive state
  })

  # Handle toggle for displaying only free parking spaces
  observeEvent(input$filter_free, {
    state$filter_free <- input$filter_free  # Update the filter for free spaces
  })

  # Handle toggle for displaying only accessible (disabled) parking spaces
  observeEvent(input$filter_accessible, {
    state$filter_accessible <- input$filter_accessible  # Update the filter for accessible spaces
  })

  # Handle slider input for setting the search radius from the user's location
  observeEvent(input$filter_radius, {
    state$filter_radius <- input$filter_radius  # Update the radius filter
    state$radar_info <- get_radar_info(input$filter_radius)  # Update radar display info based on radius
  })

  # Handle input for minimum and maximum parking cost
  observeEvent(input$filter_cost_min, {
    state$filter_cost <- c(input$filter_cost_min, input$filter_cost_max) * 100  # Convert to cents
  })
  
  observeEvent(input$filter_cost_max, {
    state$filter_cost <- c(input$filter_cost_min, input$filter_cost_max) * 100  # Update the max cost filter
  })

  # Handle input for parking duration (in minutes)
  observeEvent(input$filter_duration, {
    state$filter_duration <- input$filter_duration * 60  # Convert from hours to minutes
  })

  # Increment the parking duration value when the "+" button is clicked
  observeEvent(input$filter_duration_inc, {
    updateNumericInput(inputId = "filter_duration", value = input$filter_duration + 1)
  })

  # Decrement the parking duration value when the "-" button is clicked
  observeEvent(input$filter_duration_dec, {
    updateNumericInput(inputId = "filter_duration", value = input$filter_duration - 1)
  })

  #' State -------------------------------------------------------------------
  # This section defines the reactive state that holds the application's current
  # configuration, filters, and user inputs.

  state <- reactiveValues()  # Initialize the reactive state object

  # Define a set of UI colors used in the app for different elements
  state$ui_colors <- list(
    "background" = "#FFFFFF",  # Main background color
    "lightgray" = "#E9E9EA",   # Light gray used in the UI
    "gray" = "#A8A8B5",        # Gray color for text or icons
    "darkgray" = "#949598",    # Darker gray for foreground elements
    "foreground" = "#52525F",  # Foreground color for text or interactive elements
    "highlight" = "#FFA500"    # Highlight color (orange)
  )

  # Define fonts used throughout the app's UI
  state$fonts <- list(
    "primary" = "'brandon-grotesque', 'Helvetica', 'Arial', sans-serif"  # Main font
    # You can add additional fonts here if needed, e.g., monospace or secondary fonts
  )

  # Default settings for various parking filters and map parameters
  state$filter_free <- FALSE           # Default setting for showing free parking
  state$filter_accessible <- FALSE     # Default setting for showing accessible parking
  state$filter_radius <- c(0, 1)       # Default search radius range (1 km)
  state$filter_cost <- c(0, 600)       # Default price range for parking (in cents)
  state$filter_duration <- 120         # Default parking duration (120 minutes)
  state$filter_tap <- FALSE            # Default setting for tap-and-go payment filter
  state$filter_cc <- FALSE             # Default setting for credit card payment filter
  state$filter_loc <- c(-37.810513, 144.962840)  # Default focus location (Melbourne Central)
  state$loc_search_name <- ""          # Default search location name
  state$loc_search_term <- ""          # Default search term input by the user

  # Observe reactive state and apply filtered data based on user inputs
  observe({
    state$filtered_data <- map_data(master_data, state)  # Update filtered data based on state
  })

  # Observe map zoom and update the markers accordingly based on the zoom level
  observe({
    leafletProxy("leaflet_map", data = state$filtered_data) %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      # Add parking bay markers dynamically
      leaflet::addMarkers(
        ~ longitude, ~ latitude,
        icon = ~ map_symbol_dynamic(input$leaflet_map_zoom, cost_per_hour, maximum_stay),
        clusterOptions = leaflet::markerClusterOptions(
          disableClusteringAtZoom = 18,         # Disable clustering at high zoom levels
          spiderfyOnMaxZoom = FALSE,            # Prevent spiderfying markers at max zoom
          removeOutsideVisibleBounds = TRUE     # Remove markers outside visible map bounds
        ),
        clusterId = "clusters"
      ) %>%
      # Add a marker for the user's selected location
      leaflet::addMarkers(
        lat = state$filter_loc[1],
        lng = state$filter_loc[2],
        icon = map_symbol("pos_1"),             # Use the custom position marker
        clusterOptions = NULL,
        options = leaflet::markerOptions(clickable = FALSE)
      )
  })

  #' Master data
  # Load the master dataset containing all parking bay information
  master_data <- load_master_data_local()

  #' Mapping 
  # Render the map using leaflet and apply the initial filters
  output$leaflet_map <- renderLeaflet(
    map_renderer(state$filtered_data, state)  # Render the map based on filtered data
  )

  # Other app components

  #' Handle user clicks on map markers to display parking bay information
  observeEvent(input$leaflet_map_marker_click, {
    # Find the selected marker from the filtered dataset
    selected_marker <- state$filtered_data %>%
      filter(longitude == input$leaflet_map_marker_click$lng & 
             latitude == input$leaflet_map_marker_click$lat) %>%
      distinct(bay_id, .keep_all = TRUE)

    # Determine visibility for disability and free parking icons
    disability <- ifelse(!is.na(selected_marker$disability_deviceid), "", "hidden")
    free <- ifelse(!is.na(selected_marker$cost_per_hour), "hidden", "")

    # Get the type of meter for this parking bay based on maximum stay
    meter_type <- get_meter_type(selected_marker$maximum_stay)

    # Extract location, cost, and time information for the selected parking bay
    location <- selected_marker$street
    cost <- sprintf("$%.2f", selected_marker$cost_per_hour / 100)
    start_time <- selected_marker$start_time
    end_time <- selected_marker$end_time

    # Check if sufficient data is available to display parking details
    is_restricted <- is.na(location) || is.na(cost) || is.na(start_time) || is.na(end_time)

    # Display different content based on whether parking restrictions apply
    if (is_restricted) {
      details <- tags$div(
        class = "bay-info-details",
        tags$p("No restrictions apply to this bay.")  # Show if no restrictions
      )
    } else {
      details <- tags$div(
        class = "bay-info-details",
        tags$p(tags$b("Location: "), location),
        tags$p(tags$b("Cost Per Hour: "), cost),
        tags$p(tags$b("Start Time: "), start_time),
        tags$p(tags$b("End Time: "), end_time)
      )
    }

    # Show a popup with detailed parking bay information
    shinyalert(
      title = "Bay Information",
      html = TRUE,
      showConfirmButton = FALSE,
      closeOnClickOutside = TRUE,
      closeOnEsc = TRUE,
      text = tags$div(
        class = "bay-info-wrapper",
        details,
        tags$div(
          class = "bay-info-icons",
          tags$img(class = "meter_type", src = meter_type, style = "width: 70px; height: 70px;"),
          tags$img(class = glue("disability {disability}"), src = "./disabled.svg", style = "width: 50px; height: 50px;"),
          tags$img(class = glue("free {free}"), src = "./free.svg", style = "width: 50px; height: 50px;")
        )
      )
    )
  })

  #' Function to determine the type of parking meter based on maximum stay
  #' @param maximum_stay_mins Maximum allowed parking time in minutes
  #' @return A string representing the parking meter type (e.g., "1P", "2P")
  get_meter_type <- function(maximum_stay_mins) {
    meter_hours <- maximum_stay_mins / 60  # Convert minutes to hours
    meter_type <- case_when(
      meter_hours == 1 ~ '1P.svg',
      meter_hours == 2 ~ '2P.svg',
      meter_hours == 3 ~ '3P.svg',
      meter_hours == 4 ~ '4P.svg',
      TRUE ~ 'P.svg'  # Default icon if no specific hour range is matched
    )
    return(meter_type)
  }

  #' Function to calculate the opacity and radius for the radar display
  #' @param filter_radius The user-selected radius range for parking search
  #' @return A list of opacity and radius values for rendering the radar
  get_radar_info <- function(filter_radius) {
    max_radius <- filter_radius[2]
    radar_info <- case_when(
      max_radius == 0 ~ list(0, 0),  # No radar if radius is 0
      max_radius == 0.25 ~ list(250, 0.11),
      max_radius == 0.5 ~ list(c(250, 500), c(0.12, 0.09)),
      max_radius == 0.75 ~ list(c(250, 500, 750), c(0.15, 0.1, 0.07)),
      max_radius == 1 ~ list(c(250, 500, 750, 1000), c(0.22, 0.13, 0.08, 0.05)),
      TRUE ~ list(0, 0)  # Default to no radar if radius is invalid
    )
    return(radar_info)
  }
}
