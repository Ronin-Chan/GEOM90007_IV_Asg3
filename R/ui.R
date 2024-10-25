# UI components for the dashboard                                              

# Import necessary libraries for UI components
library(shiny)          # For building the Shiny app interface
library(glue)           # For string interpolation in UI components
library(htmltools)      # For working with HTML within Shiny
library(shinyWidgets)   # For additional UI widgets and customization

# Headers
# Define the headers that include external styles, scripts, and favicons

headers <- tags$head(
  # Add favicon (icon displayed in browser tab)
  tags$link(rel = "icon", type = "image/x-icon", href = "favicon.svg"),
  
  # Custom CSS for leaflet map styling and interactive elements
  tags$style(HTML("
  .leaflet-interactive {
    stroke: #FFA500 !important;       /* Outline color for polygons (orange) */
    stroke-width: 1px !important;     /* Thinner outline for improved aesthetics */
    fill: #FFA500 !important;         /* Fill color for polygons (orange) */
    fill-opacity: 0.1 !important;     /* Adjust fill opacity for better visibility */
  }
")),

  # Import custom web fonts for text styling in the app
  tags$link(rel = "stylesheet", type = "text/css", href = "https://use.typekit.net/zvh8ynu.css"),
  tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/icon?family=Material+Icons"),

  # Custom styles for the search results panel
  tags$style(HTML("
    /* Styling for the search results dropdown */
    #search-results {
      position: absolute;
      top: 150px; /* Place below the search bar */
      left: 0;
      right: 0;
      z-index: 1000;
      background-color: white;
      border: 1px solid #ddd;
      max-height: 300px;
      overflow-y: auto; /* Allow vertical scrolling for long results */
      box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1); /* Slight shadow for depth */
      display: none; /* Hidden by default */
    }
    .result-wrapper {
      padding: 10px;
      border-bottom: 1px solid #eee;
      cursor: pointer; /* Highlightable results */
    }
    .result-wrapper:hover {
      background-color: #f1f1f1; /* Highlight on hover */
    }
    .result-none {
      padding: 10px;
      color: #888; /* Styling for no-results message */
    }
  ")),

  # JavaScript for handling search functionality and GPS location fetching
  tags$script(
    HTML("
      document.addEventListener('DOMContentLoaded', function() {
        // Search button click listener
        document.getElementById('button-search').addEventListener('click', function() {
          var searchValue = document.getElementById('search-input').value;
          if (searchValue) {
            // Fetch search results from OpenStreetMap's Nominatim API
            fetch(`https://nominatim.openstreetmap.org/search?format=json&countrycodes=au&q=${searchValue}&viewbox=144.93366,-37.79264,144.97670,-37.82391&bounded=1`, {
              headers: {
                'User-Agent': 'MyApp/1.0 (your-email@example.com)'
              }
            })
              .then(response => response.json())
              .then(data => {
                var resultsPanel = document.getElementById('search-results');
                resultsPanel.innerHTML = '';
                resultsPanel.style.display = 'block'; // Display search results panel
                if (data.length > 0) {
                  data.forEach(function(location) {
                    var locationDiv = document.createElement('div');
                    locationDiv.classList.add('result-wrapper');
                    locationDiv.innerHTML = `<strong>${location.display_name}</strong>`;
                    locationDiv.addEventListener('click', function() {
                      Shiny.setInputValue('js_set_loc', { lat: location.lat, lon: location.lon });
                      resultsPanel.style.display = 'none'; // Hide panel after selection
                    });
                    resultsPanel.appendChild(locationDiv);
                  });
                } else {
                  resultsPanel.innerHTML = '<div class=\"result-none\">No results found, please try again.</div>';
                }
              });
          }
        });

        // GPS button click listener
        document.getElementById('button-gps').addEventListener('click', function() {
          if (navigator.geolocation) {
            navigator.geolocation.getCurrentPosition(function(position) {
              var lat = position.coords.latitude;
              var lon = position.coords.longitude;
              Shiny.setInputValue('js_set_loc', {lat: lat, lon: lon});
            });
          }
        });
      });
    ")
  )
)

# Search Panel
# Defines the search panel for the app where users can input search terms or use GPS

search_panel <- fluidRow(
  class = "search-bar",
  tags$div(
    class = "wrapper",
    tags$div(
      class = "label", style = "color: black; font-size: 1.2em;", "Search"
    ),
    textInput(
      inputId = "search-input",            # Input for typing search terms
      label = NULL,
      placeholder = "Search Destination"   # Placeholder text for the input
    ),
    actionButton(
      inputId = "button-search",           # Search button
      label = "Search",
      icon = icon("search"),
      class = "button search"              # CSS class for styling the button
    ),
    actionButton(
      inputId = "button-gps",              # GPS button for fetching current location
      label = "Use GPS",
      icon = icon("location-arrow"),
      class = "button gps"                 # CSS class for styling the button
    )
  )
)

# Search Results Panel
# A panel to display search results dynamically from the user's query

search_results_panel <- tags$div(
  id = "search-results",                 # Panel for search results
  class = "search-results-panel"         # CSS class for styling the panel
)

# Filter Sidebar Panel
# Sidebar panel that contains various filter options for users to adjust

toggle_sidebar_button <- actionButton(
  inputId = "toggle-sidebar",            # Button to toggle the visibility of the sidebar
  label = "Toggle Filters",
  icon = icon("filter"),
  class = "toggle-sidebar"
)

filter_sidebar <- sidebarPanel(
  class = "sidebar",                     # CSS class for sidebar styling
  # toggle_sidebar_button,               # Optional button to toggle the sidebar visibility
  search_panel,                          # Embed the search panel in the sidebar
  search_results_panel,                  # Embed the search results panel
  tags$div(class = "spacer h32"),        # Spacer for layout spacing (32px height)

  # Checkbox to filter only free parking spots
  fluidRow(
    class = "control",
    checkboxInput(
      inputId = "filter_free",
      label = "Show only free ($0) spots",
      value = FALSE
    )
  ),
  
  # Checkbox to filter only disability-accessible parking spots
  fluidRow(
    class = "control",
    checkboxInput(
      inputId = "filter_accessible",
      label = "Show only disability-friendly spots",
      value = FALSE
    )
  ),

  tags$div(class = "spacer h32"),        # Spacer for layout spacing (32px height)

  # Slider for adjusting the radius filter (0 to 1 km)
  fluidRow(
    class = "control",
    tags$div(
      class = "label",
      style = "color: black; font-size: 1.2em;",
      "Radius"
    ),
    sliderInput(
      inputId = "filter_radius",         # Slider for adjusting the search radius
      min = 0,
      max = 1,
      step = 0.25,
      value = c(0, 1),                  # Default range is 0 to 1 km
      dragRange = TRUE,
      label = NULL,
      post = "km"                       # Add "km" unit to the slider values
    )
  ),

  tags$div(class = "spacer h32"),        # Spacer for layout spacing (32px height)

  # Numeric input for minimum and maximum cost per hour
  fluidRow(
    class = "control input-small cost",
    tags$div(
      class = "label",
      style = "color: black; font-size: 1.2em;",
      "Cost per hour"
    ),
    fluidRow(
      style = "display: flex; gap: 5px; align-items: flex-start; margin-left: 0px;",
      numericInput(
        inputId = "filter_cost_min",     # Minimum cost input
        label = "min",
        value = 0,
        width = "80px"
      ),
      numericInput(
        inputId = "filter_cost_max",     # Maximum cost input
        label = "max",
        value = 6,
        width = "80px"
      )
    )
  ),

  tags$div(class = "spacer h32"),        # Spacer for layout spacing (32px height)

  # Numeric input for adjusting parking duration with increment and decrement buttons
  fluidRow(
    class = "control input-small hide-label",
    tags$div(
      class = "label",
      style = "color: black; font-size: 1.2em;",
      "Parking duration (hours)"
    ),
    fluidRow(
      class = "w-150",
      style = "display: flex; gap: 5px; align-items: flex-start; margin-left: 0px;",
      actionButton(
        inputId = "filter_duration_dec", # Button to decrement parking duration
        class = "stepwise",
        label = "-"
      ),
      div(
        style = "padding-top: 0px;",
        numericInput(
          inputId = "filter_duration",   # Input for parking duration in hours
          label = NULL,
          value = 2,
          width = "60px"
        )
      ),
      actionButton(
        inputId = "filter_duration_inc", # Button to increment parking duration
        class = "stepwise",
        label = "+"
      )
    )
  )
)

# Main Panel
# Defines the main panel of the app where the map is displayed

main_panel <- mainPanel(
  class = "main-panel",                 # CSS class for the main panel
  leafletOutput(                        # Output area for rendering the Leaflet map
    "leaflet_map",
    height = "100vh",                   # Full height for the map
    width = "100%"                      # Full width for the map
  )
)

# UI Layout
# Defines the layout of the UI with the sidebar and main panel

ui <- fluidPage(
  chooseSliderSkin("Flat", color = "#FFA500"),  # Custom slider skin for consistency with theme
  headers,                                     # Include the headers defined earlier
  sidebarLayout(                               # Layout with sidebar and main panel
    sidebarPanel = filter_sidebar,             # Sidebar panel for search and filters
    mainPanel = main_panel                     # Main panel for displaying the map
  )
)
