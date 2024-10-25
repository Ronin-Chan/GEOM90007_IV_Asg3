################################################################################
# UI components for the dashboard                                              #
################################################################################
library(shiny)
library(glue)
library(htmltools)

# Headers----------------------------------------------------------------------

headers <- tags$head(
  # favicon
  tags$link(rel = "icon", type = "image/x-icon", href = "favicon.svg"),
  tags$style(HTML("
  .leaflet-interactive {
    stroke: #FFA500 !important;       /* Outline color for polygons (orange) */
    stroke-width: 1px !important;     /* Thinner outline */
    fill: #FFA500 !important;         /* Fill color for polygons (orange) */
    fill-opacity: 0.1 !important;     /* Adjust fill opacity for better visibility */
  }
")),
  # web fonts
  tags$link(rel = "stylesheet", type = "text/css", href = "https://use.typekit.net/zvh8ynu.css"),
  tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/icon?family=Material+Icons"),
  tags$style(HTML("
    /* Overlay styling for search results */
    #search-results {
      position: absolute;
      top: 150px; /* Adjust this to be below the search bar */
      left: 0;
      right: 0;
      z-index: 1000;
      background-color: white;
      border: 1px solid #ddd;
      max-height: 300px;
      overflow-y: auto;
      box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
      display: none; /* Hidden by default */
    }
    .result-wrapper {
      padding: 10px;
      border-bottom: 1px solid #eee;
      cursor: pointer;
    }
    .result-wrapper:hover {
      background-color: #f1f1f1;
    }
    .result-none {
      padding: 10px;
      color: #888;
    }
  ")),
  # JavaScript for search and GPS functionality
  tags$script(
    HTML("
      document.addEventListener('DOMContentLoaded', function() {
        // Search button click listener
        document.getElementById('button-search').addEventListener('click', function() {
          var searchValue = document.getElementById('search-input').value;
          if (searchValue) {
            fetch(`https://nominatim.openstreetmap.org/search?format=json&countrycodes=au&q=${searchValue}&viewbox=144.93366,-37.79264,144.97670,-37.82391&bounded=1`, {
              headers: {
                'User-Agent': 'MyApp/1.0 (your-email@example.com)'
              }
            })
              .then(response => response.json())
              .then(data => {
                var resultsPanel = document.getElementById('search-results');
                resultsPanel.innerHTML = '';
                resultsPanel.style.display = 'block'; // Show the results panel
                if (data.length > 0) {
                  data.forEach(function(location) {
                    var locationDiv = document.createElement('div');
                    locationDiv.classList.add('result-wrapper');
                    locationDiv.innerHTML = `<strong>${location.display_name}</strong>`;
                    locationDiv.addEventListener('click', function() {
                      Shiny.setInputValue('js_set_loc', { lat: location.lat, lon: location.lon });
                      resultsPanel.style.display = 'none'; // Hide the panel after selection
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

# Search Panel------------------------------------------------------------------

search_panel <- fluidRow(
  class = "search-bar",
  tags$div(
    class = "wrapper",
    tags$div(
      class = "label", style = "color: black; font-size: 1.2em;", "Search"
    ),
    textInput(
      inputId = "search-input",
      label = NULL,
      placeholder = "Search Destination"
    ),
    actionButton(
      inputId = "button-search",
      label = "Search",
      icon = icon("search"),
      class = "button search"
    ),
    actionButton(
      inputId = "button-gps",
      label = "Use GPS",
      icon = icon("location-arrow"),
      class = "button gps"
    )
  )
)

# Search Results Panel-------------------------------------------------------

search_results_panel <- tags$div(
  id = "search-results",
  class = "search-results-panel"
)

# Filter Sidebar Panel---------------------------------------------------------

toggle_sidebar_button <- actionButton(
  inputId = "toggle-sidebar",
  label = "Toggle Filters",
  icon = icon("filter"),
  class = "toggle-sidebar"
)

filter_sidebar <- sidebarPanel(
  class = "sidebar",
  # titlePanel("Filters"),
  # toggle_sidebar_button,
  search_panel,
  search_results_panel,
  tags$div(class = "spacer h32"),
  fluidRow(
    class = "control",
    checkboxInput(
      inputId = "filter_free",
      label = "Show only free ($0) spots",
      value = FALSE
    )
  ),
  fluidRow(
    class = "control",
    checkboxInput(
      inputId = "filter_accessible",
      label = "Show only disability-friendly spots",
      value = FALSE
    )
  ),
  tags$div(class = "spacer h32"),
  fluidRow(
    class = "control",
    tags$div(
      class = "label",
      style = "color: black; font-size: 1.2em;",
      "Radius"
    ),
    sliderInput(
      inputId = "filter_radius",
      min = 0,
      max = 1,
      step = 0.25,
      value = c(0, 1),
      dragRange = TRUE,
      label = NULL,
      post = "km"
    )
  ),
  tags$div(class = "spacer h32"),
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
        inputId = "filter_cost_min",
        label = "min",
        value = 0,
        width = "80px"
      ),
      numericInput(
        inputId = "filter_cost_max",
        label = "max",
        value = 6,
        width = "80px"
      )
    )
  ),
  tags$div(class = "spacer h32"),
  fluidRow(
    class = "control input-small hide-label",
    tags$div(
      class = "label",
      style = "color: black; font-size: 1.2em;",
      "Parking duration(hours)"
    ),
    fluidRow(
      class = "w-150",
      style = "display: flex; gap: 5px; align-items: flex-start; margin-left: 0px;",
      actionButton(
        inputId = "filter_duration_dec",
        class = "stepwise",
        label = "-"
      ),
      div(
        style = "padding-top: 0px;",
        numericInput(
          inputId = "filter_duration",
          label = NULL,
          value = 2,
          width = "60px"
        )
      ),
      actionButton(
        inputId = "filter_duration_inc",
        class = "stepwise",
        label = "+"
      )
    )
  )
)

# Main Panel-------------------------------------------------------------------

main_panel <- mainPanel(
  class = "main-panel",
  leafletOutput(
    "leaflet_map",
    height = "100vh",
    width = "100%"
  )
)

# UI element-------------------------------------------------------------------

ui <- fluidPage(
  headers,
  # toggle_sidebar_button,
  sidebarLayout(
    sidebarPanel = filter_sidebar,
    mainPanel = main_panel
  )
)
