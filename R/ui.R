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


search_panel <- tabPanel(
  title = "Search",
  fluidRow(
    class = "logo",
    tags$img(
      src = "logo.svg"
    )
  ),
  fluidRow(
    class = "search-bar",
    tags$div(
      class = "wrapper",
      textInput(
        inputId = "search-input",
        label = "NGV",
        placeholder = "Search Destination"
      ),
      tags$div(
        id = "button-gps",
        class = "button gps"
      ),
      tags$div(
        id = "button-search",
        class = "button search"
      )
    )
  )
)

# Search Results Panel-------------------------------------------------------

search_results_panel <- tags$div(
  id = "search-results",
  class = "search-results-panel"  # Overlay panel for search results
)

# Filter Sidebar Panel---------------------------------------------------------

filter_sidebar <- sidebarPanel(
  fluidRow(
    class = "search-bar",
    tags$div(
      class = "wrapper",
      textInput(
        inputId = "search-input",
        label = "NGV",
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
  ),
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
      "Cost per hour"
    ),
    numericInput(
      inputId = "filter_cost_min",
      label = "min",
      value = 0
    ),
    tags$hr(
      class = "accent-connect"
    ),
    numericInput(
      inputId = "filter_cost_max",
      label = "max",
      value = 6
    )
  ),
  tags$div(class = "spacer h32"),
  fluidRow(
    class = "control input-small hide-label",
    tags$div(
      class = "label",
      "Parking duration"
    ),
    fluidRow(
      class = "w-150",
      actionButton(
        inputId = "filter_duration_dec",
        class = "stepwise",
        label = "-"
      ),
      numericInput(
        inputId = "filter_duration",
        label = "hours",
        value = 2
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
  leafletOutput(
    "leaflet_map",
    height = "100vh",
    width = "100%"
  )
)

# UI element-------------------------------------------------------------------
ui <- fluidPage(
  headers,
  sidebarLayout(
    sidebarPanel = filter_sidebar,
    mainPanel = main_panel
  )
)

