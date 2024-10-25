# Libraries required for the app                                               

#' A vector of package dependencies used in the app
#'
#' These are the required libraries that the application needs in order to 
#' function properly. It includes libraries for JSON handling, data manipulation,
#' visualization, geographic data, and shiny components for interactive UI.
dependencies <- c(
  "rjson",         # For handling JSON data
  "jsonlite",      # Alternative JSON handling with additional flexibility
  "shiny",         # To build the interactive web application
  "dplyr",         # For data wrangling and manipulation
  "janitor",       # For cleaning up and formatting data frames
  "tidyr",         # For tidying and reshaping data
  "tidyselect",    # For selecting columns in dplyr pipelines
  "readxl",        # For reading Excel files into R
  "ggplot2",       # For creating static visualizations
  "plotly",        # For creating interactive plots
  "leaflet",       # For interactive maps and geographic visualizations
  "rworldmap",     # For mapping country-level data
  "sp",            # For handling spatial data (older spatial package)
  "glue",          # For string interpolation
  "colorspace",    # For managing color schemes in visualizations
  "htmltools",     # For handling HTML within shiny applications
  "pracma",        # For mathematical functions (e.g., Haversine formula)
  "RSocrata",      # For accessing Socrata open data platforms
  "geojsonio",     # For reading and writing GeoJSON spatial data
  "sf",            # For handling modern simple feature spatial data
  "shinyalert"     # For providing alert popups within shiny apps
)

#' Load required packages and install them if not already available
#'
#' This function attempts to load each of the packages listed in the `dependencies`
#' vector. If any of the packages are not installed on the system, the function 
#' will install them first from CRAN, and then load them.
#'
#' @param dependencies A character vector of package names that are required.
#' The function loops through this list and attempts to load each package.
load_dependencies <- function(dependencies) {
  print(dependencies)  # Display the list of dependencies for logging purposes
  
  # Loop through each package in the list of dependencies
  for (package in dependencies) {
    tryCatch({
      # Try to load the package
      library(package, character.only = TRUE)
    }, error = function(e) {
      # If loading fails, install the package from CRAN and then load it
      install.packages(package, repos = "https://cloud.r-project.org")
      library(package, character.only = TRUE)
    })
  }
}

#' Load dependencies only if running locally
#'
#' This condition checks if the code is being executed on shinyapps.io (or another
#' server environment) by looking at the current working directory. If the app 
#' is not running in a server environment (i.e., not on shinyapps.io), it will
#' attempt to load the dependencies locally.
if (!grepl("srv/connect", getwd())) {
  load_dependencies(dependencies)  # Load packages if running locally
}
