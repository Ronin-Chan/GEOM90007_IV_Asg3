# Data handlers for the app                                                    

# Loading the required libraries for working with JSON, data manipulation, 
# API requests, spatial data processing, and mathematical operations.
library(jsonlite)    # To read and parse JSON files
library(dplyr)       # For data manipulation tasks (filtering, joining, selecting)
library(RSocrata)    # To access data from the Socrata API
library(glue)        # For string interpolation and formatting
library(tidyr)       # For data reshaping and tidying
library(stringr)     # For string manipulation
library(geojsonio)   # To read GeoJSON spatial data
library(rgeos)       # For spatial operations like calculating centroids
library(pracma)      # For mathematical functions like the Haversine formula

#' Load data from a JSON file and convert it into a dataframe
#'
#' This function reads a JSON file and flattens its structure into a 
#' dataframe, making it easier to work with in data analysis. 
#' Flattening helps convert nested fields into separate columns.
#'
#' @param filename The name or path of the JSON file to load.
#' @param ... Additional arguments to pass to jsonlite::fromJSON for custom handling.
#' @return A dataframe containing the JSON data in a flattened structure.
load_json <- function(filename, ...) {
  # Use jsonlite to read and flatten the JSON content into a dataframe
  df <- jsonlite::fromJSON(txt = filename, flatten = TRUE)
  
  # Print the number of rows in the dataframe to confirm successful loading
  print(glue("Loaded dataframe with {nrow(df)} rows"))
  
  # Return the dataframe for further processing
  return(df)
}


#' Load spatial data from a GeoJSON file and convert it into a spatial dataframe
#'
#' This function reads spatial data from a GeoJSON file and converts it into 
#' a spatial dataframe format, which is crucial for working with geographic 
#' data such as coordinates, polygons, and boundaries.
#'
#' @param filename The path to the GeoJSON file to load.
#' @param ... Additional parameters passed to geojsonio::geojson_read for customization.
#'
#' @return A spatial dataframe containing geographic features like coordinates and polygons.
load_geojson <- function(filename, ...) {
  # Use geojsonio to read the GeoJSON file into a spatial dataframe
  sp_df <- geojsonio::geojson_read(filename, what = "sp")
  
  # Print the number of rows in the spatial dataframe to verify successful loading
  print(glue("Loaded spatial dataframe with {nrow(sp_df)} rows"))
  
  # Return the spatial dataframe for further geographic processing
  return(sp_df)
}

#' Query data from the Socrata API based on the provided dataset identifier
#'
#' This function fetches data from the Socrata API using the specified dataset 
#' identifier. It allows for additional query parameters to be passed, enabling 
#' filtering or limiting the results directly from the API.
#'
#' @param dataset A string representing the name of the dataset to retrieve.
#' @param params A list of key-value pairs representing query parameters (optional).
#' @return A dataframe containing the data retrieved from the Socrata API.
#' @throws Error if the dataset is unrecognized or if there is an issue during the API request.
load_remote <- function(dataset, params = list()) {
  # Print a message indicating the dataset being loaded for logging purposes
  print(glue("Loading remote dataset: {dataset}"))

  # The app token required to access the Socrata API
  app_token <- "M9ZZYTOg8ADyERRdemysg1SOU"

  # Construct the query string from the provided parameters if any
  query <- ""
  if (length(params) > 0) {
    query_string <- paste(names(params), params, sep = "=", collapse = "&")
    query <- glue("?{query_string}")
  }

  # Map the dataset name to the corresponding API resource identifier
  resource <- switch(
    dataset,
    #' @see https://dev.socrata.com/foundry/data.melbourne.vic.gov.au/wuf8-susg
    "bays" = "wuf8-susg.json",           # On-street parking bay data
    #' @see https://dev.socrata.com/foundry/data.melbourne.vic.gov.au/vdsi-4gtj
    "meters" = "vdsi-4gtj.json",         # Parking meter data
    #' @see https://dev.socrata.com/foundry/data.melbourne.vic.gov.au/ntht-5rk7
    "restrictions" = "ntht-5rk7.json",   # Parking restrictions data
    #' @see https://dev.socrata.com/foundry/data.melbourne.vic.gov.au/vh2v-4nfs
    "sensors" = "vh2v-4nfs.json",        # Real-time parking sensor data
    #' @see https://dev.socrata.com/foundry/data.melbourne.vic.gov.au/7pgd-bdf2
    "sensors_2019" = "7pgd-bdf2.json",   # Historical sensor data from 2019
    #' @see https://dev.socrata.com/foundry/data.melbourne.vic.gov.au/7q9g-yyvg
    "paystay_restrictions" = "7q9g-yyvg.json",  # PayStay parking restrictions
    #' @see https://data.melbourne.vic.gov.au/Transport/Pay-Stay-parking-restrictions/ambt-72qg
    "paystay_segments" = "7q9g-yyvg.json",  # PayStay zone data
    TRUE: stop(glue("Unrecognised dataset {dataset}"))  # Throw error if dataset is not recognized
  )

  # Fetch the dataset from the Socrata API using the constructed query
  df <- read.socrata(
    glue("https://data.melbourne.vic.gov.au/resource/{resource}{query}"),
    app_token = app_token
  )
  
  # Print the number of rows loaded to verify data retrieval
  print(glue("Dataset {dataset} loaded with {nrow(df)} rows"))

  
  # Return the fetched data as a dataframe
  return(df)
}

#' Load and combine multiple datasets from local files into a single dataframe
#'
#' This function loads various local datasets (both JSON and GeoJSON files) 
#' and merges them based on common keys, such as bay_id. It combines parking 
#' data, restrictions, and sensor data to create a complete dataframe for further analysis.
#'
#' @return A merged dataframe containing all the relevant parking and sensor data.
load_master_data_local <- function() {
  #' @section City of Melbourne parking datasets -----------------------------

  # Load historical parking sensor data from 2019
  sensors <- load_json("./data/sensors_2019_09_27_0800.json") %>%
    rename(bay_id = bayid, occupied_id = deviceid) %>%
    select(c(bay_id, occupied_id))

  # Load on-street parking bay data (GeoJSON) and calculate the centroids of the bays
  bays_sp <- load_geojson("./data/bays.geojson")
  bay_centroids <- rgeos::gCentroid(bays_sp, byid = TRUE)
  
  # Extract dataframe from the spatial data and assign longitude and latitude from centroids
  bays <- bays_sp@data %>%
    mutate(
      longitude = bay_centroids@coords[, 1],  # Add longitude of bay centroid
      latitude = bay_centroids@coords[, 2]    # Add latitude of bay centroid
    )

  # Load parking restrictions for disabled-access bays
  disability <- load_json("./data/restrictions_disability_only.json") %>%
    rename(bay_id = bayid, disability_deviceid = deviceid) %>%
    select(c(bay_id, disability_deviceid))

  #' @section PayStay datasets -----------------------------------------------

  # Load PayStay parking restrictions and convert specific columns to numeric format
  paystay_restrictions <- load_json(
    "./data/paystay_restrictions_fri_0800.json"
  ) %>%
  mutate_at(c("cost_per_hour", "maximum_stay"), as.numeric)

  # Load PayStay zones and link them to street segments
  paystay_segments <- load_json("./data/paystay_segments.json") %>%
    rename(rd_seg_id = street_segment_id)  # Rename key for consistency

  # Merge all the datasets together into a single dataframe
  df <- bays %>%
    left_join(sensors, by = "bay_id") %>%
    left_join(disability, by = "bay_id") %>%
    left_join(paystay_segments, by = "rd_seg_id") %>%
    left_join(paystay_restrictions, by = "pay_stay_zone") 

  # Return the final combined dataframe
  return(df)
}

#' Filter the master dataset based on user-defined criteria for map rendering
#'
#' This function filters the master dataset according to parameters selected by 
#' the user, such as parking availability, distance, cost, and accessibility. 
#' The filtered data is then ready for use in visualizing relevant parking spots 
#' on a map.
#'
#' @param master_data The full dataset containing all parking and sensor data.
#' @param state A reactive object containing user-selected filter criteria.
#'
#' @return A filtered dataframe that meets the user’s criteria for map rendering.
map_data <- function(master_data, state) {
  # Extract the filter options from the state object
  filter_free <- state$filter_free  # Filter for free parking
  filter_accessible <- state$filter_accessible  # Filter for accessible parking
  filter_radius <- state$filter_radius  # Filter for distance radius
  filter_cost <- state$filter_cost  # Filter for cost range
  filter_duration <- state$filter_duration  # Filter for parking duration
  filter_loc <- state$filter_loc  # User's current location

  # Apply filters based on the user’s location and other criteria
  filtered <- master_data %>%
    rowwise() %>%
    mutate(
      distance = pracma::haversine(c(latitude, longitude), filter_loc)  # Calculate distance from user’s location
    ) %>%
    # Filter points within the specified radius
    filter(
      distance >= filter_radius[1] & distance <= filter_radius[2]
    ) %>%
    # Filter points by cost, free parking if cost_per_hour is NA
    filter(
      (filter_cost[1] == 0 & is.na(cost_per_hour)) |
      (cost_per_hour >= filter_cost[1] & cost_per_hour <= filter_cost[2])
    ) %>%
    # Filter points by minimum required parking duration
    filter(
      is.na(maximum_stay) | maximum_stay >= filter_duration
    )

  # Apply free parking filter if requested by the user
  if (filter_free) {
    filtered <- filtered %>% 
      filter(
        is.na(cost_per_hour) | cost_per_hour == 0
      )
  }

  # Apply accessible parking filter if requested by the user
  if (filter_accessible) {
    filtered <- filtered %>% filter(!is.na(disability_deviceid))
  }

  # Ensure only unique bays and filter out occupied bays
  filtered <- filtered %>%
    filter(is.na(occupied_id)) %>%
    distinct(bay_id, .keep_all = TRUE)

  # Print the filtered data and return the result
  print(filtered)
  return(filtered)
}

#' Load historical sensor data for a specific time period
#'
#' This function retrieves historical parking sensor data for a given date 
#' and time, allowing analysis of parking availability trends over time.
#'
#' @return A dataframe containing sensor data for the specified historical period.
load_historical_sensors <- function() {
  # Query the historical sensor data from 27-09-2019 at 08:00
  df <- load_remote("sensors_2019", params = list(
    "$where" = paste(
      "arrivaltime", "<=", "'2019-09-27T08:00:00.000'",
      "and",
      "departuretime", ">", "'2019-09-27T08:00:00.000'",
      sep = " "
    )
  )) %>%
  distinct(deviceid, .keep_all = TRUE)  # Keep only unique sensor entries

  # Display the data for inspection
  View(df)
  return(df)
}
