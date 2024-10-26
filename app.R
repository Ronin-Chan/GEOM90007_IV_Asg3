# App dependencies
source("./R/libraries.R")

# Data
source("./R/data.R")

# Mapping
source("./R/map.R")

# Define the Shiny app
shiny::shinyApp(ui, server)
