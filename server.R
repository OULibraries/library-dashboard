# Install Packages
library("lubridate")
library("dplyr")
library("ggplot2")
library("shiny")

# Path to our raw data
pathToSumaTotal <- "data/sumaTotal.csv"

# Activities within Suma we want to keep
activityList <- c("[78]","[79]")

# Link to data
sumaTotal <- read.csv(pathToSumaTotal)

# Filter data to only keep activities in activityList
sumaTotal <- filter(sumaTotal, activities %in% activityList)

# Rename activities so that they have names instead of ID numbers
levels(sumaTotal$activities) <- c("Null", "Group", "Mixed", "Individual")

# Start server function
shinyServer(function(input, output) {
  
# Warning text if date range is invalid 
  output$dateValidate <- renderText({validate(need(input$dates[2] >= input$dates[1], "End Date is earlier than Start Date."))})
  
# Get script from external source
  source(file="R/hclc.r", local=TRUE)
})