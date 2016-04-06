# External source file for HCLC-related data

# Interactive Plot for Dashboard, under Site Visits. Use to demonstrate interactivity
# Wrapping data file in reactive statement lets it change according to inputs
df_plot1 <- reactive({
  
# Filter down data file using dplyr 
  sumaTotal <- sumaTotal %>%
    
#   Group data by location, sessionId, sessionEnd, and activity
    group_by(sessionId, sessionEnd, location, activities) %>%
    
#   Create new column, "value", which is the sum of records as broken down by the grouping above
    summarize(value=n()) %>%
    
#   Filter data to fall between start date and end date from UI - "input$dates[1]" for start and "input$dates[2]" for end
    filter(as.Date(sessionEnd) >= as.Date(input$dates[1]) & as.Date(sessionEnd) <= as.Date(input$dates[2])) %>%
    
#   Group and summarize by sessionId and hour
    group_by(sessionId, countHour=hour(sessionEnd)) %>%
    summarize(valSum=sum(value)) %>%
    
#   Group by hour and return the mean result as valMean
    group_by(countHour) %>%
    summarize(valMean=mean(valSum))
})

# renderPlot lets you create a plot the way you would normally, but makes it reactive. Saving the result as output$plot1 lets us call it in the UI.
output$plot1 <- renderPlot({
  
# Regular old ggplot stuff, except we call our data as df_plot1() instead of just df_plot1 because it is reactive
  p <- ggplot(data=df_plot1(),aes(x=countHour,y=valMean))
  pl <- p + 
    geom_line() + 
    geom_text(aes(label=round(valMean,digits=2)), nudge_y = 2) + 
    geom_point() +
    labs(x="Hour", y="Average Number of Occupants")
  return(pl)
})

# renderPrint returns text we can call as an output for the UI
output$plot1_data <- renderPrint({
  
# nearPoints returns data from points in our file that are near our input event, input$plot1_click
  res <- nearPoints(df_plot1(), input$plot1_click,
                    
#                   Points must be within 10 pixels of our input, and maxpoints tells us how many will be returned
                    threshold = 10, maxpoints = 1,

#                   Adds a column to the data that registers the distance from our click to the point
                    addDist = TRUE)
  
# Rounds the distance column to 1 digit  
  res$dist_ <- round(res$dist_, 1)
  
# We can return any data from the row associated with our click using $ as a selector
  return(res$valMean)
})

# Main Dashboard graph for HCLC
# Format data
df_mainPlot <- reactive({
  sumaTotal <- sumaTotal %>%
    group_by(sessionId, sessionEnd, location, activities) %>%
    summarize(value=n()) %>%
    filter(as.Date(sessionEnd) >= as.Date(input$dates[1]) & as.Date(sessionEnd) <= as.Date(input$dates[2])) %>%
    group_by(sessionId, countHour=hour(sessionEnd)) %>%
    summarize(valSum=sum(value)) %>%
    group_by(countHour) %>%
    summarize(valMean=mean(valSum))
})
# Create graph
output$mainHclc <- renderPlot({
  p <- ggplot(data=df_mainPlot())
  pl <- p + 
    geom_line(aes(x=countHour,y=valMean)) + 
    geom_text(aes(x=countHour, y=valMean, label=round(valMean,digits=2)), nudge_y = 2) + 
    geom_point(aes(x=countHour,y=valMean)) +
    labs(x="Hour", y="Average Number of Occupants")
  return(pl)
})


# Initial formatting of data for HCLC tab
totals <- reactive({
  sumaTotal %>%
    group_by(sessionId, sessionEnd, location, activities) %>%
    summarize(value=n()) %>%
#   Filter by date and by making sure that our location is selected on input$checkGroup from the UI
    filter(as.Date(sessionEnd) >= as.Date(input$dates[1]) & as.Date(sessionEnd) <= as.Date(input$dates[2]) & location %in% input$checkGroup)
})

# Average Occupancy of Entire HCLC
# Format data for this graph
df_totalMean <- reactive({
  totals() %>%
    group_by(sessionId, countHour=hour(sessionEnd)) %>%
    summarize(valSum=sum(value)) %>%
    group_by(countHour) %>%
    summarize(valMean=mean(valSum))
})

# Create Graph
output$totalMean <- renderPlot({
  p <- ggplot(data=df_totalMean(),aes(x=countHour,y=valMean))
  pl <- p + 
    geom_line() + 
    geom_text(aes(label=round(valMean,digits=2)), nudge_y = 2) + 
    geom_point() +
    labs(x="Hour", y="Average Number of Occupants")
  return(pl)
})


# Occupancy by time and location
# Format Data
df_locMean <- reactive({
  totals() %>%
    group_by(sessionId, location, countHour=hour(sessionEnd)) %>%
    summarize(valSum=sum(value), valMean=mean(value)) %>%
    group_by(location, countHour) %>%
    summarize(valMean=mean(valSum))
})
# Create graph
output$locMean <- renderPlot({
  p <- ggplot(data=df_locMean())
  pl <- p + 
    geom_line(aes(x=countHour,y=valMean,color=location)) + 
    scale_color_discrete(name="Locations",guide=guide_legend(reverse=TRUE)) + 
    geom_text(aes(x=countHour, y=valMean, color=location, label=round(valMean, digits=2)), nudge_y = 1) + 
    geom_point(aes(x=countHour,y=valMean,color=location)) +
    labs(x="Hour", y="Average Number of Occupants")
  return(pl)
})


# Area graph of individuals vs. groups
# Format Data
df_actFill <- reactive({
  totals() %>%
    group_by(sessionId, activities, countHour=hour(sessionEnd)) %>%
    summarize(valSum=sum(value), valMean=mean(value)) %>%
    group_by(activities, countHour) %>%
    summarize(valMean=mean(valSum))
})

# Create Graph
output$actFill <- renderPlot({
  p <- ggplot(data=df_actFill()) 
  pl <- p + 
    geom_area(aes(x=countHour,y=valMean,fill=activities),color="black",position = "fill") + 
    scale_fill_discrete(name=NULL,guide=guide_legend(reverse=TRUE)) +
    labs(x="Hour", y="Percent of Users")
  return(pl)
})

# Create Copy of Graph for second tab - we can only call a given output once
output$actFillCopy <- renderPlot({
  p <- ggplot(data=df_actFill()) 
  pl <- p + 
    geom_area(aes(x=countHour,y=valMean,fill=activities),color="black",position = "fill") + 
    scale_fill_discrete(name=NULL, labels=c("Group","Individual"), guide=guide_legend(reverse=TRUE)) +
    labs(x="Hour", y="Percent of Users")
  return(pl)
})


# Weekday and time heatmap
# Format Data
df_wday <- reactive({
  totals <- totals() %>%
    group_by(sessionId, weekday=wday(sessionEnd),countHour=hour(sessionEnd)) %>%
    summarize(valSum=sum(value)) %>%
    group_by(weekday, countHour) %>%
    summarize(valMean=mean(valSum))
})

# Create graph
output$wdayHeat <- renderPlot({a
  p <- ggplot(data=df_wday(),aes(x=countHour, y=weekday))
  pl <- p + 
    geom_tile(aes(fill=valMean), color="black") + 
    scale_fill_gradient(low="mistyrose", high="navy", guide=guide_colorbar(title=expression(paste("Average \nNumber of Users")))) +
    scale_y_continuous(trans="reverse", breaks=1:7, labels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")) +
    labs(x="Hour", y="Weekday")
  return(pl)
})

# Text output from clicking on un-faceted graph
output$wd_data <- renderText({
  res <- nearPoints(df_wday(), 
                    input$wd_click,
                    threshold = 10, maxpoints = 1,
                    addDist = TRUE)
  res$dist_ <- round(res$dist_, 1)
  return(paste('Average number of users:', res$valMean))
})

# Data file that includes activities so that we can facet them
df_wday2 <- reactive({
  totals <- totals() %>%
    group_by(sessionId, activities, weekday=wday(sessionEnd),countHour=hour(sessionEnd)) %>%
    summarize(valSum=sum(value)) %>%
    group_by(activities, weekday, countHour) %>%
    summarize(valMean=mean(valSum))
})

# Create graph with activities facets
output$wdActHeat <- renderPlot({
  p <- ggplot(data=df_wday2())
  pl <- p + 
    geom_tile(aes(x=countHour, y=weekday, fill=valMean), color="black") + 
    scale_fill_gradient(low="mistyrose", high="navy", guide=guide_colorbar(title=expression(paste("Average \nNumber of Users")))) +
    scale_y_continuous(trans="reverse", breaks=1:7, labels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")) +
    facet_grid(activities ~ .) +
    labs(x="Hour", y="Weekday")
  return(pl)
})

# Text output from clicking on faceted graph
output$wd_data2 <- renderText({
  res <- nearPoints(df_wday2(), 
                    input$wd_click2,
                    threshold = 10, maxpoints = 1,
                    addDist = TRUE)
  res$dist_ <- round(res$dist_, 1)
  return(paste('Average number of users:', res$valMean))
})

