if (!require("rio"))install.packages("rio")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("shiny")) install.packages("shiny")  
if (!require("bslib")) install.packages("bslib")  
if (!require("ggplot2")) install.packages("ggplot2")  
if (!require("dplyr")) install.packages("dplyr")  
if (!require("tidyr")) install.packages("tidyr")  
if (!require("reactable")) install.packages("reactable") 
if (!require("DT")) install.packages("DT") 
if (!require("plotly")) install.packages("plotly") 
if (!require("highcharter")) install.packages("highcharter")
if (!require("waiter")) install.packages("waiter")  
if (!require("shinyFeedback")) install.packages("shinyFeedback")  
if (!require("shinyalert")) install.packages("shinyalert")  
if (!require('leaflet')) install.packages('leaflet')


library(rio)
library(tidyverse)
library(shiny)  
library(bslib)  
library(ggplot2)  
library(dplyr)  
library(tidyr)  
library(reactable)  
library(DT)  
library(plotly)  
library(waiter) 
library(shinyFeedback)  
library(shinyalert)  
library(highcharter)
library(leaflet)

-------#step1: Import the data and clean it 
  
retail_data <- import("data/online_retail.csv")
  
  str(retail_data)
  
  # Rename the column
  retail_data <- retail_data %>%
    rename(CustomerID = `Customer ID`)
  
  # Check the updated column names
  print(names(retail_data))
  
  retail_data <- retail_data %>%
    filter(!is.na(CustomerID)) %>%
    mutate(Revenue = Quantity * Price)
  
  
  
  ----------#Step2: visualization
    # 📌 Goal: Create an interactive sales trend (total revenue over time).
    # Step 1: Prepare the data
    # Assuming retail_data has columns: InvoiceDate, Revenue, and Country
    # Convert InvoiceDate to Date type if it's not already
    retail_data <- retail_data %>%
    mutate(InvoiceDate = as.Date(InvoiceDate))
  
  # Aggregate total revenue by InvoiceDate and Country
  sales_trend <- retail_data %>%
    group_by(InvoiceDate, Country) %>%
    summarise(TotalRevenue = sum(Revenue, na.rm = TRUE), .groups = 'drop')
  
  # Step 2: Create the interactive line chart
  fig <- sales_trend %>%
    plot_ly(
      x = ~InvoiceDate,  # X-axis: InvoiceDate
      y = ~TotalRevenue,  # Y-axis: TotalRevenue
      color = ~Country,  # Color lines by Country
      type = 'scatter',
      mode = 'lines+markers',  # Lines with markers
      hoverinfo = 'text',  # Customize tooltips
      text = ~paste(
        "Date: ", InvoiceDate, "<br>",
        "Country: ", Country, "<br>",
        "Total Revenue: $", round(TotalRevenue, 2)
      )
    )
  
  # Step 3: Add layout and interactivity
  fig <- fig %>%
    layout(
      title = "Sales Trends: Total Revenue Over Time",
      xaxis = list(
        title = "Invoice Date",
        rangeselector = list(
          buttons = list(
            list(
              count = 7,
              label = "1 Week",
              step = "day",
              stepmode = "backward"),
            list(
              count = 1,
              label = "1 Month",
              step = "month",
              stepmode = "backward"),
            list(
              count = 6,
              label = "6 Months",
              step = "month",
              stepmode = "backward"),
            list(
              count = 1,
              label = "1 Year",
              step = "year",
              stepmode = "backward"),
            list(
              label = "All",
              step = "all")
          )
        ),
        rangeslider = list(visible = TRUE)  # Add a range slider for zooming
      ),
      yaxis = list(
        title = "Total Revenue",
        tickformat = "$,.0f"  # Format revenue as currency
      ),
      hovermode = "x unified",  # Show tooltips for all lines at the same x-value
      showlegend = TRUE  # Show the legend for Country filtering
    )
  
  # Step 4: Display the plot
  fig
  
  #Total Revenue By country 
  # Step 1: Prepare the data
  # Aggregate total revenue by country
  revenue_by_country <- retail_data %>%
    group_by(Country) %>%
    summarise(TotalRevenue = sum(Revenue, na.rm = TRUE), .groups = 'drop')
  
  # Step 2: Create the choropleth map
  hc <- hcmap(
    "custom/world-robinson-lowres",  # Use a low-resolution world map
    data = revenue_by_country,  # Data for the map
    name = "Total Re
    
    
    
  

