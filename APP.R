library(shiny)
library(rio)        # For easy data import
library(dplyr)
library(ggplot2)
library(shinythemes)
library(tidyverse)
library(bslib)       # Enhanced UI
library(shinyWidgets) # Enhanced input widgets
library(shinyjs)      # Show/hide UI elements
library(leaflet)   # For creating interactive maps
library(DT)        # For interactive tables
library(plotly)    # For interactive plots
library(highcharter) # For interactive charts
library(waiter)    # For loading screens
library(shinycssloaders) # For spinner animations
library(rnaturalearthdata) #For maps
library(rnaturalearth)
library(lubridate)

# **1. Data Preparation**

# Store the file path
data_file <- "data/online_retail.csv"

  # Read the data from the CSV file
  retail_data <- import("data/online_retail.csv")
  
  # Rename the 'Customer ID' column to 'CustomerID' for easier use
  retail_data <- retail_data %>%
    rename(CustomerID = `Customer ID`)
  
  # Remove rows with missing CustomerID, calculate Revenue, and ensure InvoiceDate is a date
  retail_data <- retail_data %>%
    filter(!is.na(CustomerID)) %>%
    mutate(Revenue = Quantity * Price) %>%
    mutate(InvoiceDate = as.Date(InvoiceDate))

  
  
  
# Make sure the data is not null
if (nrow(retail_data) == 0) {
  stop("No data was loaded. Check the file path and format.")
}

# --- 3. User Interface (UI) Definition ---
# This section describes the layout and appearance of your web app.
ui <- page_fluid(
  theme = bs_theme(bootswatch = "flatly"),  # Use a dark theme for the app
  title = "Interactive Retail Dashboard",  # Set the title of the app
  use_waiter(), # Enable loading screens
  waiter_preloader(html = tagList(spin_loaders(16, color = "#3c8dbc"))), # Initial loading screen
  
  tags$head(
    tags$style(HTML("
      .value-box-title {
        font-size: 16px;
        font-weight: bold;
        color: #333;
      }
    "))
  ),
  
  layout_sidebar(   # Overall layout with a sidebar
    sidebar = sidebar(  # Define the sidebar content
      width = "250px",   # Set the sidebar width
      title = "Filters",  # Add a title to the sidebar
      selectInput("country_select", "Select Country:",   # Dropdown for country selection
                  choices = c("All", sort(unique(retail_data$Country)))),
      dateRangeInput("date_range", "Select Date Range:",  # Date range picker
                     start = min(retail_data$InvoiceDate, na.rm = TRUE),
                     end = max(retail_data$InvoiceDate, na.rm = TRUE))
    ),
    
    layout_columns(   # Arrange content in columns
      col_widths = c(6, 6),  # Two columns, each taking up half the space
      card(  # A card is a container with a header and body
        card_header("Sales Trends Over Time"),
        card_body(plotlyOutput("sales_trend_plot") %>% withSpinner()) # Plot with loading spinner
      ),
      card(
        card_header("Total Revenue by Country (Map)"),
        card_body(highchartOutput("revenue_map") %>% withSpinner())  # Map with loading spinner
      )
    ),
    
    layout_columns(  # Another row of columns
      col_widths = c(6, 6),
      card(
        card_header("Top-Selling Products"),
        card_body(highchartOutput("top_products_plot") %>% withSpinner()) # Chart with loading spinner
      ),
      # **New: Column Chart for Revenue Distribution**
      card(
        card_header("Revenue Distribution by Country (Column Chart)"),
        card_body(highchartOutput("revenue_by_country_column_plot") %>% withSpinner())
      )
    ),
    
    layout_columns(   # A full-width column for the Leaflet map
      col_widths = c(12),
      card(
        card_header("Sales by Country (Leaflet Map)"),
        card_body(leafletOutput("sales_leaflet_map", height = "600px") %>% withSpinner()) # Map with loading spinner
      )
    ),
    
    layout_columns(  # A full-width column for the data table
      col_widths = c(12),
      card(
        card_header("Interactive Transaction Table (DT)"),
        card_body(DTOutput("transaction_table") %>% withSpinner()) # Table with loading spinner
      )
    )
  )
)

# --- 4. Server Logic Definition ---
# This section defines how your app responds to user input and generates outputs.
server <- function(input, output, session) {
  
  # Filter data based on user selections
  filtered_data <- reactive({
    req(retail_data) #Make sure you have the data
    data <- retail_data %>%
      filter(InvoiceDate >= input$date_range[1] & InvoiceDate <= input$date_range[2])
    
    if (input$country_select != "All") {
      data <- data %>%
        filter(Country == input$country_select)
    }
    return(data)
  })
  
  # --- Output: Sales Trends Over Time (Plotly) ---
  output$sales_trend_plot <- renderPlotly({
    req(filtered_data())
    sales_trend <- filtered_data() %>%
      group_by(InvoiceDate, Country) %>%
      summarise(TotalRevenue = sum(Revenue, na.rm = TRUE), .groups = 'drop')
    
    plot_ly(sales_trend, x = ~InvoiceDate, y = ~TotalRevenue, color = ~Country,
            type = 'scatter', mode = 'lines+markers',
            text = ~paste("Date: ", InvoiceDate, "<br>Country: ", Country,
                          "<br>Total Revenue: $", round(TotalRevenue, 2)),
            hoverinfo = 'text') %>%
      layout(title = "Sales Trends: Total Revenue Over Time",
             xaxis = list(title = "Invoice Date", rangeslider = list(visible = TRUE)),
             yaxis = list(title = "Total Revenue", tickformat = "$,.0f"),
             hovermode = "x unified", showlegend = TRUE)
  })
  
  # --- Output: Total Revenue by Country (Highcharter Map) ---
  output$revenue_map <- renderHighchart({
    req(filtered_data())
    revenue_by_country <- filtered_data() %>%
      group_by(Country) %>%
      summarise(TotalRevenue = sum(Revenue, na.rm = TRUE), .groups = 'drop')
    
    # Convert country names to uppercase to match the map data
    revenue_by_country <- revenue_by_country %>%
      mutate(Country = toupper(Country))
    
    hc <- highchart() %>%
      hcmap(
        "custom/world-robinson-lowres",
        data = revenue_by_country,
        joinBy = c("name", "Country"),
        value = "TotalRevenue",
        name = "Total Revenue",
        dataLabels = list(enabled = FALSE) # Disable data labels for cleaner look
      ) %>%
      hc_colorAxis(
        stops = color_stops(colors = viridisLite::inferno(10, begin = 0.1)),
        type = "logarithmic"
      ) %>%
      hc_title(text = "Total Revenue by Country") %>%
      hc_tooltip(pointFormat = "<b>{point.name}:</b> ${point.value:,.0f}")
  })
  
  # --- Output: Top-Selling Products (Highcharter) ---
  output$top_products_plot <- renderHighchart({
    req(filtered_data())
    top_products <- filtered_data() %>%
      group_by(Description, Country) %>%
      summarise(TotalQuantity = sum(Quantity, na.rm = TRUE), .groups = 'drop')
    
    hchart(top_products, "bar", hcaes(x = Description, y = TotalQuantity, group = Country)) %>%
      hc_title(text = "Top-Selling Products by Quantity Sold") %>%
      hc_xAxis(title = list(text = "Product")) %>%
      hc_yAxis(title = list(text = "Total Quantity Sold")) %>%
      hc_tooltip(shared = TRUE, pointFormat = "<b>{series.name}:</b> {point.y}") %>%
      hc_legend(enabled = TRUE)
  })
  
  # --- Output: Revenue Distribution by Country (Column Chart) ---
  output$revenue_by_country_column_plot <- renderHighchart({
    req(filtered_data())
    revenue_by_country <- filtered_data() %>%
      group_by(Country) %>%
      summarise(TotalRevenue = sum(Revenue, na.rm = TRUE), .groups = 'drop')
    
    hchart(revenue_by_country, "column", hcaes(x = Country, y = TotalRevenue)) %>%
      hc_title(text = "Total Revenue by Country (Column Chart)") %>%
      hc_xAxis(title = list(text = "Country")) %>%
      hc_yAxis(title = list(text = "Total Revenue")) %>%
      hc_tooltip(pointFormat = "<b>{point.Country}:</b> ${point.TotalRevenue:,.0f}")
  })
  
  # --- Output: Sales by Country (Leaflet Map) ---
  output$sales_leaflet_map <- renderLeaflet({
    req(filtered_data())
    sales_by_country <- filtered_data() %>%
      group_by(Country) %>%
      summarise(TotalRevenue = sum(Revenue, na.rm = TRUE), .groups = 'drop')
    
    world_data <- ne_countries(scale = "medium", returnclass = "sf") %>%
      left_join(sales_by_country, by = c("name_long" = "Country"))
    
    leaflet(world_data) %>%
      addTiles() %>%
      addPolygons(fillColor = ~colorNumeric(palette = "viridis", domain = TotalRevenue)(TotalRevenue),
                  weight = 1, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7,
                  highlightOptions = highlightOptions(weight = 5, color = "#666", dashArray = "",
                                                      fillOpacity = 0.7, bringToFront = TRUE),
                  label = ~paste0(name_long, ": ", "$", round(TotalRevenue, 2)),
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "13px", direction = "auto")) %>%
      addLegend(pal = colorNumeric(palette = "viridis", domain = world_data$TotalRevenue),
                values = ~TotalRevenue, opacity = 0.7, title = "Total Revenue", position = "bottomright")
  })
  
  # --- Output: Interactive Transaction Table (DT) ---
  output$transaction_table <- renderDT({
    req(filtered_data())
    datatable(filtered_data(), extensions = 'Buttons',
              options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                             pageLength = 10))
  })
}

# --- 5. Run the App ---
# This line starts the Shiny app.
shinyApp(ui, server)