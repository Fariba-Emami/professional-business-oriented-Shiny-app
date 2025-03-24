library(shiny)
library(rio)
library(dplyr)
library(ggplot2)
library(shinythemes)
library(bslib)
library(shinyWidgets)
library(shinyjs)
library(leaflet)
library(DT)
library(plotly)
library(waiter)
library(shinycssloaders)
library(rnaturalearthdata)
library(rnaturalearth)
library(lubridate)

# -----------------------------------------------------------------------------
# DATA PREPARATION
# -----------------------------------------------------------------------------

data_file <- "data/online_retail.csv"
retail_data <- import(data_file) %>%
  rename(CustomerID = `Customer ID`) %>%
  filter(!is.na(CustomerID)) %>%
  mutate(
    Revenue = Quantity * Price,
    InvoiceDate = as.Date(InvoiceDate)
  )

if (nrow(retail_data) == 0) stop("No data was loaded. Check the file path and format.")

# -----------------------------------------------------------------------------
# UI DEFINITION
# -----------------------------------------------------------------------------

ui <- page_fluid(
  theme = bs_theme(bootswatch = "flatly"),
  title = "Interactive Retail Dashboard",
  use_waiter(),
  waiter_preloader(html = tagList(spin_loaders(16, color = "#3c8dbc"))),
  
  tags$head(
    tags$style(HTML("
      .card {
        border-radius: 10px;
        box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);
        transition: 0.3s;
      }

      .card:hover {
        box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2);
      }

      .card-header {
        background-color: #f0f0f0;
        font-weight: bold;
        border-bottom: 1px solid #ddd;
        padding: 10px;
      }

      .sidebar {
        background-color: #f8f9fa;
        border-right: 1px solid #eee;
      }
    "))
  ),
  
  layout_sidebar(
    sidebar = sidebar(
      width = "250px",
      title = "Filters",
      class = "sidebar",
      selectInput("country_select", "Select Country:",
                  choices = c("All", sort(unique(retail_data$Country)))),
      dateRangeInput("date_range", "Select Date Range:",
                     start = min(retail_data$InvoiceDate, na.rm = TRUE),
                     end = max(retail_data$InvoiceDate, na.rm = TRUE)),
      
      selectInput("dataset", "Select data set:", choices = "retail_data"),
      selectInput("file_format", "Select File Format:",
                  choices = c("CSV" = "csv", "EXCEL" = "xlsx")),
      downloadButton("download_btn", "Download Dataset")
    ),
    
    mainPanel(
      tabsetPanel(  # Use tabsetPanel
        id = "tabs",
        tabPanel("Sales Trends", card(card_header("Sales Trends Over Time"), card_body(plotlyOutput("sales_trend_plot") %>% withSpinner()))),
        tabPanel("Revenue Distribution", card(card_header("Revenue Distribution"), card_body(plotlyOutput("revenue_distribution_plot") %>% withSpinner()))),
        tabPanel("Top Products", card(card_header("Top-Selling Products"), card_body(plotlyOutput("top_products_plot") %>% withSpinner()))),
        tabPanel("Sales by Country", card(card_header("Sales by Country"), card_body(leafletOutput("sales_leaflet_map", height = "400px") %>% withSpinner()))),
        tabPanel("Transaction Table", card(card_header("Interactive Transaction Table"), card_body(DTOutput("transaction_table") %>% withSpinner())))
      )
    )
  )
)

# -----------------------------------------------------------------------------
# SERVER LOGIC
# -----------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # --- Reactive Data Filtering ---
  filtered_data <- reactive({
    req(retail_data)
    data <- retail_data %>%
      filter(InvoiceDate >= input$date_range[1] & InvoiceDate <= input$date_range[2])
    
    if (input$country_select != "All") {
      data <- data %>% filter(Country == input$country_select)
    }
    return(data)
  })
  
  # --- Sales Trends Plot ---
  output$sales_trend_plot <- renderPlotly({
    req(filtered_data())
    sales_trend <- filtered_data() %>%
      group_by(InvoiceDate, Country) %>%
      summarise(TotalRevenue = sum(Revenue, na.rm = TRUE), .groups = 'drop')
    
    plot_ly(sales_trend, x = ~InvoiceDate, y = ~TotalRevenue, color = ~Country,
            type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Sales Trends Over Time",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Revenue"))
  })
  
  # --- Revenue Distribution Plot ---
  output$revenue_distribution_plot <- renderPlotly({
    req(filtered_data())
    revenue_data <- filtered_data() %>%
      group_by(Country) %>%
      summarise(TotalRevenue = sum(Revenue, na.rm = TRUE), .groups = 'drop')
    
    plot_ly(revenue_data, x = ~Country, y = ~TotalRevenue, type = 'bar') %>%
      layout(title = "Revenue by Country",
             xaxis = list(title = "Country"),
             yaxis = list(title = "Revenue"))
  })
  
  # --- Top Products Plot ---
  output$top_products_plot <- renderPlotly({
    req(filtered_data())
    top_products <- filtered_data() %>%
      group_by(Description) %>%
      summarise(TotalQuantity = sum(Quantity, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(TotalQuantity)) %>%
      head(10)
    
    plot_ly(top_products, x = ~Description, y = ~TotalQuantity, type = 'bar') %>%
      layout(title = "Top Selling Products",
             xaxis = list(title = "Product"),
             yaxis = list(title = "Quantity Sold"))
  })
  
  # --- Leaflet Map ---
  output$sales_leaflet_map <- renderLeaflet({
    req(filtered_data())
    sales_by_country <- filtered_data() %>%
      group_by(Country) %>%
      summarise(TotalRevenue = sum(Revenue, na.rm = TRUE), .groups = 'drop')
    
    world_data <- ne_countries(scale = "medium", returnclass = "sf") %>%
      left_join(sales_by_country, by = c("name_long" = "Country"))
    
    leaflet(world_data) %>%
      addTiles() %>%
      addPolygons(fillColor = ~colorNumeric("viridis", TotalRevenue)(TotalRevenue),
                  weight = 1, fillOpacity = 0.7,
                  label = ~paste0(name_long, ": $", round(TotalRevenue, 2)))
  })
  
  # --- Data Table ---
  output$transaction_table <- renderDT({
    req(filtered_data())
    datatable(filtered_data(), options = list(pageLength = 10))
  })
  
  # --- Download Handler ---
  output$download_btn <- downloadHandler(
    filename = function() {
      paste("retail_data", input$file_format, sep = ".")
    },
    content = function(file) {
      if (input$file_format == "csv") {
        write.csv(filtered_data(), file, row.names = FALSE)
      } else {
        rio::export(filtered_data(), file)
      }
    }
  )
}

# -----------------------------------------------------------------------------
# RUN THE APPLICATION
# -----------------------------------------------------------------------------

shinyApp(ui, server)