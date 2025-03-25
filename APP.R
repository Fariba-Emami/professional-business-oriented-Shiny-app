library(shiny)
library(shinymanager)
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
library(tibble)

# Create credentials data frame with roles and timestamps
credentials <- data.frame(
  user = c(
    Sys.getenv("ADMIN_USER", "admin"),
    Sys.getenv("USER_USER", "user"),
    Sys.getenv("VIEWER_USER", "viewer")
  ),
  password = c(
    Sys.getenv("ADMIN_PASS", "admin123"),
    Sys.getenv("USER_PASS", "user123"),
    Sys.getenv("VIEWER_PASS", "viewer123")
  ),
  # Define user roles with different access levels
  role = c("admin", "user", "viewer"),
  stringsAsFactors = FALSE
)

# Error handling for data import
tryCatch({
  data_file <- "data/online_retail.csv"
  retail_data <- import(data_file) %>%
    rename(CustomerID = `Customer ID`) %>%
    filter(!is.na(CustomerID)) %>%
    mutate(
      Revenue = Quantity * Price,
      InvoiceDate = parse_date_time(as.character(InvoiceDate), orders = c("ymd HMS", "mdy HMS", "dmy HMS"))
    )
  
  if (nrow(retail_data) == 0) stop("No data was loaded. Check the file path and format.")
}, error = function(e) {
  stop(paste("Error importing data:", e$message))
})

# UI Definition
ui <- secure_app(
  # UI content 
  fluidPage(
    theme = bs_theme(bootswatch = "flatly"),
    title = "Interactive Retail Dashboard",
    sidebarLayout(
      sidebarPanel(
        selectInput("country_select", "Select Country:",
                    choices = c("All", sort(unique(retail_data$Country)))),
        dateRangeInput("date_range", "Select Date Range:",
                       start = min(as.Date(retail_data$InvoiceDate), na.rm = TRUE),
                       end = max(as.Date(retail_data$InvoiceDate), na.rm = TRUE))
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Sales Trends", plotlyOutput("sales_trend_plot")),
          tabPanel("Revenue Distribution", plotlyOutput("revenue_distribution_plot"))
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  # Authentication
  res_auth <- secure_server(
    check_credentials = function(username, password) {
      # Custom authentication logic
      user_match <- credentials %>%
        filter(user == username & password == password)
      
      if (nrow(user_match) > 0) {
        return(list(
          result = TRUE, 
          user_info = list(
            role = user_match$role
          )
        ))
      } else {
        return(list(result = FALSE))
      }
    }
  )
  
  # Observe authentication status
  observe({
    #  user role from authentication
    user_role <- res_auth$role
    
    # the user role for conditional rendering or access control
    print(paste("Logged in user role:", user_role))
  })
  
  # Your existing server logic for data visualization
  output$sales_trend_plot <- renderPlotly({
    # Example plot (similar to previous implementation)
    sales_trend <- retail_data %>%
      group_by(InvoiceDate = as.Date(InvoiceDate), Country) %>%
      summarise(TotalRevenue = sum(Quantity * Price, na.rm = TRUE), .groups = 'drop')
    
    plot_ly(sales_trend, x = ~InvoiceDate, y = ~TotalRevenue, color = ~Country,
            type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Sales Trends Over Time")
  })
  
  output$revenue_distribution_plot <- renderPlotly({
    revenue_dist <- retail_data %>%
      group_by(Country) %>%
      summarise(TotalRevenue = sum(Quantity * Price, na.rm = TRUE))
    
    plot_ly(revenue_dist, x = ~Country, y = ~TotalRevenue, type = 'bar') %>%
      layout(title = "Revenue Distribution by Country")
  })
}

# Run the application
shinyApp(ui, server)