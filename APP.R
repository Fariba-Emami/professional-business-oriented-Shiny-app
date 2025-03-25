library(shiny)
library(shinymanager)  # Authentication package
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
# AUTHENTICATION CREDENTIALS
# -----------------------------------------------------------------------------

# Use environment variables for storing credentials
# Before running, set these in .Renviron or using Sys.setenv()
# usethis::edit_r_environ() can help set these

# Create credentials data frame with roles
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

# -----------------------------------------------------------------------------
# DATA PREPARATION
# -----------------------------------------------------------------------------

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

# -----------------------------------------------------------------------------
# UI DEFINITION
# -----------------------------------------------------------------------------

# Custom authentication UI
auth_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$div(
      class = "login-container",
      tags$h2("Retail Dashboard Login"),
      tags$style(HTML("
        .login-container {
          width: 300px;
          margin: 100px auto;
          padding: 20px;
          background-color: #f4f4f4;
          border-radius: 8px;
          box-shadow: 0 4px 6px rgba(0,0,0,0.1);
        }
        .login-header {
          text-align: center;
          margin-bottom: 20px;
          color: #333;
        }
      ")),
      # Additional custom login page styling can be added here
      shinymanager::login_ui(ns("auth"))
    )
  )
}

# Main application UI
main_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
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
      "))
    ),
    
    # Add a logout button
    actionButton(ns("logout"), "Logout", class = "btn-danger"),
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput("country_select", "Select Country:",
                    choices = c("All", sort(unique(retail_data$Country)))),
        dateRangeInput("date_range", "Select Date Range:",
                       start = min(as.Date(retail_data$InvoiceDate), na.rm = TRUE),
                       end = max(as.Date(retail_data$InvoiceDate), na.rm = TRUE)),
        
        # Conditional panels based on user role
        conditionalPanel(
          condition = "input.current_user_role == 'admin'",
          selectInput("dataset", "Select data set:", choices = "retail_data"),
          selectInput("file_format", "Select File Format:",
                      choices = c("CSV" = "csv", "EXCEL" = "xlsx")),
          downloadButton("download_btn", "Download Dataset")
        )
      ),
      
      mainPanel(
        # Store current user role as a hidden input
        tags$input(
          type = "hidden", 
          id = ns("current_user_role"),
          value = ""
        ),
        tabsetPanel(
          id = "tabs",
          tabPanel("Sales Trends", plotlyOutput(ns("sales_trend_plot")) %>% withSpinner()),
          tabPanel("Revenue Distribution", plotlyOutput(ns("revenue_distribution_plot")) %>% withSpinner()),
          tabPanel("Top Products", plotlyOutput(ns("top_products_plot")) %>% withSpinner()),
          # Conditional panels can be added here to restrict access
          conditionalPanel(
            condition = "input.current_user_role != 'viewer'",
            tabPanel("Sales by Country", leafletOutput(ns("sales_leaflet_map"), height = "400px") %>% withSpinner())
          ),
          tabPanel("Transaction Table", DTOutput(ns("transaction_table")) %>% withSpinner())
        )
      )
    )
  )
}

# Wrap UI with secure_app
ui <- secure_app(
  # Use a custom login UI
  head_auth = tags$style(type = "text/css", "body { background-color: #f0f0f0; }"),
  ui = tagList(
    auth_ui("login"),
    main_ui("main")
  )
)

# -----------------------------------------------------------------------------
# SERVER LOGIC
# -----------------------------------------------------------------------------

server <- function(input, output, session) {
  # Authentication
  credentials_reactive <- reactive({
    # Allow overriding credentials with environment variables
    data.frame(
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
      role = c("admin", "user", "viewer"),
      stringsAsFactors = FALSE
    )
  })
  
  # Authenticate user
  auth <- callModule(
    module = shinymanager::login,
    id = "auth",
    data = credentials_reactive,
    # Optional: add additional authentication checks
    valid_pass = shinymanager::check_credentials(credentials_reactive())
  )
  
  # Observe authentication status
  observe({
    req(auth())
    # Update hidden input with user role
    updateTextInput(
      session, 
      "main-current_user_role", 
      value = auth()$role
    )
  })
  
  # Logout functionality
  observeEvent(input$logout, {
    shinymanager::logout_server()
  })
  
  # Existing dashboard logic (similar to previous implementation)
  
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
  
  # Sales Trends Plot (similar to previous implementation)
  output$sales_trend_plot <- renderPlotly({
    req(filtered_data())
    sales_trend <- filtered_data() %>%
      group_by(InvoiceDate = as.Date(InvoiceDate), Country) %>%
      summarise(TotalRevenue = sum(Revenue, na.rm = TRUE), .groups = 'drop')
    
    plot_ly(sales_trend, x = ~InvoiceDate, y = ~TotalRevenue, color = ~Country,
            type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Sales Trends Over Time",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Revenue"))
  })
  
  # Similar rendering for other outputs...
  
  # Conditional Download Handler (only for admin)
  output$download_btn <- downloadHandler(
    filename = function() {
      paste("retail_data", input$file_format, sep = ".")
    },
    content = function(file) {
      # Additional role-based check
      req(auth()$role == "admin")
      
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