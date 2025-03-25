
# Libraries
if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  shiny, rio, dplyr, ggplot2, shinythemes, bslib, 
  shinyWidgets, shinyjs, leaflet, DT, plotly, 
  waiter, shinycssloaders, rnaturalearthdata, 
  rnaturalearth, lubridate
)

# -----------------------------------------------------------------------------
# DATA PREPARATION
# -----------------------------------------------------------------------------
data_file <- "data/online_retail.csv"
retail_data <- import(data_file) %>%
  rename(CustomerID = `Customer ID`) %>%
  filter(!is.na(CustomerID)) %>%
  mutate(
    Revenue = Quantity * Price,
    InvoiceDate = parse_date_time(as.character(InvoiceDate), orders = c("ymd HMS", "mdy HMS", "dmy HMS"))
  )

if (nrow(retail_data) == 0) stop("No data was loaded. Check the file path and format.")

# -----------------------------------------------------------------------------
# UI DEFINITION
# -----------------------------------------------------------------------------

ui <- page_fluid(
  # Dark mode theme with custom colors
  theme = bs_theme(
    version = 5,
    bootswatch = "darkly",  # Dark base theme
    primary = "#6f42c1",    # Custom primary color
    secondary = "#007bff",  # Custom secondary color
    
    # Custom CSS for fine-tuning
    "body-bg" = "#121212",     # Deep dark background
    "body-color" = "#e0e0e0",  # Light text for readability
    
    # Customize other theme variables
    "font-size-base" = "1rem",
    "card-bg" = "#1e1e1e",     # Slightly lighter card background
    "border-color" = "#333"    # Dark border color
  ),
  
  title = "Interactive Retail Dashboard",
  
  # Waiter for loading
  use_waiter(),
  waiter_preloader(html = tagList(
    spin_loaders(16, color = "#6f42c1"),  # Purple spinner
    div(style = "color: white; margin-top: 10px;", "Loading Dashboard...")
  )),
  
  # Additional custom CSS
  tags$head(
    tags$style(HTML("
      /* Enhanced card styling */
      .card {
        border-radius: 12px;
        box-shadow: 0 6px 12px rgba(0,0,0,0.3);
        transition: all 0.3s ease;
        margin-bottom: 20px;
      }
      
      .card:hover {
        transform: translateY(-5px);
        box-shadow: 0 10px 20px rgba(0,0,0,0.4);
      }
      
      .card-header {
        background-color: rgba(111, 66, 193, 0.2);
        color: #6f42c1;
        border-bottom: 2px solid #6f42c1;
        font-weight: bold;
      }
      
      /* Sidebar enhancements */
      .sidebar {
        background-color: #1e1e1e;
        border-right: 2px solid #333;
        padding: 15px;
        border-radius: 10px;
      }
      
      /* Tab styling */
      .nav-tabs {
        border-bottom: 2px solid #6f42c1;
      }
      
      .nav-tabs .nav-link {
        color: #e0e0e0;
        background-color: #2c2c2c;
        margin-right: 5px;
        border: 1px solid #333;
        transition: all 0.3s ease;
      }
      
      .nav-tabs .nav-link.active {
        color: white;
        background-color: #6f42c1;
        border-color: #6f42c1;
      }
      
      .nav-tabs .nav-link:hover {
        background-color: #5a32a3;
        color: white;
      }
    "))
  ),
  
  layout_sidebar(
    sidebar = sidebar(
      width = "250px",
      title = div(
        icon("chart-line"), 
        strong("Dashboard Filters"), 
        style = "color: #6f42c1;"
      ),
      class = "sidebar",
      
      # Country selection with icon
      selectInput("country_select", 
                  div(icon("globe"), "Select Country:"),
                  choices = c("All", sort(unique(retail_data$Country))),
                  selected = "All"),
      
      # Date range with icon
      dateRangeInput("date_range", 
                     div(icon("calendar"), "Select Date Range:"),
                     start = min(retail_data$InvoiceDate, na.rm = TRUE),
                     end = max(retail_data$InvoiceDate, na.rm = TRUE)),
      
      # Divider
      tags$hr(style = "border-top: 2px solid #6f42c1;"),
      
      # Dataset and download options
      selectInput("dataset", 
                  div(icon("database"), "Select Data Set:"), 
                  choices = "retail_data"),
      selectInput("file_format", 
                  div(icon("file-download"), "Select File Format:"),
                  choices = c("CSV" = "csv", "Excel" = "xlsx")),
      
      # Download button with custom styling
      downloadButton("download_btn", "Download Dataset", 
                     class = "btn-outline-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs",
        type = "pills",  # Use pill-style tabs
        
        # Sales Trends Tab
        tabPanel(
          title = div(icon("chart-line"), "Sales Trends"),
          card(
            card_header("Sales Trends Over Time"), 
            card_body(plotlyOutput("sales_trend_plot") %>% withSpinner())
          )
        ),
        
        # Revenue Distribution Tab
        tabPanel(
          title = div(icon("chart-pie"), "Revenue Distribution"),
          card(
            card_header("Revenue by Country"), 
            card_body(plotlyOutput("revenue_distribution_plot") %>% withSpinner())
          )
        ),
        
        # Top Products Tab
        tabPanel(
          title = div(icon("tags"), "Top Products"),
          card(
            card_header("Top-Selling Products"), 
            card_body(plotlyOutput("top_products_plot") %>% withSpinner())
          )
        ),
        
        # Sales by Country Tab
        tabPanel(
          title = div(icon("map-marked-alt"), "Global Sales"),
          card(
            card_header("Sales Geographical Distribution"), 
            card_body(leafletOutput("sales_leaflet_map", height = "400px") %>% withSpinner())
          )
        ),
        
        # Transaction Table Tab
        tabPanel(
          title = div(icon("table"), "Transactions"),
          card(
            card_header("Interactive Transaction Details"), 
            card_body(DTOutput("transaction_table") %>% withSpinner())
          )
        )
      )
    )
  )
)

# Server logic remains the same as in the previous implementation

# Run the application
shinyApp(ui, server)