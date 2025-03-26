# Load required libraries
library(shiny)
library(shinymanager)

# Credentials data
credentials <- data.frame(
  user = c("shiny", "shinymanager"),
  password = c("azerty", "12345"),
  # Password will automatically be hashed
  admin = c(FALSE, TRUE),
  stringsAsFactors = FALSE
)

# Correct file path (use forward slashes or double backslashes)
sqlite_path <- "E:/R_Track at DLH/Modual_3/professional-business-oriented-Shiny-app/secure_db.sqlite"

# Create the database
create_db(
  credentials_data = credentials,
  sqlite_path = sqlite_path,
  passphrase = "passphrase"
)

# UI with secure authentication
ui <- fluidPage(
  tags$h2("My Secure Application"),
  verbatimTextOutput("auth_output")
)

# Wrap UI with secure_app
ui <- secure_app(ui, enable_admin = TRUE)

# Server function
server <- function(input, output, session) {
  # Check credentials directly on SQLite database
  res_auth <- secure_server(
    check_credentials = check_credentials(
      sqlite_path,
      passphrase = "passphrase"
    )
  )
  
  # Render authentication output
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
}

# Run the Shiny app
shinyApp(ui, server)