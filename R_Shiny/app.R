# Function to check and install required packages
check_and_install_packages <- function(packages) {
  missing_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(missing_packages) > 0) {
    message("Installing missing packages: ", paste(missing_packages, collapse=", "))
    install.packages(missing_packages, dependencies=TRUE)
  }
}

# List of required packages
required_packages <- c("shiny", "shinydashboard", "shinyjs", "readxl", "DT", "plotly", 
                      "ROSE", "caret", "smotefamily", "reactable", "shinyjqui", 
                      "shinycssloaders", "bslib")

# Check and install missing packages
check_and_install_packages(required_packages)

# Load required packages
library(shiny)
library(shinydashboard)
library(shinyjs)
library(readxl)  
library(DT)
library(plotly)
library(ROSE)
library(caret)
library(smotefamily)
library(reactable)
library(shinyjqui)
library(shinycssloaders)
library(bslib)

# Static credentials
credentials <- list(user = "HIS", password = "1234")

# Create www directory if it doesn't exist
if (!dir.exists("www")) {
  dir.create("www")
}

# Enhanced Login UI
login_ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f4f6f9;
        font-family: 'Segoe UI', sans-serif;
      }
      .login-box {
        width: 360px;
        margin: 100px auto;
        padding: 30px;
        background-color: white;
        box-shadow: 0 0 10px rgba(0,0,0,0.1);
        border-radius: 10px;
      }
      .login-title {
        text-align: center;
        font-size: 24px;
        font-weight: bold;
        color: #003366;
        margin-bottom: 20px;
      }
      #login_error {
        margin-top: 10px;
        font-size: 14px;
        color: red;
      }
      .login-logo {
        text-align: center;
        margin-bottom: 20px;
      }
      .login-logo img {
        max-width: 150px;
        height: auto;
      }
      
    ")),
    tags$script('
      $(document).on("keyup", function(e) {
        if(e.keyCode == 13) {
          if($("#user").is(":focus") || $("#password").is(":focus")) {
            $("#login_btn").click();
          }
        }
      });
    ')
  ),
  div(class = "login-box",
      div(class = "login-logo", 
      tags$img(src = "logo.png", alt = "Logo")
      ),
      div(class = "login-title", "🔐 Secure Login"),
      textInput("user", "Username", placeholder = "Enter username"),
      passwordInput("password", "Password", placeholder = "Enter password"),
      actionButton("login_btn", "Log in", class = "btn btn-primary btn-block"),
      div(id = "login_error", "❌ Invalid username or password.", style = "display: none;")
  )
)

# Add a function to check if source files exist
check_source_files <- function() {
  ui_file <- "R/ui.R"
  server_file <- "R/server.R"
  
  if (!file.exists(ui_file)) {
    stop("UI file not found: ", ui_file)
  }
  
  if (!file.exists(server_file)) {
    stop("Server file not found: ", server_file)
  }
  
  return(TRUE)
}

# Check if source files exist before loading them
if(check_source_files()) {
  # Load actual UI & server from source files
  source("R/ui.R", local = TRUE)
  source("R/server.R", local = TRUE)
}

# Full app logic
ui_combined <- function() {
  uiOutput("app_ui")
}

server_combined <- function(input, output, session) {
  user_authenticated <- reactiveVal(FALSE)
  
  # Login handler
  observeEvent(input$login_btn, {
    if (input$user == credentials$user && input$password == credentials$password) {
      shinyjs::hide("login_error")
      user_authenticated(TRUE)
    } else {
      shinyjs::show("login_error")
    }
  })
  
  # Logout handler
  observeEvent(input$logout_btn, {
    user_authenticated(FALSE)
    session$reload()  # This will refresh the app, clearing any state
  })
  
  output$app_ui <- renderUI({
    if (user_authenticated()) {
      ui  # from ui.R
    } else {
      login_ui
    }
  })
  
  observe({
    if (user_authenticated()) {
      server(input, output, session)  # call your original server logic
    }
  })
}

# Run the app
shinyApp(ui_combined, server_combined)