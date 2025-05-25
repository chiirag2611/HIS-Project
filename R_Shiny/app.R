# Function to check and install required packages
check_and_install_packages <- function(packages) {
  missing_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(missing_packages) > 0) {
    message("Installing missing packages: ", paste(missing_packages, collapse=", "))
    tryCatch({
      install.packages(missing_packages, dependencies=TRUE)
    }, error = function(e) {
      message("Error installing packages: ", e$message)
      message("Please manually install these packages: ", paste(missing_packages, collapse=", "))
    })
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

# Ensure all required directories exist
required_dirs <- c("www", "R")
for (dir in required_dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir)
    cat(sprintf("Created directory: %s\n", dir))
  }
}

# Check for logo file and copy if missing
if (!file.exists("www/logo.png")) {
  warning("Logo file not found at www/logo.png - login page will show broken image")
}

# Enhanced Login UI with fixes
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
      .shiny-notification {
        position: fixed;
        top: 20px;
        right: 20px;
        width: 300px;
      }
    ")),
    # Simplified and more reliable enter key handler
    tags$script('
      $(document).on("keydown", function(e) {
        if(e.which == 13) {
          Shiny.setInputValue("enter_pressed", Math.random(), {priority: "event"});
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
  app_dir <- getwd()
  ui_file <- file.path(app_dir, "R/ui.R")
  server_file <- file.path(app_dir, "R/server.R")
  
  missing_files <- character(0)
  
  if (!file.exists(ui_file)) {
    missing_files <- c(missing_files, ui_file)
  }
  
  if (!file.exists(server_file)) {
    missing_files <- c(missing_files, server_file)
  }
  
  if (length(missing_files) > 0) {
    stop("Critical files not found: \n", paste("- ", missing_files, collapse = "\n"), 
         "\nPlease ensure you're running the app from the correct directory.")
  }
  
  return(TRUE)
}

# Full app logic
ui_combined <- function() {
  uiOutput("app_ui")
}

server_combined <- function(input, output, session) {
  user_authenticated <- reactiveVal(FALSE)
  last_activity <- reactiveVal(Sys.time())
  
  # Session timeout observer
  observe({
    invalidateLater(60000) # Check every minute
    if (user_authenticated() && difftime(Sys.time(), last_activity(), units = "mins") > 30) {
      showNotification("Session timed out due to inactivity", type = "warning")
      user_authenticated(FALSE)
      session$reload()
    }
  })
  
  # Update last activity time on any input change
  observeEvent(reactiveValuesToList(input), {
    if(user_authenticated()) {
      last_activity(Sys.time())
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # Handle both button click and enter press
  observeEvent(input$login_btn, {
    attempt_login()
  })
  
  observeEvent(input$enter_pressed, {
    attempt_login()
  })
  
  # Consolidated login function
  attempt_login <- function() {
    req(input$user, input$password)
    
    # Trim whitespace and standardize case if needed
    username <- trimws(input$user)
    password <- trimws(input$password)
    
    # Debugging output (remove in production)
    cat(sprintf("Login attempt - Username: '%s', Password: '%s'\n", username, password))
    
    if (identical(username, credentials$user) && identical(password, credentials$password)) {
      shinyjs::hide("login_error")
      user_authenticated(TRUE)
      # Reset fields after successful login
      updateTextInput(session, "user", value = "")
      updateTextInput(session, "password", value = "")
    } else {
      shinyjs::show("login_error")
      # Clear password field on failed attempt
      updateTextInput(session, "password", value = "")
      # Add shake animation to indicate error
      shinyjs::runjs('
        $(".login-box").effect("shake", { times: 2, distance: 5 }, 200);
      ')
    }
  }
  
  # Logout handler
  observeEvent(input$logout_btn, {
    user_authenticated(FALSE)
    session$reload()  # This will refresh the app, clearing any state
  })
  
  output$app_ui <- renderUI({
    if (user_authenticated()) {
      if(check_source_files()) {
        # Load actual UI & server from source files
        source("R/ui.R", local = TRUE)
        source("R/server.R", local = TRUE)
        return(ui)
      } else {
        return(tags$div("Error loading application UI"))
      }
    } else {
      login_ui
    }
  })
  
  observe({
    if (user_authenticated()) {
      if(check_source_files()) {
        # Load server logic
        source("R/server.R", local = TRUE)
        server(input, output, session)
      }
    }
  })
}

# Run the app
shinyApp(ui_combined, server_combined)