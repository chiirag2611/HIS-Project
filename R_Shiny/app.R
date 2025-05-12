library(shiny)
library(shinydashboard)
library(shinyjs)

# Static credentials
credentials <- list(user = "1234", password = "HIS")

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
    "))
  ),
  div(class = "login-box",
      div(class = "login-title", "🔐 Secure Login"),
      textInput("user", "Username", placeholder = "Enter username"),
      passwordInput("password", "Password", placeholder = "Enter password"),
      actionButton("login_btn", "Log in", class = "btn btn-primary btn-block"),
      div(id = "login_error", "❌ Invalid username or password.", style = "display: none;")
  )
)

# Load actual UI & server from source files
source("R/ui.R", local = TRUE)
source("R/server.R", local = TRUE)

# Full app logic
ui_combined <- function() {
  uiOutput("app_ui")
}

server_combined <- function(input, output, session) {
  user_authenticated <- reactiveVal(FALSE)
  
  observeEvent(input$login_btn, {
    if (input$user == credentials$user && input$password == credentials$password) {
      shinyjs::hide("login_error")
      user_authenticated(TRUE)
    } else {
      shinyjs::show("login_error")
    }
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
