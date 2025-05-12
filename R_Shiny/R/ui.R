library(shiny)
library(shinydashboard)
library(bslib)
library(DT)
library(plotly)
library(reactable)
library(shinyjs)
library(shinyjqui)

ui <- dashboardPage(
  dashboardHeader(title = "Data Pre-Processing for Machine Learning", titleWidth = 500),
  
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      menuItem("Load Data", tabName = "load_data", icon = icon("file-upload", lib = "font-awesome"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .main-header .navbar {
          background-color: #003366 !important;
        }
        .main-header .logo {
          background-color: #003366 !important;
          color: white !important;
        }
        .skin-blue .main-sidebar {
          background-color: #2E3B4E;
          color: #f8f9fa;
        }
        .skin-blue .sidebar-menu>li.active>a {
          background-color: #3399FF;
          color: white;
        }
        .skin-blue .sidebar-menu>li>a:hover {
          background-color: #66B2FF;
          color: white;
        }
        .box {
          border: 2px solid #003366;
          background-color: #f5f7fa;
          border-radius: 10px;
        }
        .box-header {
          background-color: #003366 !important;
          color: white !important;
          font-weight: bold;
          border-radius: 10px 10px 0 0;
        }
        .btn {
          background-color: #3399FF;
          color: white;
          border: none;
        }
        .btn:hover {
          background-color: #66B2FF;
          color: white;
        }
        .btn-action {
          background-color: #3399FF;
          color: white;
        }
        .btn-action:hover {
          background-color: #66B2FF;
          color: white;
        }
        .btn-primary {
          background-color: #3399FF;
          color: white;
        }
        .btn-primary:hover {
          background-color: #66B2FF;
          color: white;
        }
      "))
    ),
    useShinyjs(),
    tabItems(
      tabItem(
        tabName = "load_data",
        fluidRow(
          box(
            title = "Loading Data",
            solidHeader = TRUE,
            width = 6,
            class = "custom-box",
            fileInput("fileInput", "Browse File", accept = c(".csv", ".xlsx")),
            helpText("Upload a CSV or Excel file.")
          ),
          box(
            title = "File Details",
            solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("fileDetails")
          )
        ),
        fluidRow(
          box(
            title = "Data Exploration",
            solidHeader = TRUE,
            width = 12,
            tabsetPanel(
              tabPanel(
                title = "Show Data",
                style = "overflow-x: auto;",
                DTOutput("table"),
                width = 9
              ),
              tabPanel(
                title = "Data Summary",
                DTOutput("dataSummary")
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Drop Feature",
            solidHeader = TRUE,
            width = 12,
            uiOutput("drop_feature_ui"),
            actionButton("apply_drop", "Delete")
          )
        ),
        fluidRow(
          box(
            title = "Convert Numerical to Categorical",
            solidHeader = TRUE,
            width = 6,
            selectInput("num_to_cat", "Select Numerical Variables:", choices = NULL, multiple = TRUE),
            actionButton("apply_num_to_cat", "Convert to Categorical")
          ),
          box(
            title = "Convert Categorical to Numerical",
            solidHeader = TRUE,
            width = 6,
            selectInput("cat_to_num", "Select Categorical Variables:", choices = NULL, multiple = TRUE),
            actionButton("apply_cat_to_num", "Convert to Numerical")
          )
        )
      )
    )
  )
)
