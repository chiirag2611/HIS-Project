library(shiny)
library(shinydashboard)
library(bslib)
library(DT)
library(plotly)
library(reactable)
library(shinyjs)
library(shinyjqui)
library(shinyBS)
library(corrplot)


ui <- dashboardPage(
  dashboardHeader(title = "📊DataPrepHIS", titleWidth = 200,
    tags$li(
      class = "dropdown",
      actionButton(
        "logout_btn", 
        "Logout", 
        icon = icon("sign-out-alt"),
        style = "color: #fff; background-color: transparent; border: none; margin-top: 8px;"
      )
    )
  ),
  
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      menuItem("Load Data", tabName = "load_data", icon = icon("file-upload", lib = "font-awesome")),
      menuItem("PreProcessing", tabName = "preprocessing", icon = icon("cogs")),
      menuItem("Data Visualization", tabName = "visualization", icon = icon("chart-bar")),
      menuItem("Report Generation", tabName = "report_generation", icon = icon("file-alt"))
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
          font-size: 12px;
        }
        .btn-action:hover {
          background-color: #66B2FF;
          color: white;
        }
        .btn-apply {
          background-color: #00796B;
          color: white;
          font-weight: bold;
        }
        .btn-apply:hover {
          background-color: #00897B;
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
        .modal-lg {
          width: 80%;
        }
        .operation-applied {
          border-left: 5px solid #4CAF50;
          padding-left: 10px;
        }
        @media (max-width: 768px) {
          .box {
            width: 100% !important;
          }
          .action-buttons {
            flex-direction: column;
          }
          .action-buttons .btn {
            margin-bottom: 10px;
          }
        }
      ")),
      tags$script(HTML('
        function triggerColumnSelection(triggerId) {
          Shiny.setInputValue("column_selection_trigger", triggerId, {priority: "event"});
        }
        $(function() { 
          $("[data-toggle=\'tooltip\']").tooltip();
        });
      '))
    ),
    useShinyjs(),
    tabItems(
      # First tab item
      tabItem(
        tabName = "load_data",
        fluidRow(
          box(
            title = HTML("Loading Data <span data-toggle='tooltip' title='Upload your dataset in CSV or Excel format' class='fa fa-question-circle'></span>"),
            solidHeader = TRUE,
            width = 6,
            class = "custom-box",
            fileInput("fileInput", "Browse File", accept = c(".csv", ".xlsx")),
            helpText("Upload a CSV or Excel file.")
          ),
          box(
            title = HTML("File Details <span data-toggle='tooltip' title='View information about your uploaded dataset including dimensions and data types' class='fa fa-question-circle'></span>"),
            solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("fileDetails")
          )
        ),
        # Column Selector Section
        fluidRow(
          box(
            title = HTML("Column Selector <span data-toggle='tooltip' title='Select specific columns from your dataset for analysis and preprocessing' class='fa fa-question-circle'></span>"),
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(
                width = 3,
                h4(HTML("Quick Actions <span data-toggle='tooltip' title='Tools to quickly select or filter columns in your dataset' class='fa fa-question-circle'></span>")),
                div(
                  style = "margin-bottom: 15px;",
                  actionButton("select_all_cols", "Select All", class = "btn-sm btn-action"),
                  actionButton("deselect_all_cols", "Deselect All", class = "btn-sm btn-action")
                ),
                div(
                  style = "margin-bottom: 15px;",
                  actionButton("select_numeric_cols", "Select Numeric", class = "btn-sm btn-action"),
                  actionButton("select_categorical_cols", "Select Categorical", class = "btn-sm btn-action")
                ),
                h4(HTML("First/Last Columns <span data-toggle='tooltip' title='Select columns based on their position in the dataset' class='fa fa-question-circle'></span>")),
                div(
                  style = "margin-bottom: 15px;",
                  numericInput("first_n_cols", "Select First N Columns:", value = 10, min = 1, max = 1000),
                  actionButton("apply_first_n", "Apply", class = "btn-sm btn-action")
                ),
                div(
                  style = "margin-bottom: 15px;",
                  numericInput("last_n_cols", "Select Last N Columns:", value = 10, min = 1, max = 1000),
                  actionButton("apply_last_n", "Apply", class = "btn-sm btn-action")
                ),
                h4(HTML("Range Selection <span data-toggle='tooltip' title='Select a continuous range of columns by position' class='fa fa-question-circle'></span>")),
                div(
                  style = "margin-bottom: 15px;",
                  uiOutput("range_slider_ui"),
                  actionButton("apply_range", "Apply Range", class = "btn-sm btn-action")
                )
              ),
              column(
                width = 9,
                h4(HTML("Column Selection <span data-toggle='tooltip' title='Individually select columns to include in your analysis' class='fa fa-question-circle'></span>")),
                div(
                  style = "margin-bottom: 15px;",
                  textInput("search_columns", "Search Columns:", placeholder = "Type to search...")
                ),
                div(
                  style = "max-height: 400px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; border-radius: 5px;",
                  uiOutput("column_selector_ui")
                )
              )
            ),
            div(
              style = "margin-top: 15px; text-align: right;",
              actionButton("apply_column_filter", "Apply Selection", class = "btn-primary")
            )
          )
        ),
        fluidRow(
          box(
            title = HTML("Data Exploration <span data-toggle='tooltip' title='View and analyze your dataset with summary statistics and data visualization' class='fa fa-question-circle'></span>"),
            solidHeader = TRUE,
            width = 12,
            tabsetPanel(
              tabPanel(
                title = HTML("Show Data <span data-toggle='tooltip' title='View the raw data in tabular format' class='fa fa-question-circle'></span>"),
                style = "overflow-x: auto;",
                DTOutput("table"),
                width = 9
              ),
              tabPanel(
                title = HTML("Data Summary <span data-toggle='tooltip' title='View summary statistics and information about each column' class='fa fa-question-circle'></span>"),
                DTOutput("dataSummary")
              )
            )
          )
        ),
        fluidRow(
          box(
              title = HTML("Drop Feature <span data-toggle='tooltip' title='Remove selected features/columns from your dataset' class='fa fa-question-circle'></span>"),
              solidHeader = TRUE,
              width = 6,
              uiOutput("drop_feature_ui"),
              actionButton("apply_drop", "Delete")
            ),    
          box(
            title = HTML("Add Feature <span data-toggle='tooltip' title='Add back previously dropped features to your dataset' class='fa fa-question-circle'></span>"),
            solidHeader = TRUE,
            width = 6,
            uiOutput("add_feature_ui"),
            actionButton("apply_add", "Insert")
          )
        ),
        fluidRow(
          box(
            title = HTML("Convert Numerical to Categorical <span data-toggle='tooltip' title='Transform numerical variables into categorical format for classification or grouping' class='fa fa-question-circle'></span>"),
            solidHeader = TRUE,
            width = 6,
            selectInput("num_to_cat", "Select Numerical Variables:", 
            choices = NULL, multiple = TRUE),
            actionButton("apply_num_to_cat", "Convert to Categorical")
          ),
          box(
            title = HTML("Convert Categorical to Numerical <span data-toggle='tooltip' title='Transform categorical variables into numerical format for use in mathematical operations' class='fa fa-question-circle'></span>"),
            solidHeader = TRUE,
            width = 6,
            selectInput("cat_to_num", "Select Categorical Variables:",
             choices = NULL, multiple = TRUE),
            actionButton("apply_cat_to_num", "Convert to Numerical")
          )
        )
      ),
      
      # Second tab item
      tabItem(
        tabName = "preprocessing",
        fluidRow(
          box(
            title = HTML("Handle Missing Values <span data-toggle='tooltip' title='Remove or replace missing values in your dataset' class='fa fa-question-circle'></span>"), 
            solidHeader = TRUE, 
            width = 6,
            collapsible = TRUE,
            uiOutput("missing_var_ui"),
            textOutput("missing_percent"),
            uiOutput("missing_method_ui"),
            div(
              style = "display: flex; justify-content: space-between; margin-top: 15px;",
              actionButton("select_missing_cols", "Select Multiple Columns", 
                          class = "btn-sm btn-action",
                          onclick = "triggerColumnSelection('missing')"),
              actionButton("apply_missing", "Apply", class = "btn-apply")
            )
          ),
          box(
            title = HTML("Handle Outliers <span data-toggle='tooltip' title='Detect and manage outlier values in your dataset' class='fa fa-question-circle'></span>"), 
            solidHeader = TRUE, 
            width = 6,
            collapsible = TRUE,
            uiOutput("outlier_var_ui"),
            selectInput(
              inputId = "outlier_method", 
              label = "Select Outlier Handling Method:", 
              choices = c("Remove Outliers", "Replace with Median", "Replace with Mean"), 
              selected = "Remove Outliers"
            ),
            div(
              style = "display: flex; justify-content: space-between; margin-top: 15px;",
              actionButton("select_outlier_cols", "Select Multiple Columns", 
                          class = "btn-sm btn-action",
                          onclick = "triggerColumnSelection('outliers')"),
              actionButton("apply_outliers", "Apply", class = "btn-apply")
            )
          )
        ),
        fluidRow(
          box(
            title = HTML("Data Transformation <span data-toggle='tooltip' title='Scale or normalize your data for better model performance' class='fa fa-question-circle'></span>"),
            solidHeader = TRUE,
            width = 6,
            collapsible = TRUE,
            uiOutput("transform_var_ui"),
            selectInput(
              inputId = "transformation_method",
              label = "Select Transformation Method:",
              choices = c("Min-Max Scaling", "Z-Score Normalization", "Log Transformation"),
              selected = "Min-Max Scaling"
            ),
            div(
              style = "display: flex; justify-content: space-between; margin-top: 15px;",
              actionButton("select_transform_cols", "Select Multiple Columns", 
                          class = "btn-sm btn-action",
                          onclick = "triggerColumnSelection('transformation')"),
              actionButton("apply_transformation", "Apply", class = "btn-apply")
            )
          ),
          box(
            title = HTML("Encoding Data <span data-toggle='tooltip' title='Convert categorical variables to numerical format for machine learning algorithms' class='fa fa-question-circle'></span>"),
            solidHeader = TRUE,
            width = 6,
            collapsible = TRUE,
            uiOutput("encoding_var_ui"),
            selectInput(
              inputId = "encoding_method",
              label = "Select Encoding Method:",
              choices = c("One-Hot Encoding","Label Encoding"),
              selected = "One-Hot Encoding"
            ),
            div(
              style = "display: flex; justify-content: space-between; margin-top: 15px;",
              actionButton("select_encoding_cols", "Select Multiple Columns", 
                          class = "btn-sm btn-action",
                          onclick = "triggerColumnSelection('encoding')"),
              actionButton("apply_encoding", "Apply", class = "btn-apply")
            )
          )
        ),
        # Preprocessing Summary
        fluidRow(
          box(
            title = "Preprocessing Summary",
            solidHeader = TRUE,
            width = 12,
            status = "primary",
            tableOutput("preprocessing_summary")
          )
        ),
        fluidRow(
          column(
            width = 12,
            div(
              style = "text-align: center; margin-top: 20px; margin-bottom: 30px;",
              actionButton("submit_data", "Submit", 
                           style = "padding: 10px 20px; font-size: 18px; background-color: #A84058; color: white;"),
              downloadButton("save_data", "Save", 
                             class = "btn btn-secondary shiny-disabled",
                             style = "padding: 10px 20px; font-size: 18px; margin-left: 20px;")
            )
          )
        )
      ),
      # Data Visualization tab
      tabItem(
        tabName = "visualization",
        fluidRow(
          # Box for single variable selection
          box(
            title = "Variable Selection for Univariate Analysis", solidHeader = TRUE, width = 6,
            selectInput("x_var", "Variable:", choices = NULL)
          ),
          # Box for two-variable selection
          box(
            title = "Variable Selection for Bivariate Analysis", solidHeader = TRUE, width = 6,
            selectInput("x_var_bi", "X Variable:", choices = NULL),
            selectInput("y_var", "Y Variable:", choices = NULL)
          )
        ),
        fluidRow(
          # Unidimensional analysis box
          box(
            title = "Unidimensional Analysis", solidHeader = TRUE, width = 6,
            tabsetPanel(
              tabPanel(
                "Histogram", 
                #numericInput("binwidth_input", "Binwidth:", value = 0.2, min = 0.01, step = 0.01),
                plotlyOutput("histogram")
              ),
              tabPanel("Box Plot", plotlyOutput("boxplot")),
               # Add a Pie Chart tab, dynamically controlled
              tabPanel(
                "Pie Chart",
                conditionalPanel(
                  condition = "output.is_categorical === true", # Show only if variable is categorical
                  plotOutput("pie_chart", height = 500, width = 600)
                )
              )
            )
          ),
          # Bidimensional analysis box
          box(
            title = "Bidimensional Analysis", solidHeader = TRUE, width = 6,
            tabsetPanel(
              tabPanel(
                "Correlation Plot",
                plotlyOutput("bivariate_analysis"),
                conditionalPanel(
                  condition = "output.show_correlation === true",  # Show only for numeric variables
                  h4(textOutput("correlation_coefficient"), style = "color: red; margin-top: 10px;")
                )
              ),
              tabPanel("Correlation Matrix", plotOutput("correlation_matrix_plot")),
              tabPanel(
                "Box Plot",
                plotOutput("boxplot_parallel"),
                conditionalPanel(
                  condition = "output.show_correlation_ratio === true",  # Show only for quantitative vs qualitative
                  h4(textOutput("correlation_ratio"), style = "color: blue; margin-top: 10px;")
                )
              )
            )
          )
        )
      ),
      # Report Generation tab
      tabItem(
        tabName = "report_generation",
        fluidRow(
          box(
            title = "Generate Report", solidHeader = TRUE, width = 12,
            #actionButton("generate_report", "Generate Report", class = "btn-primary"),
            downloadButton("download_report", "Download Report", class = "btn-secondary")
          )
        )
      )
    ),
    # Column Selection Modal
    bsModal(
      id = "columnSelectionModal",
      title = "Select Columns for Operation",
      trigger = "column_selection_trigger",
      size = "large",
      fluidRow (
        column(
          width = 3,
          h4("Quick Actions"),
          div(
            style = "margin-bottom: 15px;",
            actionButton("modal_select_all", "Select All", class = "btn-sm btn-action"),
            actionButton("modal_deselect_all", "Deselect All", class = "btn-sm btn-action")
          ),
          div(
            style = "margin-bottom: 15px;",
            actionButton("modal_select_numeric", "Select Numeric", class = "btn-sm btn-action"),
            actionButton("modal_select_categorical", "Select Categorical", class = "btn-sm btn-action")
          )
        ),
        column(
          width = 9,
          h4("Column Selection"),
          div(
            style = "margin-bottom: 15px;",
            textInput("modal_search_columns", "Search Columns:", placeholder = "Type to search...")
          ),
          div(
            style = "max-height: 400px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; border-radius: 5px;",
            uiOutput("modal_column_selector")
          )
        )
      ),
      footer = tagList(
        actionButton("modal_cancel", "Cancel", class = "btn-default"),
        actionButton("modal_confirm", "Confirm Selection", class = "btn-primary")
      )
    )
  )
)