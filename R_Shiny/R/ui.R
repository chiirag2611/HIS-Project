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
library(viridis)
library(dlookr)
library(rmarkdown)

#dashboardPage with custom styles and layout
ui <- dashboardPage(
  dashboardHeader(
    title = "📊DataPrepHIS", 
    titleWidth = 200,
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
      menuItem("Report Generation", tabName = "report_generation", icon = icon("file-alt")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))

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
          margin-right: 5px;
          margin-bottom: 5px;
          white-space: nowrap;
        }
        .btn-action:hover {
          background-color: #66B2FF;
          color: white;
        }
        .button-group {
          display: flex;
          flex-wrap: wrap;
          gap: 5px;
          margin-bottom: 15px;
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
        .js-plotly-plot, .plot-container {
          width: 100% !important;
          max-width: 100% !important;
        }
        .plotly-graph-div {
          min-height: 400px;
          width: 100% !important;
        }        .plotly {
          width: 100%;
          height: 100%;
        }          /* Drag and drop zone styling */        .drag-drop-zone {
          border: 3px dashed #3399FF;
          border-radius: 10px;
          padding: 13px;
          text-align: center;
          color: #666;
          margin-bottom: 9px;
          transition: all 0.3s;
          background-color: #f8f9fa;
          cursor: pointer;
        }
        .drag-drop-zone:hover, .drag-drop-zone.dragover {
          background-color: #e6f2ff;
          border-color: #0066cc;
          border-width: 3px;
        }        .drag-drop-zone .icon {
          font-size: 35px;
          color: #3399FF;
          margin-bottom: 9px;
        }
        .drag-drop-zone p {
          margin: 2px 0;
        }        .drag-drop-zone .drag-text {
          font-weight: bold;
          font-size: 15px;
        }        .drag-drop-zone .file-info {
          font-size: 12px;
          color: #888;
        }
        .drag-drop-zone.file-uploaded {
          background-color: #d4edda;
          border-color: #28a745;
          border-width: 3px;
        }        .drag-drop-zone.file-uploaded .icon {
          color: #28a745;
        }
        #fileInput {
          display: none !important;
        }
      ")),
      tags$script(HTML('
        function triggerColumnSelection(triggerId) {
          Shiny.setInputValue("column_selection_trigger", triggerId, {priority: "event"});
        }
        
        // Function to update drag-drop zone appearance
        function updateDropZoneStatus(isUploaded, fileName) {
          var dropZone = document.getElementById("dragDropZone");
          if (dropZone) {
            if (isUploaded && fileName) {
              $(dropZone).addClass("file-uploaded");
              var dragText = dropZone.querySelector(".drag-text");
              if (dragText) {
                dragText.textContent = "File Uploaded: " + fileName;
              }
            } else {
              $(dropZone).removeClass("file-uploaded");
              var dragText = dropZone.querySelector(".drag-text");
              if (dragText) {
                dragText.textContent = "Drag & Drop Files Here";
              }
            }
          }
        }
        
        $(function() { 
          $("[data-toggle=\'tooltip\']").tooltip();
          
          // Drag and drop handling
          var dropZone = document.getElementById("dragDropZone");
          
          if (dropZone) {
            dropZone.addEventListener("dragover", function(e) {
              e.preventDefault();
              e.stopPropagation();
              $(this).addClass("dragover");
            });
            
            dropZone.addEventListener("dragleave", function(e) {
              e.preventDefault();
              e.stopPropagation();
              $(this).removeClass("dragover");
            });
            
            dropZone.addEventListener("drop", function(e) {
              e.preventDefault();
              e.stopPropagation();
              $(this).removeClass("dragover");
              
              if (e.dataTransfer.files.length > 0) {
                // Get the file input element
                var fileInput = document.getElementById("fileInput");
                
                // Create a new FileList-like object with the dropped files
                var dT = new DataTransfer();
                for (var i = 0; i < e.dataTransfer.files.length; i++) {
                  dT.items.add(e.dataTransfer.files[i]);
                }
                
                // Set the files property
                fileInput.files = dT.files;
                
                // Trigger change event to notify Shiny
                $(fileInput).trigger("change");
                
                // Update drop zone status
                updateDropZoneStatus(true, e.dataTransfer.files[0].name);
              }
            });
            
            // Also trigger file dialog on click
            dropZone.addEventListener("click", function() {
              document.getElementById("fileInput").click();
            });
          }
          
          // Handle custom messages from server
          Shiny.addCustomMessageHandler("updateDropZone", function(message) {
            updateDropZoneStatus(message.uploaded, message.fileName);
          });
        });
      '))
    ),
    useShinyjs(),
    
    tabItems(
      # First tab item
      tabItem(
        tabName = "load_data",
        fluidRow( box(
            title = HTML("Loading Data <span data-toggle='tooltip' title='Upload your dataset in CSV or Excel format' class='fa fa-question-circle'></span>"),
            solidHeader = TRUE,
            width = 6,
            class = "custom-box",
            div(id = "dragDropZone", class = "drag-drop-zone",
              div(class = "icon", icon("file-upload")),
              p(class = "drag-text", "Drag & Drop Files Here"),
              p("or click to browse"),
              p(class = "file-info", "Supported formats: CSV, Excel (.xlsx, .xls)")
            ),
            div(style = "display: none;",
              fileInput("fileInput", "Browse File", accept = c(".csv", ".xlsx", ".xls"))
            )
          ),
          box(
            title = HTML("File Details <span data-toggle='tooltip' title='Overview the dimensions and data types of your uploaded dataset.' class='fa fa-question-circle'></span>"),
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
                  style = "margin-bottom: 15px; display: flex; flex-wrap: wrap; gap: 5px;",
                  actionButton("select_all_cols", "Select All", class = "btn-sm btn-action"),
                  actionButton("deselect_all_cols", "Deselect All", class = "btn-sm btn-action")
                ),
                div(
                  style = "margin-bottom: 15px; display: flex; flex-wrap: wrap; gap: 5px;",
                  actionButton("select_numeric_cols", "Select Numeric", class = "btn-sm btn-action"),
                  actionButton("select_categorical_cols", "Select Categorical", class = "btn-sm btn-action"),
                  actionButton("select_other_cols", "Select Other", class = "btn-sm btn-action", style = "margin-top: 5px;")
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
    div(
      style = "display: flex; justify-content: space-between; margin-top: 15px;",
      actionButton("apply_drop", "Delete", class = "btn-apply"),
      actionButton("select_drop_cols", "Select Multiple Columns",
                   class = "btn-sm btn-action",
                   onclick = "triggerColumnSelection('drop_feature')")
    )
  ),
  box(
    title = HTML("Add Feature <span data-toggle='tooltip' title='Add back previously dropped features to your dataset' class='fa fa-question-circle'></span>"),
    solidHeader = TRUE,
    width = 6,
    uiOutput("add_feature_ui"),
    div(
      style = "display: flex; justify-content: space-between; margin-top: 15px;",
      actionButton("apply_add", "Insert", class = "btn-apply"),
      actionButton("select_add_cols", "Select Multiple Columns",
                   class = "btn-sm btn-action",
                   onclick = "triggerColumnSelection('add_feature')")
    )
  )
),
        fluidRow(
          box(
            title = HTML("Convert Numerical to Categorical <span data-toggle='tooltip' title='Transform numerical variables into categorical format for classification or grouping' class='fa fa-question-circle'></span>"),
            solidHeader = TRUE,
            width = 6,
            selectInput("num_to_cat", "Select Numerical Variables:", 
            choices = NULL, multiple = TRUE),
            div(
              style = "display: flex; justify-content: space-between; margin-top: 15px;",
              actionButton("apply_num_to_cat", "Convert to Categorical", class = "btn-apply"),
              actionButton("select_num_to_cat_cols", "Select Multiple Columns",
               class = "btn-sm btn-action",
               onclick = "triggerColumnSelection('num_to_cat')")
            )
          ),
          box(
            title = HTML("Convert Categorical to Numerical <span data-toggle='tooltip' title='Transform categorical variables into numerical format for use in mathematical operations' class='fa fa-question-circle'></span>"),
            solidHeader = TRUE,
            width = 6,
            selectInput("cat_to_num", "Select Categorical Variables:",
            choices = NULL, multiple = TRUE),
            div(
              style = "display: flex; justify-content: space-between; margin-top: 15px;",
              actionButton("apply_cat_to_num", "Convert to Numerical", class = "btn-apply"),
              actionButton("select_cat_to_num_cols", "Select Multiple Columns",
               class = "btn-sm btn-action",
               onclick = "triggerColumnSelection('cat_to_num')")
            )
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
      # actionButton("submit_data", "Submit", 
      #              style = "padding: 10px 20px; font-size: 18px; background-color: #A84058; color: white;"),
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
            title = HTML("Variable Selection for Univariate Analysis <span data-toggle='tooltip' title='Select a variable for univariate analysis' class='fa fa-question-circle'></span>"), 
            solidHeader = TRUE, 
            width = 6,
            selectInput("x_var", "Variable:", choices = NULL),
            actionButton("select_univar_cols", "Search Variable", 
                        class = "btn-sm btn-action",
                        onclick = "triggerColumnSelection('visualization_univar')")
          ),
          # Box for two-variable selection
          box(
            title = HTML("Variable Selection for Bivariate Analysis <span data-toggle='tooltip' title='Select two variables for relationship analysis' class='fa fa-question-circle'></span>"), 
            solidHeader = TRUE, 
            width = 6,
            selectInput("x_var_bi", "X Variable:", choices = NULL),
            actionButton("select_x_var_bi_cols", "Quick Search X Variable", 
                        class = "btn-sm btn-action",
                        onclick = "triggerColumnSelection('visualization_x_var')"),
            selectInput("y_var", "Y Variable:", choices = NULL),
            actionButton("select_y_var_cols", "Quick Search Y Variable", 
                        class = "btn-sm btn-action",
                        onclick = "triggerColumnSelection('visualization_y_var')")
          )
        ),
        fluidRow(
          # Unidimensional analysis box
          box(
            title = "Unidimensional Analysis", solidHeader = TRUE, width = 6, height = "500px",
            tabsetPanel(
              tabPanel(
                HTML("Histogram <span data-toggle='tooltip' title='Visualize the distribution of a single variable' class='fa fa-question-circle'></span>"),
                div(style = "height: 400px; width: 100%;",
                    plotlyOutput("histogram", height = "400px"))
              ),
              tabPanel(
                HTML("Box Plot <span data-toggle='tooltip' title='View the distribution, median, and potential outliers' class='fa fa-question-circle'></span>"),
                div(style = "height: 400px; width: 100%;",
                    plotlyOutput("boxplot", height = "400px"))
              ),
              tabPanel(
                HTML("Pie Chart <span data-toggle='tooltip' title='Show proportion of categorical values' class='fa fa-question-circle'></span>"),
                conditionalPanel(
                  condition = "output.is_categorical === true",
                  div(style = "height: 400px; width: 100%; overflow: hidden;",
                      plotOutput("pie_chart", height = "400px", width = "100%"))
                )
              )
            )
          ),
          
          # Bidimensional analysis box
          box(
            title = "Bidimensional Analysis", solidHeader = TRUE, width = 6, height = "500px",
            tabsetPanel(
              tabPanel(
                  HTML("Correlation Plot <span data-toggle='tooltip' title='Visualize relationship between two variables' class='fa fa-question-circle'></span>"),
                  div(style = "height: 360px; width: 100%;",
                  plotlyOutput("bivariate_analysis", height = "100%")),
                  conditionalPanel(
                  condition = "output.show_correlation === true",
                  div(style = "padding: 5px; text-align: center;",
                  h5(textOutput("correlation_coefficient"), style = "color: #A84058; margin: 0;"))
                  )
              ),
              tabPanel(
                HTML("Correlation Matrix <span data-toggle='tooltip' title='View correlations between all numeric variables' class='fa fa-question-circle'></span>"),
                div(style = "height: 400px; width: 100%; overflow: hidden;",
                    plotOutput("correlation_matrix_plot", height = "400px", width = "100%"))
              ),
              tabPanel(
                HTML("Box Plot <span data-toggle='tooltip' title='Compare distributions across categories' class='fa fa-question-circle'></span>"),
                div(style = "height: 360px; width: 100%; overflow: hidden;",
                plotOutput("boxplot_parallel", height = "100%", width = "100%")),
                conditionalPanel(
                  condition = "output.show_correlation_ratio === true",
                  div(style = "padding: 5px; text-align: center;",
                      h5(textOutput("correlation_ratio"), style = "color: #2E86C1; margin: 0;"))
                  )
                )
            )
          )
        ),
      ),
      # Report Generation tab
      tabItem(
        tabName = "report_generation",
        fluidRow(
          box(
            title = HTML("Report Generation <span data-toggle='tooltip' title='Configure your data report settings' class='fa fa-question-circle'></span>"),
            solidHeader = TRUE,
            width = 12,
            status = "primary",
            fluidRow(
              column(
                width = 8,
                div(
                  style = "background-color: #f8f9fa; border-radius: 10px; padding: 20px; margin-bottom: 20px;",
                  h4(icon("file-alt"), " Data Processing Report"),
                  p("Generate a comprehensive report of your dataset and all preprocessing steps applied. 
                    The report includes data summaries, visualizations, and transformation details."),
                  tags$ul(
                    tags$li("Data summary statistics and structure"),
                    tags$li("Data Preprocessing operations performed"),
                    tags$li("Data exploration and missing value handling"),
                    tags$li("Data visualizations including correlation plots and bivariate analysis"),
                    tags$li("Executive summary of all operations performed")
                  )
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                div(
                  style = "display: flex; justify-content: center; margin-top: 25px;",
                  downloadButton(
                    "download_report", 
                    "Download HTML Report", 
                    class = "btn-primary",
                    style = "padding: 12px 25px; font-size: 16px; background-color: #3399FF; border-radius: 5px; box-shadow: 0px 2px 5px rgba(0, 0, 0, 0.1);"
                  )
                ),
                div(
                  style = "text-align: center; margin-top: 15px;",
                  tags$small("The report will be generated based on your current dataset and applied transformations.")
                )
              )
            )
          )
        )
      ),
    
          tabItem(
        tabName = "about",
        fluidRow(
          box(
            title = "📊DataPrepHIS Tool: User Manual",
            solidHeader = TRUE,
            width = 12,
            div(style = "overflow-y: auto; max-height: 80vh;", # Enables scrolling
                HTML('
<h2>Overview</h2>
<p>This Shiny application offers a comprehensive platform for data analysis and preprocessing. It supports end-to-end workflows, from loading datasets to cleaning data through imputation, outlier detection, and visualizations.</p>

<h2>Features</h2>
<ul>
  <li>Data Import: Upload datasets in CSV or Excel file formats.</li>
  <li>Data Preprocessing: Address missing values, scale numerical data, encode categorical features, and handle outliers.</li>
  <li>Data visualization: Conduct univariate and bivariate analysis using histograms, box plots, correlation matrices, and detailed statistical metrics.</li>
  <li>Model Evaluation: Visualize performance metrics and display the confusion matrix.</li>
  <li>Operation Summary: Download a comprehensive summary of all performed tasks.</li>
</ul>

<h2>Application Structure</h2>
<h3>Sidebar Menu</h3>
<ul>
  <li>Load Data: Enables users to upload datasets and examine their structure.</li>
  <li>Preprocessing: Provides tools to handle missing values, manage outliers, and apply feature transformations. Supports univariate and bivariate analysis through visualizations.</li>
  <li>Data Visualizations: Includes variable selection for univariate and bivariate analysis, along with histogram, box plot, pie chart, correlation plots, correlation matrix, and model performance visualizations.</li>
  <li>Report Generation: Allows users to download a detailed summary of all preprocessing operations performed.</li>
  <li>About: Shares project information and acknowledgments.</li>
</ul>

<h2>Detailed Features</h2>

<h3>1. Load Data</h3>
<p>The Load Data section allows users to upload and explore datasets. Key functionalities include:</p>
<ul>
  <li><strong>File Upload:</strong>
    <ul>
      <li>Supports CSV and Excel files.</li>
      <li>Displays error notifications for unsupported file types or missing sheets.</li>
    </ul>
  </li>
  <li><strong>File Details:</strong>
    <ul>
      <li>Displays metadata such as file name, format, number of rows (instances), number of columns (features), and counts of categorical and numerical features.</li>
    </ul>
  </li>
  <li><strong>Column Selector:</strong>
    <ul>
      <li>Provides multiple options for selecting columns for analysis, including: First N, Last N, Range, Select All, Deselect All, and search-based selection.</li>
      <li>Supports both single and multiple column selection.</li>
    </ul>
  </li>
  <li><strong>Dataset Exploration:</strong>
    <ul>
      <li>Offers a scrollable and searchable table view of the uploaded dataset.</li>
      <li>Displays a summary of each column, including type (numerical or categorical), missing values, and basic statistics such as mean, median, and standard deviation.</li>
    </ul>
  </li>
  <li><strong>Feature Management:</strong>
    <ul>
      <li><strong>Drop Features:</strong> Dynamically select and remove specific features (columns) from the dataset, with confirmation feedback.</li>
      <li><strong>Add Features:</strong> Re-add previously removed features back into the dataset.</li>
      <li><strong>Convert Features:</strong>
        <ul>
          <li><strong>Numerical to Categorical:</strong> Converts selected numerical variables to categorical by changing their data type to factors.</li>
          <li><strong>Categorical to Numerical:</strong> Converts selected categorical variables to numerical by restoring original values or attempting numeric conversion where possible.</li>
        </ul>
        <li>Displays confirmation dialogs for successful conversions and error messages for unsuccessful attempts.</li>
      </li>
    </ul>
  </li>
</ul>


<h3>2. Preprocessing</h3>
<p>The Preprocessing section provides advanced tools for cleaning and preparing the dataset:</p>
<ul>
  <li><strong>Handling Missing Values</strong>
    <ul>
      <li><strong>Variable Selection:</strong> Dynamically select a variable to manage its missing values.</li>
      <li><strong>Methods:</strong>
        <ul>
          <li>Row Deletion: Removes rows containing missing values for the selected variable (From Load Data Section).</li>
          <li>Handle with Mode: Fills missing values with the most frequent value.</li>
          <li>Handle with Median: Fills missing values with the median of the variable.</li>
          <li>Handle with Mean: Fills missing values with the mean of the variable.</li>
          <li>Create Missing Category: Fills missing values with the text "Missing" if user wants to keep it explicit.</li>
        </ul>
      </li>
      <li><strong>Feedback:</strong> Provides detailed notifications and modal dialogs summarizing changes, including the number of missing values handled and the chosen method.</li>
    </ul>
  </li>
  <li><strong>Handle Outliers</strong>
    <ul>
      <li><strong>Variable Selection:</strong> Supports numerical variables for outlier handling.</li>
      <li><strong>Outlier Detection:</strong> Uses the Interquartile Range (IQR) method to identify outliers.</li>
      <li><strong>Methods:</strong>
        <ul>
          <li>Remove Outliers: Deletes rows containing outliers.</li>
          <li>Replace with Median: Replaces outliers with the median value of the variable.</li>
          <li>Replace with Mean: Replaces outliers with the mean value of the variable.</li>
        </ul>
      </li>
      <li><strong>Feedback:</strong> Provides a summary of the number of outliers detected and handled, along with the selected method, through modal dialogs and notifications.</li>
    </ul>
  </li>
  <li><strong>Data Transformation</strong>
    <ul>
      <li><strong>Variable Selection:</strong> Dynamically select one or more numerical variables for transformation.</li>
      <li><strong>Transformation Methods:</strong>
        <ul>
          <li>Min-Max Scaling: Scales values to a range of [0, 1].</li>
          <li>Z-Score Normalization: Standardizes values by centering around the mean and scaling to unit variance.</li>
          <li>Log Transformation: Applies log transformation to handle skewed distributions (adds 1 to avoid log of zero).</li>
        </ul>
      </li>
      <li><strong>Feedback:</strong> Summarizes the transformations applied, specifying the variables and methods, in a detailed modal dialog.</li>
    </ul>
  </li>
  <li><strong>Data Encoding</strong>
    <ul>
      <li><strong>Variable Selection:</strong> Dynamically identify categorical variables for encoding.</li>
      <li><strong>Encoding Methods:</strong>
        <ul>
          <li>Label Encoding: Converts categorical values to integer labels.</li>
          <li>One-Hot Encoding: Expands categorical variables into multiple binary columns.</li>
        </ul>
      </li>
      <li><strong>Feedback:</strong> Confirms successful encoding through notifications and modal dialogs.</li>
    </ul>
  </li>
  <li><strong>Download Preprossed File</strong>
    <ul>
      <li><strong>CSV download:</strong> Allows user to export the pre-processed dataset in CSV format.</li>
  </li>
</ul>

<h3>3. Visualize Data</h3>
<p>The Visualization Data section offers a comprehensive set of graphical tools and charts for univariate and bivariate analysis.</p>
<ul>
  <li><strong>Univariate Analysis</strong>
    <ul>
      <li>Histograms: Visualize the distribution of a selected numerical variable.</li>
      <li>Box Plots: Summarize the spread, median, and outliers of a numerical variable.</li>
      <li>Pie Charts: Display proportions of categories for categorical variables or numerical variables with few unique values (treated as categories).</li>
    </ul>
  </li>
  <li><strong>Bivariate Analysis</strong>
    <ul>
      <li>Correlation Plots: Scatter plots for visualizing relationships between two variables, with optional regression lines for numeric pairs.</li>
      <li>Correlation Coefficients: Calculate and display the correlation coefficient (r) for two numeric variables.</li>
      <li>Correlation Matrices: Generate heatmaps showing correlations between all numeric variables in the dataset.</li>
      <li>Parallel Box Plots: Display box plots of a numeric variable grouped by a categorical variable.</li>
      <li>Correlation Ratios: Measure the strength of the relationship between a categorical and a numerical variable.</li>
    </ul>
  </li>
</ul>













<h3>4. Report Generation</h3>
<p>The <strong>Detailed Summary</strong> section offers insights into preprocessing and visualization steps performed in the app.</p>
<p>Generate a comprehensive report including:</p>
<ul>
  <li>Dataset structure and summary statistics</li>
  <li>Missing value analysis and treatment</li>
  <li>Outlier detection and handling</li>
  <li>Transformation and encoding summary</li>
  <li>Key visualizations and correlation insights</li>
</ul>
<p><strong>Report Sections:</strong></p>
<ol>
  <li>Dataset Overview</li>
  <li>Preprocessing Operations</li>
  <li>Missing Value Handling</li>
  <li>Outlier Detection</li>
  <li>Visual Summaries & Correlations</li>
  <li>Executive Summary</li>
</ol>






<h3>How to Use</h3>

<p><strong>Start the App:</strong> Run the application in RStudio or using Shiny Server with the command:</p>

<pre><code>shiny::runApp("path_to_app")</code></pre>

<p><strong>Login:</strong> Enter your username and password on the secure login screen. (Default: <code>user = "HIS"</code>, <code>password = "1234"</code>)</p>

<p><strong>Upload Data:</strong> Navigate to the <em>Load Data</em> tab and upload your dataset (CSV or Excel files).</p>

<p><strong>Preprocess Data:</strong> Use the <em>Preprocessing</em> tab to handle missing values, detect and treat outliers, apply transformations, and encode categorical variables.</p>

<p><strong>Visualize Data:</strong> Explore data patterns and relationships using the <em>Visualize Data</em> tab, featuring interactive plots and statistical summaries.</p>

<p><strong>View Results and Reports:</strong> Visit the <em>Report Generation</em> tab to review metrics and generated visual reports.</p>

<p><strong>Logout and Session Timeout:</strong> Use the logout button to end your session securely. Sessions automatically timeout after 30 minutes of inactivity.</p>

<h3>Technical Details</h3>

<h4>Libraries Used</h4>

<ul>
  <li><strong>UI & Interaction:</strong> shiny, shinydashboard, shinyjs, bslib, shinyjqui, shinycssloaders, shinyBS, shinyvalidate</li>
  <li><strong>Data Handling:</strong> readxl, DT, reactable, dplyr, tidyr, lubridate, mice, missForest</li>
  <li><strong>Visualization:</strong> plotly, ggplot2, corrplot, viridis, GGally, moments</li>
  <li><strong>Preprocessing & Modeling:</strong> ROSE, caret, smotefamily, dlookr</li>
  <li><strong>Reporting:</strong> rmarkdown, knitr, kableExtra</li>
</ul>

<p>The app automatically checks for and installs missing packages at startup.</p>

<h4>System Requirements</h4>

<ul>
  <li><strong>R Version:</strong> 4.0 or higher.</li>
  <li><strong>Required Folders:</strong> <code>www</code> (for logos and assets) and <code>R</code> (for UI/server code) folders are created automatically if missing.</li>
  <li><strong>Static Assets:</strong> Place your logo as <code>www/logo.png</code> to display on the login screen.</li>
  <li><strong>Browser:</strong> Use a modern web browser with JavaScript enabled for full functionality.</li>
</ul>
<h3>Contributors</h3>
<ul>
  <li><strong>Adesh Shirke (adesh.shirke@stud.fra-uas.de)</strong>: 1541788</li>
  <li><strong>Chirag Chawla (chirag.chawla@stud.fra-uas.de)</strong>: 1541803</li>
  <li><strong>Samyak Soni (samyak.soni@stud.fra-uas.de)</strong>: 1521498</li>
</ul>

')
            )
          )
        )
      )
    ),

    # Column Selection Modal - moved inside dashboardBody
    bsModal(
  id = "columnSelectionModal",
  title = "Select Columns for Operation",
  trigger = "column_selection_trigger",
  size = "large",
  fluidRow(
    column(
      width = 3,
      h4("Quick Actions"),
      div(
        style = "margin-bottom: 15px; display: flex; flex-wrap: wrap; gap: 5px;",
        actionButton("modal_select_all", "Select All", class = "btn-sm btn-action"),
        actionButton("modal_deselect_all", "Deselect All", class = "btn-sm btn-action")
      ),
      conditionalPanel(
  condition = "!(input.column_selection_trigger == 'encoding' || input.column_selection_trigger == 'transformation' || input.column_selection_trigger == 'outliers' || input.column_selection_trigger == 'num_to_cat' || input.column_selection_trigger == 'cat_to_num')",
  div(
    style = "margin-bottom: 15px; display: flex; flex-wrap: wrap; gap: 5px;",
    actionButton("modal_select_numeric", "Select Numeric", class = "btn-sm btn-action"),
    actionButton("modal_select_categorical", "Select Categorical", class = "btn-sm btn-action"),
    actionButton("modal_select_other", "Select Other", class = "btn-sm btn-action", style = "margin-top: 5px;")
  )
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