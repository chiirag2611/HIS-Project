library(shiny)
library(shinydashboard)
library(bslib)
library(readxl)  
library(DT)
library(plotly)
library(ROSE)
library(caret)
library(smotefamily)

server <- function(input, output, session) {
  
  ## Loading Data
  
  # Reactive data to store the uploaded data 
  
  training_data <- reactiveVal(NULL)
  display_data <- reactiveVal(NULL)
  y <- reactiveVal(NULL)
  y_display <- reactiveVal(NULL)
  save_variable <- reactiveVal(NULL)
  
  # Main observer for file input
  observeEvent(input$fileInput, {
    req(input$fileInput)
    
    data <- load_file_data(input$fileInput)  # Load the file data
    if (is.null(data)) return(NULL)  # Stop if data couldn't be loaded
    
    training_data(data)
    display_data(data)
    
    # Show success notification when data is loaded successfully
    showNotification("Data uploaded successfully!", type = "message")
  })
  
  # Function to load data based on file type
  load_file_data <- function(fileInput) {
    file_ext <- tools::file_ext(fileInput$name)
    
    if (file_ext == "csv") {
      return(read.csv(fileInput$datapath))
    } else if (file_ext %in% c("xlsx", "xls")) {
      sheet_names <- excel_sheets(fileInput$datapath)
      if ("Data" %in% sheet_names) {
        return(read_excel(fileInput$datapath, sheet = "Data"))
      } else {
        showNotification("Sheet 'Data' not found in the file.", type = "error")
        return(NULL)
      }
    } else {
      showNotification("Unsupported file type", type = "error")
      return(NULL)
    }
  }
  
  ## Dataset initial details
  
  # Observer to print file details
  observeEvent(display_data(), {
    req(display_data())  # Ensure the data is available
    
    data <- display_data()
    
    file_name <- tools::file_path_sans_ext(input$fileInput$name)
    file_ext <- tools::file_ext(input$fileInput$name)
    
    num_instances <- nrow(data)
    num_features <- ncol(data)
    
    # Identify categorical and numerical features
    num_categorical <- sum(sapply(display_data(), function(x) is.factor(x) || is.character(x)))
    num_numerical <- num_features - num_categorical
    
    # Print the details to the UI
    output$fileDetails <- renderText({
      paste(
        " File Name:", file_name , "\n",
        "File Format:", file_ext, "\n",
        "Number of Instances:", num_instances, "\n",
        "Number of Features:", num_features, "\n",
        "Categorical Features:", num_categorical, "\n",
        "Numerical Features:", num_numerical
      )
    })
  })
  
  ## Data exploratation
  
  # Render the dataset in the "Show Data" tab
  output$table <- renderDT({
    req(display_data())  # Ensure data is available
    datatable(display_data(), options = list(scrollX = TRUE))
  })
  
  generate_summary <- function(data) {
    summary_df <- data.frame(
      Column = names(data),
      Class = sapply(data, function(x) if (is.numeric(x)) "Numerical" else "Categorical"),
      Missing = sapply(data, function(x) sum(is.na(x))),
      Min = sapply(data, function(x) if (is.numeric(x)) min(x, na.rm = TRUE) else "-"),
      Median = sapply(data, function(x) if (is.numeric(x)) median(x, na.rm = TRUE) else "-"),
      Max = sapply(data, function(x) if (is.numeric(x)) max(x, na.rm = TRUE) else "-"),
      Mean = sapply(data, function(x) if (is.numeric(x)) sprintf("%.3f", mean(x, na.rm = TRUE)) else "-"),
      SD = sapply(data, function(x) if (is.numeric(x)) sprintf("%.3f", sd(x, na.rm = TRUE)) else "-"),
      Variance = sapply(data, function(x) if (is.numeric(x)) sprintf("%.3f", var(x, na.rm = TRUE)) else "-"),
      Unique_Values = sapply(data, function(x) if (!is.numeric(x)) length(unique(x)) else "-")
    )
    
    # Ensure appropriate formatting for all columns
    summary_df[] <- lapply(summary_df, function(x) ifelse(is.na(x), "-", as.character(x)))
    return(summary_df)
  }
  
  # Render the dataset summary in the "Data Summary" tab
  
  output$dataSummary <- renderDT({
    req(display_data())  # Ensure data is available
    data <- display_data()
    
    # Generate the summary for numerical and categorical features
    summary_df <- generate_summary(data)
    
    # Render the summary as a datatable
    datatable(
      summary_df,
      options = list(scrollX = TRUE, pageLength = 10),  # Add scrolling and pagination
      rownames = FALSE
    )
  })
  
  ## Drop Feature
  
  # Dynamic UI for selecting features to drop
  output$drop_feature_ui <- renderUI({
    req(display_data())  # Ensure data is loaded
    selectInput(
      inputId = "drop_feature",
      label = "Select Feature(s) to Drop",
      choices = colnames(display_data()),  # Get column names from the displayed data
      multiple = TRUE  # Allow selecting multiple features
    )
  })
  
  # Observe the Apply Drop button
  observeEvent(input$apply_drop, {
    req(input$drop_feature)  # Ensure at least one feature is selected
    req(training_data())     # Ensure data is available
    
    # Get the selected features to drop
    selected_features <- input$drop_feature
    
    # Update training_data by removing the selected features
    updated_training_data <- training_data()
    updated_training_data <- updated_training_data[, !(colnames(updated_training_data) %in% selected_features)]
    training_data(updated_training_data)
    
    # Update display_data by removing the selected features
    updated_display_data <- display_data()
    updated_display_data <- updated_display_data[, !(colnames(updated_display_data) %in% selected_features)]
    display_data(updated_display_data)
    
    # Provide feedback to the user
    showNotification(
      paste("Dropped feature(s):", paste(selected_features, collapse = ", ")),
      type = "message"
    )
  })
  
  ## Switch Categorical and Numerical data
  
  # Dynamically update dropdown options
  observe({
    current_data <- display_data()
    
    numerical_vars <- names(current_data)[sapply(current_data, is.numeric)]
    categorical_vars <- names(current_data)[sapply(current_data, function(x) is.factor(x) || is.character(x))]
    
    updateSelectInput(session, "num_to_cat", choices = numerical_vars, selected = NULL)
    updateSelectInput(session, "cat_to_num", choices = categorical_vars, selected = NULL)
  })
  
  # Convert Numerical to Categorical
  observeEvent(input$apply_num_to_cat, {
    req(input$num_to_cat)  # Ensure input is not empty
    
    current_training_data <- training_data()
    current_display_data <- display_data()
    
    for (var in input$num_to_cat) {
      if (!is.factor(current_training_data[[var]])) {
        # Save the original values in an attribute for potential reconversion
        attr(current_training_data[[var]], "original_values") <- current_training_data[[var]]
        attr(current_display_data[[var]], "original_values") <- current_display_data[[var]]
      }
      current_training_data[[var]] <- as.factor(current_training_data[[var]])
      current_display_data[[var]] <- as.factor(current_display_data[[var]])
    }
    
    # Update datasets
    training_data(current_training_data)
    display_data(current_display_data)
    
    # Show confirmation dialog
    showModal(modalDialog(
      title = "Conversion Completed",
      paste("The following numerical variables have been converted to categorical:",
            paste(input$num_to_cat, collapse = ", ")),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
  })
  
  # Convert Categorical to Numerical
  observeEvent(input$apply_cat_to_num, {
    req(input$cat_to_num)  # Ensure input is not empty
    
    current_training_data <- training_data()
    current_display_data <- display_data()
    
    successfully_converted <- c()
    failed_conversions <- c()
    
    for (var in input$cat_to_num) {
      # Check if the variable has original numerical values stored as an attribute
      original_values <- attr(current_training_data[[var]], "original_values")
      if (!is.null(original_values)) {
        # Restore the original numerical values
        current_training_data[[var]] <- original_values
        current_display_data[[var]] <- original_values
        successfully_converted <- c(successfully_converted, var)
      } else {
        # Handle missing values and check if non-missing values are numeric
        non_missing_values <- as.character(current_training_data[[var]][!is.na(current_training_data[[var]])])
        if (all(grepl("^[0-9.-]+$", non_missing_values))) {
          # Convert to numeric, preserving NA
          current_training_data[[var]] <- as.numeric(as.character(current_training_data[[var]]))
          current_display_data[[var]] <- as.numeric(as.character(current_display_data[[var]]))
          successfully_converted <- c(successfully_converted, var)
        } else {
          # Add to the failed list if conversion isn't possible
          failed_conversions <- c(failed_conversions, var)
        }
      }
    }
    
    # Update datasets if any conversion happened
    if (length(successfully_converted) > 0) {
      training_data(current_training_data)
      display_data(current_display_data)
    }
    
    # Show success dialog for converted variables
    if (length(successfully_converted) > 0) {
      showModal(modalDialog(
        title = "Conversion Completed",
        paste("The following categorical variables have been converted to numerical:",
              paste(successfully_converted, collapse = ", ")),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
    
    # Show error dialog for failed conversions
    if (length(failed_conversions) > 0) {
      showModal(modalDialog(
        title = "Conversion Error",
        paste("The following variables contain non-numeric values (excluding missing values) and could not be converted to numerical:",
              paste(failed_conversions, collapse = ", ")),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
  })
  
     }
