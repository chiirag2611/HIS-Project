library(shiny)
library(shinydashboard)
library(bslib)
library(readxl)  
library(DT)
library(plotly)
library(ROSE)
library(caret)
library(smotefamily)
library(reactable)
library(shinyjs)
library(shinyjqui)
library(shinycssloaders)

server <- function(input, output, session) {
  
  ## Loading Data
  
  # Reactive data to store the uploaded data 
  training_data <- reactiveVal(NULL)
  display_data <- reactiveVal(NULL)
  y <- reactiveVal(NULL)
  y_display <- reactiveVal(NULL)
  save_variable <- reactiveVal(NULL)
  dropped_features <- reactiveVal(list())
  
  # Handle submit data button
  observeEvent(input$submit_data, {
    showNotification("Data processed successfully!", type = "message")
  })
  
  # Enable the save button functionality
  output$save_data <- downloadHandler(
    filename = function() {
      paste("processed_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(display_data(), file, row.names = FALSE)
      showNotification("Data saved successfully!", type = "message")
    }
  )
  
  # Main observer for file input
  observeEvent(input$fileInput, {
    req(input$fileInput)
    
    file_ext <- tools::file_ext(input$fileInput$name)
    
    # If it's an Excel file, handle sheet selection
    if (file_ext %in% c("xlsx", "xls")) {
      sheet_names <- excel_sheets(input$fileInput$datapath)
      
      # If there's only one sheet, load it directly
      if (length(sheet_names) == 1) {
        data <- tryCatch({
          read_excel(input$fileInput$datapath, sheet = sheet_names[1])
        }, error = function(e) {
          showNotification(paste("Error loading sheet:", e$message), type = "error")
          return(NULL)
        })
        
        if (!is.null(data)) {
          training_data(data)
          display_data(data)
          showNotification(paste("Data loaded successfully from sheet: '", sheet_names[1], "'", sep=""), type = "message")
        }
      } else {
        # Use modal dialog to select a sheet only if multiple sheets exist
        showModal(modalDialog(
          title = "Select Excel Sheet",
          selectInput("sheet_select", "Choose a sheet:", choices = sheet_names, selected = sheet_names[1]),
          footer = tagList(
            actionButton("confirm_sheet", "Load Selected Sheet"),
            modalButton("Cancel")
          )
        ))
      }
    } else {
      # For non-Excel files, load directly
      data <- load_file_data(input$fileInput)
      if (!is.null(data)) {
        training_data(data)
        display_data(data)
        showNotification("Data uploaded successfully!", type = "message")
      }
    }
  })
  
  # Function to load data based on file type
  load_file_data <- function(fileInput) {
    file_ext <- tools::file_ext(fileInput$name)
    
    if (file_ext == "csv") {
      return(read.csv(fileInput$datapath))
    } else if (file_ext %in% c("xlsx", "xls")) {
      # This function will now be used only for direct calls
      sheet_names <- excel_sheets(fileInput$datapath)
      return(read_excel(fileInput$datapath, sheet = sheet_names[1]))
    } else {
      showNotification("Unsupported file type", type = "error")
      return(NULL)
    }
  }
  
  # Handle sheet selection confirmation
  observeEvent(input$confirm_sheet, {
    req(input$fileInput, input$sheet_select)
    
    # Load data with the selected sheet
    data <- tryCatch({
      read_excel(input$fileInput$datapath, sheet = input$sheet_select)
    }, error = function(e) {
      showNotification(paste("Error loading sheet:", e$message), type = "error")
      return(NULL)
    })
    
    if (!is.null(data)) {
      training_data(data)
      display_data(data)
      showNotification(paste("Data from sheet '", input$sheet_select, "' loaded successfully!", sep=""), type = "message")
    }
    
    removeModal()  # Close the modal after selection
  })
  
  ## Dataset initial details
  
  # Observer to print file details
  observeEvent(display_data(), {
    req(display_data())  # Ensure the data is available
    
    data <- display_data()
    
    # Check if fileInput exists before accessing its properties
    if (!is.null(input$fileInput)) {
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
          "File Name:", file_name , "\n",
          "File Format:", file_ext, "\n",
          "Number of Instances:", num_instances, "\n",
          "Number of Features:", num_features, "\n",
          "Categorical Features:", num_categorical, "\n",
          "Numerical Features:", num_numerical
        )
      })
    }
  })
  
  ## Data exploration
  
  # Safe reactive accessor function to avoid errors
  safe_get_display_data <- function() {
    if (is.null(display_data())) {
      return(NULL)
    }
    return(display_data())
  }

  # Initialize column selection state
  column_selection <- reactiveVal(NULL)

  # Update column selection when data changes
  observe({
    df <- safe_get_display_data()
    if (!is.null(df)) {
      selection <- setNames(rep(TRUE, ncol(df)), names(df))
      column_selection(selection)
    }
  })

  # Generate range slider UI based on the number of columns
  output$range_slider_ui <- renderUI({
    df <- safe_get_display_data()
    req(df)
    num_cols <- ncol(df)
    sliderInput("col_range", "Column Range:", min = 1, max = num_cols, value = c(1, min(50, num_cols)), step = 1)
  })

  # Generate column selection checkboxes organized in groups
  output$column_selector_ui <- renderUI({
    df <- safe_get_display_data()
    req(df)
    col_names <- names(df)
    
    # Filter columns based on search term if any
    if (!is.null(input$search_columns) && input$search_columns != "") {
      search_term <- tolower(input$search_columns)
      col_names <- col_names[grepl(search_term, tolower(col_names))]
    }
    
    if (length(col_names) == 0) {
      return(tags$div("No columns match your search criteria"))
    }
    
    selection <- column_selection()
    
    # Create checkboxes in a more efficient way
    checkbox_list <- lapply(col_names, function(col) {
      is_numeric <- is.numeric(df[[col]])
      type_label <- if(is_numeric) "numeric" else "categorical"
      type_color <- if(is_numeric) "#007bff" else "#28a745"
      
      is_selected <- if (!is.null(selection)) selection[col] else TRUE
      
      div(
        style = "margin-bottom: 5px;",
        checkboxInput(
          inputId = paste0("col_", make.names(col)),
          label = tags$span(
            col,
            tags$span(
              style = paste0("color: ", type_color, "; margin-left: 5px; font-size: 80%;"),
              paste0("(", type_label, ")")
            )
          ),
          value = is_selected
        )
      )
    })
    
    # Return the list of checkboxes
    do.call(tagList, checkbox_list)
  })

  # Select all columns
  observeEvent(input$select_all_cols, {
    df <- safe_get_display_data()
    req(df)
    
    selection <- column_selection()
    if (!is.null(selection)) {
      selection[] <- TRUE
      column_selection(selection)
    }
  })

  # Deselect all columns
  observeEvent(input$deselect_all_cols, {
    df <- safe_get_display_data()
    req(df)
    
    selection <- column_selection()
    if (!is.null(selection)) {
      selection[] <- FALSE
      column_selection(selection)
    }
  })

  # Select numeric columns
  observeEvent(input$select_numeric_cols, {
    df <- safe_get_display_data()
    req(df)
    
    selection <- column_selection()
    if (!is.null(selection)) {
      selection[] <- FALSE
      for (col in names(df)) {
        selection[col] <- is.numeric(df[[col]])
      }
      column_selection(selection)
    }
  })

  # Select categorical columns
  observeEvent(input$select_categorical_cols, {
    df <- safe_get_display_data()
    req(df)
    
    selection <- column_selection()
    if (!is.null(selection)) {
      selection[] <- FALSE
      for (col in names(df)) {
        selection[col] <- !is.numeric(df[[col]])
      }
      column_selection(selection)
    }
  })

  # Select first N columns
  observeEvent(input$apply_first_n, {
    df <- safe_get_display_data()
    req(df, input$first_n_cols)
    
    n <- min(input$first_n_cols, ncol(df))
    selection <- column_selection()
    
    if (!is.null(selection)) {
      selection[] <- FALSE
      selection[names(df)[1:n]] <- TRUE
      column_selection(selection)
    }
  })

  # Select last N columns
  observeEvent(input$apply_last_n, {
    df <- safe_get_display_data()
    req(df, input$last_n_cols)
    
    n <- min(input$last_n_cols, ncol(df))
    total_cols <- ncol(df)
    selection <- column_selection()
    
    if (!is.null(selection)) {
      selection[] <- FALSE
      selection[names(df)[(total_cols-n+1):total_cols]] <- TRUE
      column_selection(selection)
    }
  })

  # Apply range selection
  observeEvent(input$apply_range, {
    df <- safe_get_display_data()
    req(df, input$col_range)
    
    range_start <- input$col_range[1]
    range_end <- input$col_range[2]
    selection <- column_selection()
    
    if (!is.null(selection)) {
      selection[] <- FALSE
      selection[names(df)[range_start:range_end]] <- TRUE
      column_selection(selection)
    }
  })

  # Apply column filter when button is clicked
  observeEvent(input$apply_column_filter, {
    df <- safe_get_display_data()
    req(df, training_data())
    
    # Get current selection state from checkboxes (in case they were manually changed)
    col_names <- names(df)
    selected_cols <- c()
    
    for (col in col_names) {
      checkbox_id <- paste0("col_", make.names(col))
      if (!is.null(input[[checkbox_id]]) && input[[checkbox_id]]) {
        selected_cols <- c(selected_cols, col)
      }
    }
    
    if (length(selected_cols) == 0) {
      showNotification("Please select at least one column", type = "warning")
      return()
    }
    
    # Apply the filter
    tryCatch({
      filtered_data <- training_data()[, selected_cols, drop = FALSE]
      # Update the reactive data object with filtered data
      display_data(filtered_data)
      showNotification(paste("Selected", length(selected_cols), "columns"), type = "message")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  
  # Render the dataset in the "Show Data" tab
  output$table <- renderDT({
    req(display_data())  # Ensure data is available
    datatable(display_data(), options = list(scrollX = TRUE))
  })
  
  generate_summary <- function(data) {
    summary_df <- data.frame(
      Column = names(data),
      Class = sapply(data, function(x) if (is.numeric(x)) "Numerical" else "Categorical"),
      Missing = sapply(data, function(x) {
        # Handle different data types for missing value detection
        if (is.factor(x) || is.character(x)) {
          sum(is.na(x) | x == "" | x == "NA", na.rm = TRUE)
        } else {
          sum(is.na(x), na.rm = TRUE)
        }
      }),
      Missing_Percent = sapply(data, function(x) {
        missing_count <- if (is.factor(x) || is.character(x)) {
          sum(is.na(x) | x == "" | x == "NA", na.rm = TRUE)
        } else {
          sum(is.na(x), na.rm = TRUE)
        }
        sprintf("%.1f%%", (missing_count / length(x)) * 100)
      }),
      Min = sapply(data, function(x) if (is.numeric(x)) min(x, na.rm = TRUE) else "-"),
      Median = sapply(data, function(x) if (is.numeric(x)) median(x, na.rm = TRUE) else "-"),
      Max = sapply(data, function(x) if (is.numeric(x)) max(x, na.rm = TRUE) else "-"),
      Mean = sapply(data, function(x) if (is.numeric(x)) sprintf("%.3f", mean(x, na.rm = TRUE)) else "-"),
      SD = sapply(data, function(x) if (is.numeric(x)) sprintf("%.3f", sd(x, na.rm = TRUE)) else "-"),
      Variance = sapply(data, function(x) if (is.numeric(x)) sprintf("%.3f", var(x, na.rm = TRUE)) else "-"),
      Unique_Values = sapply(data, function(x) if (!is.numeric(x)) length(unique(x)) else "-")
    )
    
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
      choices = colnames(display_data()),
      multiple = TRUE 
    )
  })
  
  # Observe the Apply Drop button
  observeEvent(input$apply_drop, {
    req(input$drop_feature)  # Ensure at least one feature is selected
    req(training_data())     # Ensure data is available
    
    # Get the selected features to drop
    selected_features <- input$drop_feature
    
    # Store the dropped features and their data
    current_dropped <- dropped_features()
    for (feature in selected_features) {
      current_dropped[[feature]] <- training_data()[[feature]]
    }
    dropped_features(current_dropped)
    
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
  
  ## Add Feature
  
  # Dynamic UI for selecting features to add
  output$add_feature_ui <- renderUI({
    # Get the names of dropped features
    feature_names <- names(dropped_features())
    
    # If no dropped features, show a message
    if (length(feature_names) == 0) {
      return(div("No features available to add back."))
    }
    selectInput(
      inputId = "add_feature",
      label = "Select Feature(s) to Add",
      choices = feature_names,
      multiple = TRUE
    )
  })
  
  # Observe the Apply Add button
  observeEvent(input$apply_add, {
    req(input$add_feature)  # Ensure at least one feature is selected
    req(training_data())    # Ensure data is available
    req(dropped_features()) # Ensure dropped features are stored
    
    # Get the selected features to add back
    selected_features <- input$add_feature
    
    # Update training_data by adding back the selected features
    current_training_data <- training_data()
    current_display_data <- display_data()
    current_dropped <- dropped_features()
    
    for (feature in selected_features) {
      # Add the feature back to the datasets
      current_training_data[[feature]] <- current_dropped[[feature]]
      current_display_data[[feature]] <- current_dropped[[feature]]
      # Remove from the dropped features list
      current_dropped[[feature]] <- NULL
    }
    
    # Update the reactive values
    training_data(current_training_data)
    display_data(current_display_data)
    dropped_features(current_dropped)
    
    # Provide feedback to the user
    showNotification(
      paste("Added feature(s) back:", paste(selected_features, collapse = ", ")),
      type = "message"
    )
  })
  
  ## Switch Categorical and Numerical data
  
  # Dynamically update dropdown options
  observe({
    req(display_data())
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
        current_display_data[[var]] <- attr(current_display_data[[var]], "original_values")
        successfully_converted <- c(successfully_converted, var)
      } else {
        # Handle missing values and check if non-missing values are numeric
        non_missing_values <- as.character(current_training_data[[var]][!is.na(current_training_data[[var]])])
        if (all(grepl("^\\s*-?[0-9]*(\\.[0-9]+)?\\s*$", non_missing_values))) {
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
  
  ## Pre Processing
  
  ## Handling Missing values
  
  output$missing_percent <- renderText({
    req(input$missing_var, display_data())  # Ensure variable selection and data availability
    var <- display_data()[[input$missing_var]]  # Extract the selected variable's column
    percent <- sum(is.na(var)) / length(var) * 100  # Compute the percentage of missing values
    paste("Missing Percent:", round(percent, 2), "%")  # Format and return the result as text
  })
  
  current_missing_var <- reactiveVal(NULL)  # Holds the currently selected variable for missing value handling
  
  # UI for selecting a variable to handle missing values.
  output$missing_var_ui <- renderUI({
    req(display_data())  # Ensure displayed data is available
    variables <- names(display_data())  # Get the column names of the displayed data
    
    selectInput(
      inputId = "missing_var", 
      label = "Select Variable to Handle Missing Values:", 
      choices = variables, 
      selected = NULL
    )
  })
  
  # Missing value handling logic
  output$missing_method_ui <- renderUI({
    selectInput(
      inputId = "missing_method",
      label = "Select Method to Handle Missing Values:",
      choices = c("Row Deletion", "Handle using Mode", "Handle using Median", "Handle using Mean"),
      selected = "Row Deletion"
    )
  })
  
  observeEvent(input$apply_missing, {
    tryCatch({
      req(input$missing_var, input$missing_method, display_data(), training_data())
      
      displayed <- display_data()
      train <- training_data()
      var <- input$missing_var
      missing_count <- sum(is.na(displayed[[var]]))
      
      if (missing_count > 0) {
        if (input$missing_method == "Row Deletion") {
          rows_to_keep <- !is.na(displayed[[var]])
          displayed <- displayed[rows_to_keep, ]
          train <- train[rows_to_keep, ]
          
        } else if (input$missing_method == "Handle using Mode") {
          mode_val <- as.numeric(names(sort(table(displayed[[var]]), decreasing = TRUE)[1]))
          displayed[[var]][is.na(displayed[[var]])] <- mode_val
          train[[var]][is.na(train[[var]])] <- mode_val
          
        } else if (input$missing_method == "Handle using Median") {
          median_val <- median(displayed[[var]], na.rm = TRUE)
          displayed[[var]][is.na(displayed[[var]])] <- median_val
          train[[var]][is.na(train[[var]])] <- median_val
          
        } else if (input$missing_method == "Handle using Mean") {
          mean_val <- mean(displayed[[var]], na.rm = TRUE)
          displayed[[var]][is.na(displayed[[var]])] <- mean_val
          train[[var]][is.na(train[[var]])] <- mean_val
        }
        
        display_data(displayed)
        training_data(train)
        
        showModal(
          modalDialog(
            title = "Missing Values Handled",
            paste("The variable", var, "had", missing_count, "missing values."),
            if (input$missing_method == "Row Deletion") {
              paste(missing_count, "rows were removed.")
            } else {
              "The missing values have been replaced."
            },
            easyClose = TRUE,
            footer = modalButton("Close")
          )
        )
      } else {
        showNotification("No missing values in the selected variable.", type = "warning")
      }
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Dynamic selection for outliers
  output$outlier_var_ui <- renderUI({
    req(display_data())
    numeric_vars <- names(display_data())[sapply(display_data(), is.numeric)]
    
    if (length(numeric_vars) > 0) {
      selectInput(
        inputId = "outlier_var", 
        label = "Select Numerical Variable:", 
        choices = numeric_vars, 
        selected = NULL
      )
    } else {
      tags$p("No numerical variables available for outlier handling.", style = "color: red;")
    }
  })
  
  observeEvent(input$apply_outliers, {
    req(input$outlier_var, input$outlier_method, display_data(), training_data())
    
    # Retrieve both datasets
    displayed <- display_data()
    training <- training_data()
    var <- input$outlier_var
    
    if (is.numeric(displayed[[var]])) {
      # Detect outliers using the IQR method
      iqr <- IQR(displayed[[var]], na.rm = TRUE)
      q1 <- quantile(displayed[[var]], 0.25, na.rm = TRUE)
      q3 <- quantile(displayed[[var]], 0.75, na.rm = TRUE)
      lower_bound <- q1 - 1.5 * iqr
      upper_bound <- q3 + 1.5 * iqr
      
      # Identify outliers
      outliers <- which(displayed[[var]] < lower_bound | displayed[[var]] > upper_bound)
      outlier_count <- length(outliers)
      
      if (outlier_count > 0) {
        if (input$outlier_method == "Remove Outliers") {
          # Remove outliers from both datasets
          displayed <- displayed[-outliers, ]
          training <- training[-outliers, ]
        } else if (input$outlier_method == "Replace with Median") {
          # Replace outliers with the median in both datasets
          median_val <- median(displayed[[var]], na.rm = TRUE)
          displayed[[var]][outliers] <- median_val
          training[[var]][outliers] <- median_val
        } else if (input$outlier_method == "Replace with Mean") {
          # Replace outliers with the mean in both datasets
          mean_val <- mean(displayed[[var]], na.rm = TRUE)
          displayed[[var]][outliers] <- mean_val
          training[[var]][outliers] <- mean_val
        }
        
        # Update the reactive datasets
        display_data(displayed)
        training_data(training)
        
        # Show a modal message summarizing the changes
        showModal(
          modalDialog(
            title = "Outliers Handled",
            paste("The variable", var, "had", outlier_count, "outliers detected."),
            if (input$outlier_method == "Remove Outliers") {
              paste(outlier_count, "rows were removed.")
            } else {
              "The outliers have been replaced."
            },
            easyClose = TRUE,
            footer = modalButton("Close")
          )
        )
      } else {
        showNotification("No outliers detected in the selected variable.", type = "warning")
      }
    } else {
      showNotification("Selected variable is not numeric.", type = "error")
    }
  })
  ## Data Transformation
  
  # Dynamically selecting Data Transformation
  output$transform_var_ui <- renderUI({
    req(display_data())
    
    data <- display_data()
    # Dynamically identify numerical columns excluding derived columns
    numeric_vars <- names(data)[sapply(data, is.numeric)]
    
    selectizeInput(
      inputId = "transform_var",
      label = "Select Numerical Variables for Transformation:",
      choices = numeric_vars,
      selected = NULL, # No variable selected by default
      multiple = TRUE
    )
  })
  
  observeEvent(input$apply_transformation, {
    req(training_data())
    req(input$transform_var)
    req(input$transformation_method)
    
    # Get the training data and selected variables
    data <- training_data()
    selected_vars <- input$transform_var
    transformed_vars <- list()
    
    for (var in selected_vars) {
      if (input$transformation_method == "Min-Max Scaling") {
        data[[var]] <- (data[[var]] - min(data[[var]], na.rm = TRUE)) / 
          (max(data[[var]], na.rm = TRUE) - min(data[[var]], na.rm = TRUE))
        transformed_vars[[var]] <- "Min-Max Scaling"
      } else if (input$transformation_method == "Z-Score Normalization") {
        data[[var]] <- scale(data[[var]], center = TRUE, scale = TRUE)
        transformed_vars[[var]] <- "Z-Score Normalization"
      } else if (input$transformation_method == "Log Transformation") {
        data[[var]] <- log(data[[var]] + 1) # Adding 1 to handle zero values
        transformed_vars[[var]] <- "Log Transformation"
      }
    }
    
    # Update the dataset with the transformations
    training_data(data) 
    
    # Show notification
    showNotification("Transformation applied successfully!", type = "message")
    
    # Prepare the transformation message
    if (length(transformed_vars) == 1) {
      var_list <- paste(names(transformed_vars), collapse = ", ")
      transformation_msg <- paste(input$transformation_method, "applied in the following variable:", var_list)
    } else {
      var_list <- paste(names(transformed_vars), collapse = ", ")
      transformation_msg <- paste(input$transformation_method, "applied in the following variables:", var_list)
    }
    
    # Show modal dialog with applied transformations
    showModal(
      modalDialog(
        title = "Transformation Applied",
        transformation_msg,
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  })
  
  ## Data Encoding 
  
  # Data Encoding Variable Selection
  output$encoding_var_ui <- renderUI({
    req(display_data())
    
    data <- display_data()
    # Dynamically identify categorical columns in the current dataset
    categorical_vars <- names(data)[sapply(data, function(col) is.factor(col) || is.character(col))]
    
    selectizeInput(
      inputId = "encoding_var",
      label = "Select Categorical Variables for Encoding:",
      choices = categorical_vars,
      selected = NULL, # No variable selected by default
      multiple = TRUE
    )
  })
  
  # Observer for applying encoding
  observeEvent(input$apply_encoding, {
    req(training_data())
    req(input$encoding_var)
    req(input$encoding_method)
    
    # Get the training data and selected variables
    data <- training_data()
    selected_vars <- input$encoding_var
    encoded_vars <- list()
    
    for (var in selected_vars) {
      if (input$encoding_method == "Label Encoding") {
        data[[var]] <- as.integer(as.factor(data[[var]]))
        encoded_vars[[var]] <- "Label Encoding"
      } else if (input$encoding_method == "One-Hot Encoding") {
        one_hot <- model.matrix(~ . - 1, data.frame(data[[var]]))
        colnames(one_hot) <- paste(var, colnames(one_hot), sep = "_")
        data <- cbind(data, one_hot)
        data[[var]] <- NULL
        encoded_vars[[var]] <- "One-Hot Encoding"
      }
    }
    
    # Update the dataset with the encoding applied
    training_data(data)
    
    # Show notification
    showNotification("Encoding applied successfully!", type = "message")
    
    # Prepare the encoding message
    if (length(encoded_vars) == 1) {
      var_list <- paste(names(encoded_vars), collapse = ", ")
      encoding_msg <- paste(encoded_vars[[1]], "applied in the following variable:", var_list)
    } else {
      var_list <- paste(names(encoded_vars), collapse = ", ")
      encoding_msg <- paste(encoded_vars[[1]], "applied in the following variables:", var_list)
    }
    
    # Show modal dialog with encoded variables
    showModal(
      modalDialog(
        title = "Encoding Applied",
        encoding_msg,
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  })
  
  ## Submit button 
  
  observeEvent(input$submit_data, {
    # Retrieve the current state of training_data and display_data
    req(training_data(), display_data())  # Ensure both datasets are available
    training <- training_data()
    display <- display_data()
    
    # Initialize messages for dialog box
    alert_messages <- c()  # For alerts
    warning_message <- "⚠️ Once you submit, further preprocessing will be locked and cannot be changed."
    
    # Check for categorical variables in training_data
    if (any(sapply(training, function(col) is.factor(col) || is.character(col)))) {
      alert_messages <- c(
        alert_messages, 
        "❗ The training data contains categorical variables. Label encoding will be automatically applied to these variables before training."
      )
    }
    
    # Check if the features in training_data differ from those in display_data
    if (ncol(training) != ncol(display)) {
      alert_messages <- c(
        alert_messages,
        "❗ The interface cannot handle a target variable that has been one-hot encoded. Please ensure the target variable is not encoded in this way."
      )
    }
    
    # Check for missing values in training_data
    if (any(is.na(training))) {
      alert_messages <- c(
        alert_messages,
        "❗ The training data contains missing values. If you don't handle them, all rows with null values will be removed automatically before training."
      )
    }
    
    # Prepare dialog box content
    dialog_content <- tagList(
      tags$h4("Alerts:", style = "color: red;"),  # Header for alert messages
      lapply(alert_messages, function(msg) tags$p(msg)),
      tags$hr(),
      tags$h4("Warning:", style = "color: orange;"),  # Header for the warning message
      tags$p(warning_message)
    )
    
    # Show modal dialog with confirm and cancel options
    showModal(
      modalDialog(
        title = "Submission Confirmation",
        dialog_content,
        easyClose = FALSE,  # Prevent closing without user action
        footer = tagList(
          actionButton("confirm_submit", "Confirm", icon = icon("check-circle")),
          modalButton("Cancel", icon = icon("times-circle"))
        )
      )
    )
  })
  
  
  # Handle Confirm and Cancel actions
  observeEvent(input$confirm_submit, {
    # Disable all preprocessing components
    shinyjs::disable("missing_values_section")
    shinyjs::disable("outliers_section")
    shinyjs::disable("transformation_section")
    shinyjs::disable("encoding_section")
    shinyjs::disable("submit_button")
    
    # Proceed to finalize the submission
    complete_submission()
    
    # Close the modal dialog
    removeModal()
  })
  
  observeEvent(input$cancel, {
    # Close the modal dialog without taking further action
    removeModal()
  })
  
  
  # Observe Confirm button click
  observeEvent(input$confirm_submit, {
    # Proceed with submission
    complete_submission()
  })
  
  # Define a function to handle submission
  complete_submission <- function() {
    # Disable all components
    shinyjs::disable("missing_var")
    shinyjs::disable("missing_method")
    shinyjs::disable("apply_missing")
    shinyjs::disable("outlier_var")
    shinyjs::disable("outlier_method")
    shinyjs::disable("apply_outliers")
    shinyjs::disable("transform_var")
    shinyjs::disable("transformation_method")
    shinyjs::disable("apply_transformation")
    shinyjs::disable("encoding_var")
    shinyjs::disable("encoding_method")
    shinyjs::disable("apply_encoding")
    shinyjs::disable("submit_data")
    save_variable("confirmed")
  }
  
  ## Save Button 
  
  # Enable the Save button only when Submit is clicked
  observe({
    toggleState("save_data", !is.null(save_variable()))
  })
  
  # Save data as CSV on Save button click
  output$save_data <- downloadHandler(
    filename = function() {
      paste("training_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(training_data())  # Ensure data exists
      write.csv(training_data(), file, row.names = FALSE)
    }
  )
  ## Show training data 
  
  # Render the training Dataset
  output$table_training <- renderDT({
    req(display_data())  # Ensure data is available
    datatable(training_data(), options = list(scrollX = TRUE))
  })
} # End of server function