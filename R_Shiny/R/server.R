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
library(missForest)
library(mice)
library(shinyBS)

server <- function(input, output, session) {
  
  ## Loading Data
  
  # Track which operation triggered the column selection
  current_operation <- reactiveVal(NULL)
  
  # Store selected columns from modal
  modal_selected_cols <- reactiveVal(character(0))
  
  # Store previous selections to restore on cancel
  previous_selected_cols <- reactiveVal(character(0))
  
  # Track if modal is currently open
  modal_is_open <- reactiveVal(FALSE)
  
  # Reactive data to store the uploaded data 
  training_data <- reactiveVal(NULL)
  display_data <- reactiveVal(NULL)
  save_variable <- reactiveVal(NULL)
  dropped_features <- reactiveVal(list())
  
  # Create an empty reactiveValues object to store preprocessing operations
  preprocessing_ops <- reactiveValues(
    missing_values = NULL,
    outliers = NULL,
    transformation = NULL,
    encoding = NULL
  )
  
  # Helper function to update all checkbox states
  update_checkbox_states <- function() {
    req(display_data())
    df <- display_data()
    selected_cols <- modal_selected_cols()
    
    # Handle the case where no columns are selected
    if (length(selected_cols) == 0) {
      for (col in names(df)) {
        checkbox_id <- paste0("modal_col_", make.names(col))
        updateCheckboxInput(session, checkbox_id, value = FALSE)
      }
      return()
    }
    
    # Normal case with selections
    for (col in names(df)) {
      checkbox_id <- paste0("modal_col_", make.names(col))
      is_selected <- col %in% selected_cols
      updateCheckboxInput(session, checkbox_id, value = is_selected)
    }
  }
  
  # Observe when column selection is triggered
  observeEvent(input$column_selection_trigger, {
    current_operation(input$column_selection_trigger)
  
    # Save current selections based on the current operation
    if (current_operation() == "missing") {
      previous_selected_cols(input$missing_var)
      modal_selected_cols(input$missing_var)
    } else if (current_operation() == "outliers") {
      previous_selected_cols(input$outlier_var)
      modal_selected_cols(input$outlier_var)
    } else if (current_operation() == "transformation") {
      previous_selected_cols(input$transform_var)
      modal_selected_cols(input$transform_var)
    } else if (current_operation() == "encoding") {
      previous_selected_cols(input$encoding_var)
      modal_selected_cols(input$encoding_var)
    } else {
      previous_selected_cols(character(0))
      modal_selected_cols(character(0))  # Default to empty
    }
    
    # Set modal state to open
    modal_is_open(TRUE)
    
    # Open modal and initialize checkbox states after it's rendered
    toggleModal(session, "columnSelectionModal", toggle = "open")
    
    # We need a slight delay to allow the modal UI to render before updating checkboxes
    shinyjs::delay(100, {
      update_checkbox_states()
    })
  })
  
  # Modal column selector UI
  output$modal_column_selector <- renderUI({
    req(display_data())
    df <- display_data()
    col_names <- names(df)
    
    # Filter columns based on search term if any
    if (!is.null(input$modal_search_columns) && input$modal_search_columns != "") {
      search_term <- tolower(input$modal_search_columns)
      col_names <- col_names[grepl(search_term, tolower(col_names))]
    }
    
    if (length(col_names) == 0) {
      return(tags$div("No columns match your search criteria"))
    }
    
    # Get current selection state
    current_selections <- modal_selected_cols()
    
    # Create checkboxes
    checkbox_list <- lapply(col_names, function(col) {
      is_numeric <- is.numeric(df[[col]])
      type_label <- if(is_numeric) "numeric" else "categorical"
      type_color <- if(is_numeric) "#007bff" else "#28a745"
      
      is_selected <- col %in% current_selections
      
      div(
        style = "margin-bottom: 5px;",
        checkboxInput(
          inputId = paste0("modal_col_", make.names(col)),
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
    
    do.call(tagList, checkbox_list)
  })
    # Observer for individual checkbox changes in the modal
  observe({
    req(display_data())
    df <- display_data()
    
    # Only run if modal is open
    if (!modal_is_open()) return()
    
    # Collect all selected columns from checkboxes
    selected <- c()
    for (col in names(df)) {
      checkbox_id <- paste0("modal_col_", make.names(col))
      if (!is.null(input[[checkbox_id]]) && input[[checkbox_id]]) {
        selected <- c(selected, col)
      }
    }
    
    # Debug message for tracking selections
    if (length(selected) > 0) {
      message("Checkbox selections updated: ", paste(selected, collapse=", "))
    }
    
    # Update the reactive value with current selections
    modal_selected_cols(selected)
  })
  # Handle modal confirmation
  observeEvent(input$modal_confirm, {
    req(current_operation())
    
    # Get all selected columns from checkboxes
    df <- display_data()
    selected_cols <- c()
    
    # If we've explicitly deselected all, ensure the list is empty
    if (length(modal_selected_cols()) == 0) {
      selected_cols <- character(0)
    } else {
      # Otherwise, gather selected columns from checkboxes
      for (col in names(df)) {
        checkbox_id <- paste0("modal_col_", make.names(col))
        if (!is.null(input[[checkbox_id]]) && input[[checkbox_id]]) {
          selected_cols <- c(selected_cols, col)
        }
      }
    }
    
    # Save the confirmed selection
    modal_selected_cols(selected_cols)
    
    # Set modal state to closed
    modal_is_open(FALSE)
    
    # Debug message
    message("Selected columns confirmed: ", paste(selected_cols, collapse=", "))
    
    # Update the appropriate input based on current operation
    if (current_operation() == "missing") {
      updateSelectInput(session, "missing_var", selected = selected_cols)
    } 
    else if (current_operation() == "outliers") {
      updateSelectInput(session, "outlier_var", selected = selected_cols)
    }
    else if (current_operation() == "transformation") {
      updateSelectInput(session, "transform_var", selected = selected_cols)
    }
    else if (current_operation() == "encoding") {
      updateSelectInput(session, "encoding_var", selected = selected_cols)
    }
    
    # Show notification
    if (length(selected_cols) > 0) {
      showNotification(paste("Selected", length(selected_cols), "columns"), type = "message")
    }
    
    toggleModal(session, "columnSelectionModal", toggle = "close")
  })
  # Modal quick actions
  observeEvent(input$modal_select_all, {
    df <- display_data()
    col_names <- names(df)
    modal_selected_cols(col_names)
    update_checkbox_states()
  })
  
  observeEvent(input$modal_deselect_all, {
    # Clear the selections completely
    modal_selected_cols(character(0))
    
    # Update all checkbox states to deselected
    df <- display_data()
    for (col in names(df)) {
      checkbox_id <- paste0("modal_col_", make.names(col))
      updateCheckboxInput(session, checkbox_id, value = FALSE)
    }
  })
  
  observeEvent(input$modal_select_numeric, {
    df <- display_data()
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    modal_selected_cols(numeric_cols)
    update_checkbox_states()
  })
  
  observeEvent(input$modal_select_categorical, {
    df <- display_data()
    cat_cols <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    modal_selected_cols(cat_cols)
    update_checkbox_states()
  })
  
  # Observer for search field changes - forces UI update
  observeEvent(input$modal_search_columns, {
    # This will trigger a re-render of the modal_column_selector UI
    # with the updated search filter
  })
  # Handle modal cancel button
  observeEvent(input$modal_cancel, {
    # Restore previous selections to what they were before opening the modal
    modal_selected_cols(previous_selected_cols())
    
    # Set modal state to closed
    modal_is_open(FALSE)
    
    # Make sure any UI inputs get restored to their previous state
    if (current_operation() == "missing") {
      updateSelectInput(session, "missing_var", selected = previous_selected_cols())
    } 
    else if (current_operation() == "outliers") {
      updateSelectInput(session, "outlier_var", selected = previous_selected_cols())
    }
    else if (current_operation() == "transformation") {
      updateSelectInput(session, "transform_var", selected = previous_selected_cols())
    }
    else if (current_operation() == "encoding") {
      updateSelectInput(session, "encoding_var", selected = previous_selected_cols())
    }
    
    # Close the modal without updating any selections
    toggleModal(session, "columnSelectionModal", toggle = "close")
  })
  
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
      tryCatch({
        sheet_names <- excel_sheets(input$fileInput$datapath)
        
        if (length(sheet_names) == 0) {
          showNotification("Excel file contains no sheets", type = "error")
          return()
        }
        
        # If there's only one sheet, load it directly
        if (length(sheet_names) == 1) {
          data <- tryCatch({
            read_excel(input$fileInput$datapath, sheet = sheet_names[1])
          }, error = function(e) {
            showNotification(paste("Error loading sheet:", e$message), type = "error")
            return(NULL)
          })
          
          if (is.null(data) || ncol(data) == 0) {
            showNotification("No data found in Excel sheet", type = "error")
            return()
          }
          
          if (!is.null(data)) {
            training_data(data)
            display_data(data)
            showNotification(paste("Data loaded successfully from sheet: '", sheet_names[1], "'", sep=""), type = "message")
            
            # Reset operation-applied indicators
            removeClass(id = "missing_var_ui", class = "operation-applied")
            removeClass(id = "outlier_var_ui", class = "operation-applied")
            removeClass(id = "transform_var_ui", class = "operation-applied")
            removeClass(id = "encoding_var_ui", class = "operation-applied")
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
      }, error = function(e) {
        showNotification(paste("Error loading Excel file:", e$message), type = "error")
      })
    } else {
      # For non-Excel files, load directly
      data <- load_file_data(input$fileInput)
      if (!is.null(data)) {
        training_data(data)
        display_data(data)
        showNotification("Data uploaded successfully!", type = "message")
        
        # Reset operation-applied indicators
        removeClass(id = "missing_var_ui", class = "operation-applied")
        removeClass(id = "outlier_var_ui", class = "operation-applied")
        removeClass(id = "transform_var_ui", class = "operation-applied")
        removeClass(id = "encoding_var_ui", class = "operation-applied")
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
      
      # Reset operation-applied indicators
      removeClass(id = "missing_var_ui", class = "operation-applied")
      removeClass(id = "outlier_var_ui", class = "operation-applied")
      removeClass(id = "transform_var_ui", class = "operation-applied")
      removeClass(id = "encoding_var_ui", class = "operation-applied")
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
    
    # Validation
    if (ncol(training_data()) <= 2) {
      showNotification("Cannot drop features: minimum of 2 columns required", type = "error")
      return()
    }
    
    # Get the selected features to drop
    selected_features <- input$drop_feature
    
    # Validate selected features exist in data
    if (!all(selected_features %in% names(training_data()))) {
      showNotification("Some selected features do not exist in the data", type = "error")
      return()
    }
    
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
          # Identify which values can't be converted
          problematic_values <- non_missing_values[!grepl("^\\s*-?[0-9]*(\\.[0-9]+)?\\s*$", non_missing_values)]
          problematic_sample <- paste(head(unique(problematic_values), 5), collapse=", ")
          showNotification(paste("Cannot convert", var, "to numeric. Examples of non-numeric values:", problematic_sample), type = "error")
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
    
    if (length(input$missing_var) == 1) {
      var <- display_data()[[input$missing_var]]  # Extract the selected variable's column
      percent <- sum(is.na(var)) / length(var) * 100  # Compute the percentage of missing values
      paste("Missing Percent:", round(percent, 2), "%")  # Format and return the result as text
    } else if (length(input$missing_var) > 1) {
      # For multiple variables, show aggregate stats
      data <- display_data()
      total_cells <- length(input$missing_var) * nrow(data)
      missing_count <- 0
      
      for (var_name in input$missing_var) {
        missing_count <- missing_count + sum(is.na(data[[var_name]]))
      }
      
      percent <- missing_count / total_cells * 100
      paste("Missing Percent (across selected variables):", round(percent, 2), "%")
    } else {
      "Select at least one variable to see missing data percentage."
    }
  })
  
  # UI for selecting a variable to handle missing values
  output$missing_var_ui <- renderUI({
    req(display_data())  # Ensure displayed data is available
    variables <- names(display_data())  # Get the column names of the displayed data
    
    selectizeInput(
      inputId = "missing_var",
      label = "Select Variable(s) with Missing Values:",
      choices = variables,
      selected = NULL,
      multiple = TRUE
    )
  })
  
  # Missing value handling method selection based on variable type
  output$missing_method_ui <- renderUI({
    req(input$missing_var, display_data())
    data <- display_data()
    
    # Check if we have multiple variables selected
    if (length(input$missing_var) > 1) {
      # If multiple variables are selected, determine if they're all the same type
      all_categorical <- all(sapply(input$missing_var, function(var) {
        is.factor(data[[var]]) || is.character(data[[var]])
      }))
      
      all_numeric <- all(sapply(input$missing_var, function(var) {
        is.numeric(data[[var]])
      }))
      
      if (all_categorical) {
        # For all categorical variables
        selectInput(
          inputId = "missing_method",
          label = "Select Method to Handle Missing Values:",
          choices = c("Row Deletion", "Handle using Mode", "Create Missing Category"),
          selected = "Handle using Mode"
        )
      } else if (all_numeric) {
        # For all numerical variables
        selectInput(
          inputId = "missing_method",
          label = "Select Method to Handle Missing Values:",
          choices = c("Row Deletion", "Handle using Mode", "Handle using Median", "Handle using Mean"),
          selected = "Handle using Mean"
        )
      } else {
        # Mixed variable types
        selectInput(
          inputId = "missing_method",
          label = "Select Method to Handle Missing Values:",
          choices = c("Row Deletion", "Handle using Mode"),
          selected = "Handle using Mode"
        )
      }
    } else if (length(input$missing_var) == 1) {
      # Single variable selected
      var <- input$missing_var
      if (is.factor(data[[var]]) || is.character(data[[var]])) {
        # For categorical variables
        selectInput(
          inputId = "missing_method",
          label = "Select Method to Handle Missing Values:",
          choices = c("Row Deletion", "Handle using Mode", "Create Missing Category"),
          selected = "Handle using Mode"
        )
      } else {
        # For numerical variables
        selectInput(
          inputId = "missing_method",
          label = "Select Method to Handle Missing Values:",
          choices = c("Row Deletion", "Handle using Mode", "Handle using Median", "Handle using Mean"),
          selected = "Handle using Mean"
        )
      }
    } else {
      # No variables selected
      selectInput(
        inputId = "missing_method",
        label = "Select Method to Handle Missing Values:",
        choices = c("Row Deletion", "Handle using Mode", "Handle using Median", "Handle using Mean", "Create Missing Category"),
        selected = "Handle using Mode"
      )
    }
  })
  
  # Missing value handling implementation
  observeEvent(input$apply_missing, {
    req(input$missing_var, input$missing_method, display_data(), training_data())
    
    # Initialize variables for summary
    total_processed <- 0
    total_missing_count <- 0
    processed_vars <- c()
    
    # Process each selected variable
    for (var in input$missing_var) {
      tryCatch({
        displayed <- display_data()
        train <- training_data()
        
        # Skip if the variable doesn't exist
        if (!var %in% names(displayed)) {
          showNotification(paste("Variable", var, "not found in dataset"), type = "error")
          next
        }
        
        # Count missing values, considering various representations of missingness
        missing_count <- sum(is.na(displayed[[var]]) | 
                            displayed[[var]] == "" | 
                            displayed[[var]] == "NA" |
                            displayed[[var]] == "NULL")
        
      # Add visual indicator that operation was applied
    addClass(id = "missing_var_ui", class = "operation-applied")
    
    # Update preprocessing summary
    preprocessing_ops$missing_values <- list(
      method = input$missing_method,
      variables = paste(input$missing_var, collapse = ", "),
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
        
        if (missing_count > 0) {
          # Determine if the variable is categorical or numerical
          is_categorical <- is.factor(displayed[[var]]) || is.character(displayed[[var]])
          
          if (input$missing_method == "Row Deletion") {
            # Remove rows with missing values in the selected variable
            rows_to_keep <- !is.na(displayed[[var]])
            displayed <- displayed[rows_to_keep, ]
            train <- train[rows_to_keep, ]
            
          } else if (input$missing_method == "Handle using Mode") {
            # Mode imputation works for both categorical and numerical data
            if (is_categorical) {
              # For categorical data, get the most frequent category
              mode_val <- names(sort(table(displayed[[var]], useNA = "no"), decreasing = TRUE)[1])
              displayed[[var]][is.na(displayed[[var]])] <- mode_val
              train[[var]][is.na(train[[var]])] <- mode_val
            } else {
              # For numerical data
              mode_val <- as.numeric(names(sort(table(displayed[[var]], useNA = "no"), decreasing = TRUE)[1]))
              displayed[[var]][is.na(displayed[[var]])] <- mode_val
              train[[var]][is.na(train[[var]])] <- mode_val
            }
            
          } else if (input$missing_method == "Create Missing Category" && is_categorical) {
            # Create a new "Missing" category for categorical data
            displayed[[var]] <- as.character(displayed[[var]])
            train[[var]] <- as.character(train[[var]])
            
            displayed[[var]][is.na(displayed[[var]])] <- "Missing"
            train[[var]][is.na(train[[var]])] <- "Missing"
            
            # Convert back to factor if original was factor
            if (is.factor(displayed[[var]])) {
              displayed[[var]] <- as.factor(displayed[[var]])
              train[[var]] <- as.factor(train[[var]])
            }
            
          } else if (input$missing_method == "Handle using Median" && !is_categorical) {
            # Median imputation (numerical only)
            median_val <- median(displayed[[var]], na.rm = TRUE)
            displayed[[var]][is.na(displayed[[var]])] <- median_val
            train[[var]][is.na(train[[var]])] <- median_val
            
          } else if (input$missing_method == "Handle using Mean" && !is_categorical) {
            # Mean imputation (numerical only)
            mean_val <- mean(displayed[[var]], na.rm = TRUE)
            displayed[[var]][is.na(displayed[[var]])] <- mean_val
            train[[var]][is.na(train[[var]])] <- mean_val
          }
          
          # Update tracking variables
          total_processed <- total_processed + 1
          total_missing_count <- total_missing_count + missing_count
          processed_vars <- c(processed_vars, var)
          
          # Update both datasets with the changes
          #display_data(displayed)
          #training_data(train)
          update_datasets(displayed, train, session)
        } else {
          showNotification(paste("No missing values found in", var), type = "warning")
        }
      }, error = function(e) {
        showNotification(paste("Error handling missing values in", var, ":", e$message), type = "error")
      })
    }
    
    # Show summary confirmation dialog if any variables were processed
    if (total_processed > 0) {
      showModal(
        modalDialog(
          title = "Missing Values Handled",
          paste(total_processed, "variable(s) processed with a total of", total_missing_count, "missing values."),
          tags$p(paste("Processed variables:", paste(processed_vars, collapse = ", "))),
          if (input$missing_method == "Row Deletion") {
            "Rows with missing values were removed."
          } else {
            paste("Missing values have been replaced using the", 
                  tolower(gsub("Handle using ", "", input$missing_method)), "method.")
          },
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    } else {
      showNotification("No variables were processed.", type = "warning")
    }
  })
  
  # Dynamic selection for outliers
  output$outlier_var_ui <- renderUI({
    req(display_data())
    numeric_vars <- names(display_data())[sapply(display_data(), is.numeric)]
    
    if (length(numeric_vars) > 0) {
      selectizeInput(
        inputId = "outlier_var", 
        label = "Select Numerical Variable(s):", 
        choices = numeric_vars, 
        selected = NULL,
        multiple = TRUE
      )
    } else {
      tags$p("No numerical variables available for outlier handling.", style = "color: red;")
    }
  })
  
  # Create a function to ensure atomic updates
  update_datasets <- function(new_display, new_training, session) {
    tryCatch({
      display_data(new_display)
      training_data(new_training)
      # Log the update for debugging
      message(sprintf("Datasets updated: %d rows, %d columns", 
                    nrow(new_display), ncol(new_display)))
    }, error = function(e) {
      showNotification(paste("Error updating datasets:", e$message), type = "error")
    })
  }

  # Then use it in outlier handling
  observeEvent(input$apply_outliers, {
    req(input$outlier_var, input$outlier_method, display_data(), training_data())
    
    # Initialize variables for summary
    total_processed <- 0
    total_outliers <- 0
    processed_vars <- c()
    rows_removed <- 0
    
    # Add visual indicator that operation was applied
    addClass(id = "outlier_var_ui", class = "operation-applied")
    
    # Update preprocessing summary
    preprocessing_ops$outliers <- list(
      method = input$outlier_method,
      variables = paste(input$outlier_var, collapse = ", "),
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
    
    # Process each selected variable
    for (var in input$outlier_var) {
      tryCatch({
        # Check if variable exists
        if (!var %in% names(display_data())) {
          showNotification(paste("Variable", var, "no longer exists in dataset"), type = "error")
          next
        }
        
        # Check for sufficient data
        if (sum(!is.na(display_data()[[var]])) < 4) {
          showNotification(paste("Not enough data points for outlier detection in", var), type = "warning")
          next
        }
        
        # Ensure variable is numeric before processing
        if (!is.numeric(display_data()[[var]])) {
          showNotification(paste("Variable", var, "must be numeric for outlier detection"), type = "error")
          next
        }
        
        # Retrieve both datasets
        displayed <- display_data()
        training <- training_data()
        
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
            rows_removed <- rows_removed + outlier_count
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
          
          # Update tracking variables
          total_processed <- total_processed + 1
          total_outliers <- total_outliers + outlier_count
          processed_vars <- c(processed_vars, var)
          
          # Update the reactive datasets
          #display_data(displayed)
          #training_data(training)
          # Use the atomic update function
          update_datasets(displayed, training, session)
        } else {
          showNotification(paste("No outliers detected in", var), type = "warning")
        }
      }, error = function(e) {
        showNotification(paste("Error in outlier detection for", var, ":", e$message), type = "error")
      })
    }
    
    # Show summary confirmation dialog if any variables were processed
    if (total_processed > 0) {
      showModal(
        modalDialog(
          title = "Outliers Handled",
          paste(total_processed, "variable(s) processed with a total of", total_outliers, "outliers detected."),
          tags$p(paste("Processed variables:", paste(processed_vars, collapse = ", "))),
          if (input$outlier_method == "Remove Outliers") {
            paste(rows_removed, "rows were removed.")
          } else {
            "The outliers have been replaced."
          },
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    } else {
      showNotification("No variables were processed for outlier detection.", type = "warning")
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
    req(training_data(), input$transform_var, input$transformation_method)
    
    # Add visual indicator that operation was applied
    addClass(id = "transform_var_ui", class = "operation-applied")
    
    # Update preprocessing summary
    preprocessing_ops$transformation <- list(
      method = input$transformation_method,
      variables = paste(input$transform_var, collapse = ", "),
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
    
    withProgress(message = 'Applying transformations...', value = 0, {
      # Create local copies
      data <- isolate(training_data())
      display <- isolate(display_data())
      selected_vars <- input$transform_var
      transformed_vars <- list()
      
      # Process variables in batches for efficiency
      incProgress(0.2, detail = "Processing variables...")
      
      if (input$transformation_method == "Min-Max Scaling") {
        # Vectorized approach
        for (var in selected_vars) {
          if (is.numeric(data[[var]])) {
            min_val <- min(data[[var]], na.rm = TRUE)
            max_val <- max(data[[var]], na.rm = TRUE)
            
            # Check for zero range to avoid division by zero
            if (max_val - min_val > 0) {
              data[[var]] <- (data[[var]] - min_val) / (max_val - min_val)
              display[[var]] <- (display[[var]] - min_val) / (max_val - min_val)
              transformed_vars[[var]] <- "Min-Max Scaling"
            } else {
              showNotification(paste("Skipping", var, "- all values are identical"), type = "warning")
            }
          } else {
            showNotification(paste("Skipping", var, "- not numeric"), type = "warning")
          }
        }
      }
      
      incProgress(0.6, detail = "Updating datasets...")
      
      # Update both datasets atomically
      training_data(data)
      display_data(display)
      
      incProgress(1.0, detail = "Complete")
    })
    
    # Show notification
    if (length(transformed_vars) > 0) {
      showNotification("Transformation applied successfully!", type = "message")
    } else {
      showNotification("No transformations were applied", type = "warning")
    }
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
    req(training_data(), input$encoding_var, input$encoding_method)
    
    # Add visual indicator that operation was applied
    addClass(id = "encoding_var_ui", class = "operation-applied")
    
    # Update preprocessing summary
    preprocessing_ops$encoding <- list(
      method = input$encoding_method,
      variables = paste(input$encoding_var, collapse = ", "),
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
    
    # Get the training data and selected variables
    data <- training_data()
    display_data_copy <- display_data() # Get a copy for updating display data too
    selected_vars <- input$encoding_var
    encoded_vars <- list()
    
    withProgress(message = 'Applying encoding...', value = 0, {
      # Check if all selected variables exist and are categorical
      invalid_vars <- c()
      for (var in selected_vars) {
        if (!var %in% names(data)) {
          invalid_vars <- c(invalid_vars, paste(var, "(not found)"))
        } else if (is.numeric(data[[var]])) {
          invalid_vars <- c(invalid_vars, paste(var, "(numeric)"))
        }
      }
      
      if (length(invalid_vars) > 0) {
        showNotification(paste("Invalid variables for encoding:", 
                             paste(invalid_vars, collapse=", ")), 
                       type = "error")
        return()
      }
      
      incProgress(0.2, detail = "Processing variables...")
      
      # Apply the selected encoding method to each variable
      for (var in selected_vars) {
        tryCatch({
          if (input$encoding_method == "Label Encoding") {
            # For label encoding, we can directly convert the factors to integers
            if (is.character(data[[var]])) {
              data[[var]] <- as.factor(data[[var]])
              display_data_copy[[var]] <- as.factor(display_data_copy[[var]])
            }
            data[[var]] <- as.integer(as.factor(data[[var]]))
            display_data_copy[[var]] <- as.integer(as.factor(display_data_copy[[var]]))
            encoded_vars[[var]] <- "Label Encoding"
          } else if (input$encoding_method == "One-Hot Encoding") {
            # For one-hot encoding, we create new binary columns for each category
            one_hot <- model.matrix(~ . - 1, data.frame(data[[var]]))
            colnames(one_hot) <- paste(var, colnames(one_hot), sep = "_")
            
            # Create the same one-hot encoding for display data
            one_hot_display <- model.matrix(~ . - 1, data.frame(display_data_copy[[var]]))
            colnames(one_hot_display) <- paste(var, colnames(one_hot_display), sep = "_")
            
            # Clean up column names for one-hot encoding - fixed pattern matching
            for (i in 1:ncol(one_hot)) {
              old_name <- colnames(one_hot)[i]
              # Extract just the unique value by removing everything before the last dot
              if (grepl("\\.\\.var\\.\\.+", old_name)) {
                unique_value <- sub(".*\\.\\.var\\.\\.(.+)$", "\\1", old_name)
                new_name <- paste0(var, "_", unique_value)
                colnames(one_hot)[i] <- new_name
                colnames(one_hot_display)[i] <- new_name
              }
            }
            
            # Add the new columns and remove the original
            data <- cbind(data, one_hot)
            data[[var]] <- NULL
            
            display_data_copy <- cbind(display_data_copy, one_hot_display)
            display_data_copy[[var]] <- NULL
            
            encoded_vars[[var]] <- "One-Hot Encoding"
          }
        }, error = function(e) {
          showNotification(paste("Error encoding variable", var, ":", e$message), type = "error")
        })
      }
      
      incProgress(0.8, detail = "Updating datasets...")
      
      # Update both datasets
      training_data(data)
      display_data(display_data_copy)
      
      incProgress(1.0, detail = "Complete")
    })
    
    # Show notification if any variables were encoded
    if (length(encoded_vars) > 0) {
      showNotification("Encoding applied successfully!", type = "message")
      
      # Prepare the encoding message
      encoding_method <- unique(unlist(encoded_vars))[1]
      var_list <- paste(names(encoded_vars), collapse = ", ")
      
      encoding_msg <- paste(encoding_method, "applied to the following variables:", var_list)
      
      # Show modal dialog with encoded variables
      showModal(
        modalDialog(
          title = "Encoding Applied",
          encoding_msg,
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    } else {
      showNotification("No variables were encoded.", type = "warning")
    }
    
    # Force garbage collection to free memory
    gc()
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
  
  observe({
    req(display_data())
    # Check for inconsistencies in the display data
    if(any(is.na(names(display_data())))) {
      showNotification("Warning: Column names contain NA values", type = "warning")
    }
  })
  
  # Add a validation observer
  observe({
    req(display_data())
    
    # Check for common data issues
    data <- display_data()
    
    # Look for NaN values which could indicate errors
    if (any(sapply(data, function(x) any(is.nan(x), na.rm = TRUE)))) {
      showNotification("Warning: Data contains NaN values which may indicate calculation errors", 
                       type = "warning")
    }
    
    # Check for infinite values
    if (any(sapply(data, function(x) any(is.infinite(x), na.rm = TRUE)))) {
      showNotification("Warning: Data contains infinite values", type = "warning")
    }
    
    # Validate row counts remain consistent
    if (!is.null(training_data()) && nrow(data) != nrow(training_data())) {
      showNotification("Warning: Row count mismatch between display and training data", 
                       type = "error")
    }
  })
  
  # Consider adding this functionality for preprocessing sections that might be computationally intensive
  observe({
    # Only enable transformation on larger datasets if explicitly allowed
    if (!is.null(display_data()) && nrow(display_data()) > 10000) {
      shinyjs::hide("transformation_section")
      shinyjs::show("show_transformation_button")
    } else {
      shinyjs::show("transformation_section")
      shinyjs::hide("show_transformation_button")
    }
  })
  
  # Update preprocessing operations when applied
  observeEvent(input$apply_missing, {
    preprocessing_ops$missing_values <- list(
      method = input$missing_method,
      variables = paste(input$missing_var, collapse = ", "),
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
  })
  
  observeEvent(input$apply_outliers, {
    preprocessing_ops$outliers <- list(
      method = input$outlier_method,
      variables = paste(input$outlier_var, collapse = ", "),
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
  })
  
  observeEvent(input$apply_transformation, {
    preprocessing_ops$transformation <- list(
      method = input$transformation_method,
      variables = paste(input$transform_var, collapse = ", "),
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
  })
  
  observeEvent(input$apply_encoding, {
    preprocessing_ops$encoding <- list(
      method = input$encoding_method,
      variables = paste(input$encoding_var, collapse = ", "),
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
  })
  
  # Render the preprocessing summary table
  output$preprocessing_summary <- renderTable({
    # Create a dataframe to hold the summary
    summary_data <- data.frame(
      Operation = character(),
      Method = character(),
      Variables = character(),
      Timestamp = character(),
      stringsAsFactors = FALSE
    )
    
    # Add missing values operation if it exists
    if (!is.null(preprocessing_ops$missing_values)) {
      summary_data <- rbind(summary_data, data.frame(
        Operation = "Missing Values",
        Method = preprocessing_ops$missing_values$method,
        Variables = preprocessing_ops$missing_values$variables,
        Timestamp = preprocessing_ops$missing_values$timestamp,
        stringsAsFactors = FALSE
      ))
    }
    
    # Add outliers operation if it exists
    if (!is.null(preprocessing_ops$outliers)) {
      summary_data <- rbind(summary_data, data.frame(
        Operation = "Outliers",
        Method = preprocessing_ops$outliers$method,
        Variables = preprocessing_ops$outliers$variables,
        Timestamp = preprocessing_ops$outliers$timestamp,
        stringsAsFactors = FALSE
      ))
    }
    
    # Add transformation operation if it exists
    if (!is.null(preprocessing_ops$transformation)) {
      summary_data <- rbind(summary_data, data.frame(
        Operation = "Transformation",
        Method = preprocessing_ops$transformation$method,
        Variables = preprocessing_ops$transformation$variables,
        Timestamp = preprocessing_ops$transformation$timestamp,
        stringsAsFactors = FALSE
      ))
    }
    
    # Add encoding operation if it exists
    if (!is.null(preprocessing_ops$encoding)) {
      summary_data <- rbind(summary_data, data.frame(
        Operation = "Encoding",
        Method = preprocessing_ops$encoding$method,
        Variables = preprocessing_ops$encoding$variables,
        Timestamp = preprocessing_ops$encoding$timestamp,
        stringsAsFactors = FALSE
      ))
    }
    
    if (nrow(summary_data) == 0) {
      return(data.frame(Message = "No preprocessing operations applied yet."))
    }
    
    return(summary_data)
  }, bordered = TRUE, hover = TRUE, spacing = "m", align = "l")
} # End of server function
