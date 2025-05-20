# Load required packages with error handling
required_packages <- c("openxlsx", "dplyr")
for(package in required_packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
  library(package, character.only = TRUE)
}

# Set seed for reproducibility
set.seed(123)

# Create a function to generate random data
generate_random_data <- function(n_rows = 200) {
  # Input validation
  if (!is.numeric(n_rows) || n_rows <= 0) {
    stop("n_rows must be a positive number")
  }
  
  # Initialize empty data frame
  df <- data.frame(matrix(nrow = n_rows, ncol = 0))
  
  # Generate 50 numeric columns with different distributions
  cat("Generating numeric columns...\n")
  for (i in 1:50) {
    if (i %% 10 == 0) cat(sprintf("Progress: %d%%\n", i * 2))
    col_name <- paste0("numeric_", i)
    
    # Randomly choose distribution type
    dist_type <- sample(c("normal", "uniform", "poisson", "exponential"), 1)
    
    if (dist_type == "normal") {
      df[[col_name]] <- rnorm(n_rows, mean = sample(1:100, 1), sd = sample(1:20, 1))
    } else if (dist_type == "uniform") {
      df[[col_name]] <- runif(n_rows, min = sample(1:50, 1), max = sample(51:200, 1))
    } else if (dist_type == "poisson") {
      df[[col_name]] <- rpois(n_rows, lambda = sample(1:20, 1))
    } else {
      df[[col_name]] <- rexp(n_rows, rate = 1/sample(1:50, 1))
    }
    
    # Add some NA values randomly (5% of values)
    na_indices <- sample(1:n_rows, size = round(0.05 * n_rows))
    df[[col_name]][na_indices] <- NA
  }
    # Generate 50 categorical columns with different characteristics
  categories <- c("Low", "Medium", "High", "Very High", "Extreme")
  colors <- c("Red", "Blue", "Green", "Yellow", "Black", "White", "Purple")
  countries <- c("USA", "UK", "Canada", "Australia", "Germany", "France", "Japan")
  statuses <- c("Active", "Inactive", "Pending", "Completed", "Cancelled")
  yes_no <- c("Yes", "No")
  
  cat("Generating categorical columns...\n")
  for (i in 1:50) {
    if (i %% 10 == 0) cat(sprintf("Progress: %d%%\n", 50 + i * 2))
    col_name <- paste0("categorical_", i)
    
    # Randomly choose category type
    cat_type <- sample(c("levels", "colors", "countries", "status", "binary"), 1)
    
    if (cat_type == "levels") {
      df[[col_name]] <- sample(categories, n_rows, replace = TRUE, 
                               prob = c(0.3, 0.25, 0.2, 0.15, 0.1))
    } else if (cat_type == "colors") {
      df[[col_name]] <- sample(colors, n_rows, replace = TRUE)
    } else if (cat_type == "countries") {
      df[[col_name]] <- sample(countries, n_rows, replace = TRUE, 
                               prob = c(0.4, 0.2, 0.15, 0.1, 0.05, 0.05, 0.05))
    } else if (cat_type == "status") {
      df[[col_name]] <- sample(statuses, n_rows, replace = TRUE)
    } else {
      df[[col_name]] <- sample(yes_no, n_rows, replace = TRUE, prob = c(0.7, 0.3))
    }
    
    # Add some NA values randomly (5% of values)
    na_indices <- sample(1:n_rows, size = round(0.05 * n_rows))
    df[[col_name]][na_indices] <- NA
  }
  
  # Add some date columns (5 columns)
  start_date <- as.Date("2020-01-01")
  for (i in 1:5) {
    col_name <- paste0("date_", i)
    random_days <- sample(0:1000, n_rows, replace = TRUE)
    df[[col_name]] <- start_date + random_days
  }
  
  # Reorder columns randomly
  df <- df[, sample(1:ncol(df))]
  
  return(df)
}

# Generate the data
random_data <- generate_random_data(200)
gc()  # Clean up memory

# Add error handling for file operations
tryCatch({
  # Write to CSV
  write.csv(random_data, "random_test_data.csv", row.names = FALSE)
  
  # Write to Excel
  wb <- createWorkbook()
  addWorksheet(wb, "TestData")
  writeData(wb, sheet = 1, random_data)
  saveWorkbook(wb, "random_test_data.xlsx", overwrite = TRUE)
  
  cat("Files generated successfully:\n- random_test_data.csv\n- random_test_data.xlsx\n")
}, error = function(e) {
  cat("Error occurred while saving files:", conditionMessage(e), "\n")
})