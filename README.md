# 📊DataPrepHIS - Data Preprocessing and Analysis Tool

A comprehensive R Shiny web application for data preprocessing, analysis, and visualization. This tool provides an interactive interface for handling various data preparation tasks and generating detailed reports.

## Features
- **Data Loading**: Upload datasets in CSV or Excel formats.
- **Data Preprocessing**: Handle missing values, scale numerical features, encode categorical variables, and manage outliers.
- **Data Visualization**: Perform univariate and bivariate analyses, including histograms, box plots, correlation plots, and advanced metrics.
- **Report Generation**: Comprehensive HTML reports including:Dataset overview, Preprocessing operations summary, Statistical analysis,Data visualizations, Executive summary.

## Application Structure
### Sidebar Menu
- **Load Data**: Allows users to upload datasets and explore their structure.
- **Preprocessing**: Offers tools for handling missing values, outliers, and feature transformations.
- **Visualization**: Enables univariate and bivariate analyses using various visualizations.
- **Report**: Displays metrics and visualizations for trained models.
- **About**: Contains project details and acknowledgments.

## Detailed Features

### 1. Load Data
The Load Data section allows users to upload and explore datasets. The key functionalities include:
- **File Upload**:
  - Supports CSV and Excel files.
  - For Excel files, allows multiple sheet handle and load.
  - Displays error notifications for unsupported file types or missing sheets.
- **File Details**:
  - Displays metadata such as file name, format, number of instances (rows), features (columns), and counts of categorical and numerical features.
- **Dataset Exploration**:
  - Provides a scrollable and searchable table view of the uploaded dataset.
  - Displays a summary of each column, including type (numerical or categorical), missing values, and statistical measures like mean, median, and standard deviation.
- **Column Selector**:
	- **Checkbox Selection**: The tool must provide checkboxes for individual column selection
	- **Bulk Selection Actions**: The tool must implement:
		– ”Select All” button to check all columns
		– ”Deselect All” button to uncheck all columns
		– ”Select Numeric” to select only numeric columns
		– ”Select Categorical” to select only categorical columns
	- **Position-Based Selection**: The tool must provide:
		– Numeric input to select first N columns with ”Apply” button
		– Numeric input to select last N columns with ”Apply” button
		– Range slider to select columns by position range
	- **Search Functionality**: The tool must include a search box that filters columns by name
	- **Type Indication**: The tool must visually indicate column types (numeric/categorical) using colour coding.
	- **Column Selection Modal**: The tool must provide a modal dialog for column selection with filtering capabilities.
- **Feature Management**:
  - **Drop Features**: Dynamically select and remove specific features (columns) from the dataset. Feedback is provided to confirm dropped features.
  - **Convert Features**:
    - Numerical to Categorical: Converts selected numerical variables to categorical by changing their data type to factors.
    - Categorical to Numerical: Converts selected categorical variables to numerical, restoring original values if available or attempting numeric conversion for compatible data.

### 2. Preprocessing
The Preprocessing section provides advanced tools for cleaning and preparing the dataset:
- **Missing Value Handling**
  - Variable Selection: Dynamically select a variable to manage its missing values.
  - Methods:
    - Row Deletion: Removes rows containing missing values for the selected variable.
    - Replace with Mode: Fills missing values with the most frequent value.
    - Replace with Median: Fills missing values with the median of the variable.
    - Replace with Mean: Fills missing values with the mean of the variable.
- **Managing Outliers**
  - Variable Selection: Supports numerical variables for outlier handling.
  - Outlier Detection: Uses the Interquartile Range (IQR) method to identify outliers.
  - Methods:
    - Remove Outliers: Deletes rows containing outliers.
    - Replace with Median: Replaces outliers with the median value of the variable.
    - Replace with Mean: Replaces outliers with the mean value of the variable.
- **Data Transformation**
  - Variable Selection: Dynamically select one or more numerical variables for transformation.
  - Transformation Methods:
    - Min-Max Scaling: Scales values to a range of [0, 1].
    - Z-Score Normalization: Standardizes values by centering around the mean and scaling to unit variance.
    - Log Transformation: Applies log transformation to handle skewed distributions (adds 1 to avoid log of zero).
- **Data Encoding**
  - Variable Selection: Dynamically identify categorical variables for encoding.
  - Encoding Methods:
    - Label Encoding: Converts categorical values to integer labels.
    - One-Hot Encoding: Expands categorical variables into multiple binary columns.

### 3. Analyse Data
The Analyse Data section provides comprehensive tools for univariate and bivariate analysis:
- **Univariate Analysis**
  - Histograms, Box Plots, Pie Charts, and Summary Statistics.
- **Bivariate Analysis**
  - Correlation Plots, Correlation Coefficients, Heatmaps, Parallel Box Plots, and more.

## How to Use
1. **Start the App**: Run the application in RStudio or using the Shiny Server.
   ```
   shiny::runApp("path_to_app")
   ```
2. **Upload Data**: Navigate to the "Load Data" tab and upload your dataset.
3. **Preprocess Data**: Use the "Preprocessing" tab to clean and prepare the data.
4. **View Reports**: Check the "Reports" tab for metrics and visualizations.

## Technical Details
### Libraries Used
- **UI Libraries**: shinyjqui, shinydashboard, bslib, shinyjs,shinyBS,shinyvalidate
- **Data Handling**: DT ,reactable ,readxl ,dplyr ,tidyr ,lubridate ,writexl ,mice ,missForest ,caret ,ROSE ,smotefamily
- **Visualization**: plotly, ggplot2, corrplot, GGally, viridis, moments
- **Reporting Packages**: rmarkdown, knitr, kableExtra, dlookr

## Getting Started

1. **Clone the Repository**
```bash
git clone [repository-url]
cd HIS-Project-main
```

2. **Install Required Packages**
```r
# Run in R console
source("R_Shiny/app.R")
```

3. **Launch the Application**
```r
shiny::runApp("R_Shiny")
```

## Authentication
- Default credentials:
  - Username: `HIS`
  - Password: `1234`
  
## Usage Guide

1. **Login** using the provided credentials
2. **Upload Data** through the Load Data tab
3. **Preprocess** your data using various available tools
4. **Visualize** the data using interactive plots
5. **Generate Reports** for documentation and analysis

## Data Processing Workflow

1. **Data Loading**
   - Upload your dataset
   - Review file details and structure

2. **Data Preprocessing**
   - Handle missing values
   - Treat outliers
   - Transform features
   - Encode categorical variables

3. **Data Analysis**
   - Explore data distributions
   - Analyze correlations
   - Generate visualizations

4. **Report Generation**
   - Configure report settings
   - Generate comprehensive HTML report
   - Download and share findings


## System Requirements

- R version 4.0.0 or higher
- Operating System: Windows/macOS/Linux
- Minimum 4GB RAM recommended
- Internet connection for package installation

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Troubleshooting

Common issues and their solutions:

1. Package Installation Issues:
   ```r
   # If you encounter package installation issues, try:
   install.packages("package_name", dependencies = TRUE)
   ```

2. Memory Issues:
   - Close other applications
   - Increase R memory limit

For more issues, please raise them in the repository's Issues section.

## Version

Current Version: 1.0.0

## Authors

[Adesh Shirke | Chirag Chawla | Samyak Soni]
[Msc Students Frankfurt University Of Applied Sciences]
