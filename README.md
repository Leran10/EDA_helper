# EDAhelper

An R package providing helper functions for Exploratory Data Analysis (EDA) with both R functions and an interactive Shiny interface.

## Installation

You can install EDAhelper from the source package:

```r
# Install from local source
install.packages("/path/to/EDAhelper", repos = NULL, type = "source")

# Or for user-specific installation
install.packages("/path/to/EDAhelper", repos = NULL, type = "source", lib = "~/R/library")
```

After installation, run the test script to verify everything is working correctly:

```r
# Test the installation
source(system.file("examples", "test_installation.R", package = "EDAhelper"))
```

## Features

EDAhelper provides the following functionality:

- Data summary statistics and visualization
- Data preprocessing (imputation, encoding, scaling)
- Missing value analysis and visualization
- Correlation analysis with visualizations
- Outlier detection using multiple methods
- Subgroup analysis with interactive visualizations
- Model recommendations
  - Basic model suggestions based on data characteristics
  - Enhanced model recommendations based on user preferences and study plans
  - Prioritize model characteristics (accuracy, interpretability, speed, explainability)
  - Exclude specific model types or focus on model families
- Automated EDA reporting
- Interactive Shiny web interface

## Function Documentation

### Data Summary and Visualization

#### `data_summary()`
Provides descriptive statistics and visualizations for all variables in a dataset.
```r
data_summary(data, include_plots = TRUE)
```
- `data`: A dataframe to analyze
- `include_plots`: Whether to generate plots for each variable

#### `load_demo_data()`
Loads a demonstration dataset with various data types and patterns.
```r
demo_data <- load_demo_data()
```

### Data Preprocessing

#### `preprocess_data()`
Comprehensive data preprocessing including imputation, encoding, and scaling.
```r
preprocess_data(
  data, 
  impute_numeric = "median",
  impute_categorical = "mode", 
  encode_categorical = "none",
  drop_original = TRUE,
  scale = FALSE,
  center = FALSE,
  drop_cols = NULL,
  fill_value = NULL,
  impute_threshold = 50,
  knn_k = 5
)
```
- `data`: A dataframe to preprocess
- `impute_numeric`: Method for numeric imputation: "mean", "median", "mode", "knn", "none"
- `impute_categorical`: Method for categorical imputation: "mode", "new_category", "none"
- `encode_categorical`: Method for encoding: "one_hot", "label", "binary", "none"
- `drop_original`: Whether to drop original columns after encoding
- `scale`: Whether to scale numeric variables
- `center`: Whether to center numeric variables
- `drop_cols`: Columns to drop
- `fill_value`: Manual value to use for imputation
- `impute_threshold`: Maximum percentage of missing values to impute
- `knn_k`: Number of neighbors for KNN imputation

#### `create_dummies()`
Creates dummy variables (one-hot encoding) for categorical variables.
```r
create_dummies(data, columns = NULL, drop_original = TRUE, max_levels = 10)
```
- `data`: A dataframe
- `columns`: Columns to encode (NULL for all categorical)
- `drop_original`: Whether to drop original columns
- `max_levels`: Maximum number of unique values to encode

#### `impute_missing()`
Imputes missing values in a dataframe.
```r
impute_missing(data, numeric_method = "median", categorical_method = "mode", 
               knn_k = 5, threshold = 50)
```
- `data`: A dataframe
- `numeric_method`: Method for numeric imputation
- `categorical_method`: Method for categorical imputation
- `knn_k`: Number of neighbors for KNN imputation
- `threshold`: Maximum percentage of missing values to impute

#### `scale_variables()`
Scales and centers numeric variables.
```r
scale_variables(data, columns = NULL, center = TRUE, scale = TRUE)
```
- `data`: A dataframe
- `columns`: Columns to scale (NULL for all numeric)
- `center`: Whether to center the variables
- `scale`: Whether to scale the variables

#### `detect_data_types()`
Identifies data types in a dataframe.
```r
detect_data_types(data)
```
- `data`: A dataframe to analyze

### Missing Data Analysis

#### `missing_analysis()`
Analyzes missing values in a dataset, including patterns and visualizations.
```r
missing_analysis(data, plot = TRUE)
```
- `data`: A dataframe to analyze
- `plot`: Whether to generate visualizations

### Correlation Analysis

#### `correlation_analysis()`
Computes and visualizes correlations between variables.
```r
correlation_analysis(data, method = "pearson", plot = TRUE, threshold = 0.7)
```
- `data`: A dataframe to analyze
- `method`: Correlation method: "pearson", "spearman", or "kendall"
- `plot`: Whether to create visualizations
- `threshold`: Threshold for identifying strong correlations

#### `correlation_plot()`
Creates a correlation plot for a dataset (wrapper function).
```r
correlation_plot(data, method = "pearson", type = "numeric")
```
- `data`: A dataframe to analyze
- `method`: Correlation method: "pearson", "spearman", or "kendall"
- `type`: Type of correlation: "numeric", "categorical", "mixed", or "all"

### Outlier Detection

#### `outlier_detection()`
Detects outliers using various methods.
```r
outlier_detection(data, methods = c("iqr", "zscore"), threshold = 3, plot = TRUE)
```
- `data`: A dataframe to analyze
- `methods`: Detection methods: "iqr", "zscore", "modified_zscore", "dbscan"
- `threshold`: Threshold for outlier detection
- `plot`: Whether to generate visualizations

### Model Recommendations

#### `model_recommendations()`
Recommends models based on data characteristics and user questions.
```r
model_recommendations(data, question = NULL, local_only = TRUE, include_code = TRUE)
```
- `data`: A dataframe to analyze
- `question`: Analysis question/objective as a string
- `local_only`: Whether to use only local logic
- `include_code`: Whether to include implementation code examples

#### `enhanced_model_recommendations()`
Provides enhanced model recommendations with additional customization options.
```r
enhanced_model_recommendations(
  data, study_plan = NULL, prioritize = c("accuracy", "interpretability"),
  exclude_models = NULL, model_family = NULL, max_complexity = 5,
  required_features = NULL, target_variable = NULL,
  local_only = TRUE, include_code = TRUE
)
```
- `data`: A dataframe to analyze
- `study_plan`: Research objective/study plan as a string
- `prioritize`: Order of priorities: "accuracy", "interpretability", "speed", "explainability"
- `exclude_models`: Models to exclude: "neural_networks", "svm", "gradient_boosting", "regression"
- `model_family`: Focus on a model family: "linear", "tree_based", "distance_based", "bayesian", "ensemble"
- `max_complexity`: Maximum model complexity (1-5)
- `required_features`: Required model features: "feature_importance", "confidence_intervals", "probabilistic", "kernel_methods"
- `target_variable`: Target variable name
- `local_only`: Whether to use only local logic
- `include_code`: Whether to include implementation code examples

### Report Generation

#### `generate_eda_report()`
Generates an HTML report with EDA results.
```r
generate_eda_report(
  data, output_file = "eda_report.html", title = "EDA Report",
  author = NULL, include_code = FALSE, theme = "cosmo", toc = TRUE,
  correlation_method = "pearson", outlier_methods = c("iqr", "zscore"),
  additional_options = list()
)
```
- `data`: A dataframe to analyze
- `output_file`: Output file path
- `title`: Report title
- `author`: Report author
- `include_code`: Whether to include R code in the report
- `theme`: HTML theme for the report
- `toc`: Whether to include a table of contents
- `correlation_method`: Correlation method to use
- `outlier_methods`: Outlier detection methods to use
- `additional_options`: Additional options for customizing the report

### Shiny App Interface

#### `run_eda_app()`
Launches the interactive Shiny web interface.
```r
run_eda_app(browser = FALSE)
```
- `browser`: Whether to launch in the system browser

## Understanding Imputation Methods

### Numeric Imputation Methods:

- **Mean**: Replaces missing values with the column's mean value. Suitable for normally distributed data.
- **Median**: Replaces missing values with the column's median value. More robust to outliers than mean.
- **Mode**: Replaces missing values with the most frequently occurring value.
- **KNN**: Uses K-Nearest Neighbors algorithm to impute values based on similar records.

### Categorical Imputation Methods:

- **Mode**: Replaces missing values with the most frequently occurring category. This preserves the natural distribution of values and is suitable when:
  - The missing values are likely missing completely at random (MCAR)
  - Maintaining the original categories is important
  - The most frequent value is a sensible default

- **New Category**: Creates a new 'Missing' category. This approach is suitable when:
  - Missing values might not be random (MNAR)
  - The fact that data is missing could be meaningful
  - You want to explicitly track which values were originally missing

## Example Usage

### Basic R Functions

```r
library(EDAhelper)

# Load demo data or your own data
data <- load_demo_data()
# Or use your own: data <- read.csv("your_data.csv")

# Generate a data summary
summary_result <- data_summary(data)
print(summary_result)

# Analyze missing values
missing_result <- missing_analysis(data)
print(missing_result)

# Create correlation analysis
corr_result <- correlation_analysis(data)
print(corr_result)
# Display the correlation plot
corr_result$plots$numeric()

# Detect outliers in numeric columns
outlier_result <- outlier_detection(data[, sapply(data, is.numeric), drop = FALSE])
print(outlier_result)
# Display an outlier plot
outlier_result$plots[[1]]

# Get model recommendations
model_rec <- model_recommendations(data, question = "Predict CustomerValue based on demographics")
print(model_rec)

# Get enhanced model recommendations with preferences
enhanced_rec <- enhanced_model_recommendations(
  data,
  study_plan = "Find interpretable models to predict CustomerValue that prioritize explainability",
  prioritize = c("interpretability", "explainability", "accuracy"),
  model_family = "tree_based",
  max_complexity = 3
)
print(enhanced_rec)

# Generate an EDA report
generate_eda_report(data, "eda_report.html", title = "My EDA Report")
```

### Interactive Shiny App

```r
library(EDAhelper)

# Launch the interactive Shiny app
run_eda_app()
```

## Notes for Using Missing Data Module

If you have missing values that aren't being detected, R only recognizes explicit `NA` values as missing. When loading your data, make sure to properly handle missing values:

```r
# When reading CSV files, specify missing value indicators
data <- read.csv("your_file.csv", 
                na.strings = c("NA", "N/A", "", " ", "#N/A", "NULL", "null", "None", "none"))
```

## License

This project is licensed under the MIT License - see the LICENSE file for details.