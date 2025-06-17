# EDAhelper Interactive Shiny App

This Shiny app provides an interactive web interface for the EDAhelper R package. It allows users to perform exploratory data analysis (EDA) tasks through a user-friendly GUI without writing code.

![EDAhelper Shiny App Screenshot](app_screenshot.png)

*Note: The screenshot will be generated the first time you run the app.*

## Features

- **Data Upload**: Upload CSV files and see a data preview
- **Data Summary**: View summary statistics and visualizations for individual variables
- **Missing Data Analysis**: Analyze and visualize missing data patterns
- **Correlation Analysis**: Explore correlations between variables with interactive plots
- **Subgroup Analysis**: Analyze variable distributions across different subgroups
- **Outlier Detection**: Detect and visualize outliers using multiple methods
- **Model Recommendations**: Get suggestions for appropriate models based on your data

## Subgroup Analysis

The Subgroup Analysis tab allows you to:

1. Select a target variable to analyze
2. Choose a grouping variable to create subgroups
3. Filter data based on additional criteria
4. Visualize distributions with facet wrap or overlaid plots
5. Normalize values for better comparison between groups
6. View summary statistics for each subgroup

## Running the App

To launch the app from the EDAhelper package:

```r
library(EDAhelper)
run_eda_app()
```

To run in a browser:

```r
run_eda_app(browser = TRUE)
```

## Requirements

This app requires the following packages:

- shiny
- shinydashboard
- DT
- ggplot2
- plotly
- dplyr
- tidyr
- scales
- EDAhelper