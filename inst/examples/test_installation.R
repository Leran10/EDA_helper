# Test script for EDAhelper package
# Run this script to check if the package is installed correctly and to see a simple demo

# Load the package
library(EDAhelper)

# Print package version
cat("EDAhelper package version:", as.character(packageVersion("EDAhelper")), "\n\n")

# Check for required packages
required_packages <- c("shiny", "shinydashboard", "DT", "ggplot2", "plotly", "dplyr", "tidyr", "scales")
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  cat("Missing required packages:", paste(missing_packages, collapse = ", "), "\n")
  cat("Please install them with:\n")
  cat(paste0("install.packages(c('", paste(missing_packages, collapse = "', '"), "'))\n\n"))
} else {
  cat("All required packages are installed.\n\n")
}

# Load the demo data
cat("Loading demo data...\n")
demo_data <- load_demo_data()
cat("Demo data loaded with", nrow(demo_data), "rows and", ncol(demo_data), "columns.\n\n")

# Show a simple example of each function
cat("Running basic examples...\n")

# Data summary
cat("1. Data summary\n")
summary_result <- data_summary(demo_data, include_plots = FALSE)
cat("   ✓ Data summary created\n")

# Missing analysis
cat("2. Missing value analysis\n")
missing_result <- missing_analysis(demo_data, plot = FALSE)
cat("   ✓ Missing value analysis created\n")

# Correlation analysis
cat("3. Correlation analysis\n")
corr_result <- correlation_analysis(demo_data, plot = FALSE)
cat("   ✓ Correlation analysis created\n")

# Outlier detection
cat("4. Outlier detection\n")
outlier_result <- outlier_detection(demo_data[, c("Age", "Income", "Weight")], plot = FALSE)
cat("   ✓ Outlier detection created\n")

# Model recommendations
cat("5. Model recommendations\n")
model_rec <- model_recommendations(
  demo_data, 
  question = "Predict HealthRisk based on demographic variables",
  local_only = TRUE
)
cat("   ✓ Model recommendations created\n\n")

cat("All tests completed successfully!\n\n")

# Ask to launch Shiny app
cat("Would you like to launch the Shiny app? (y/n): ")
response <- tolower(readline())

if (response == "y" || response == "yes") {
  cat("\nLaunching Shiny app...\n")
  run_eda_app()
} else {
  cat("\nTo launch the Shiny app later, run: run_eda_app()\n")
}