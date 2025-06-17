library(testthat)
library(EDAhelper)

# Create a test dataset for reuse in multiple tests
setup_test_data <- function() {
  set.seed(123)
  test_data <- data.frame(
    num1 = c(rnorm(95), NA, NA, NA, NA, NA),  # 5 NAs
    num2 = c(runif(98), 100, 101),  # 2 outliers
    cat1 = c(sample(LETTERS[1:5], 90, replace = TRUE), rep(NA, 10)),  # 10 NAs
    cat2 = factor(sample(c("Yes", "No", "Maybe"), 100, replace = TRUE)),
    date1 = as.Date("2023-01-01") + sample(0:100, 100, replace = TRUE)
  )
  return(test_data)
}

# Tests for data_summary function
test_that("data_summary works with iris dataset", {
  result <- data_summary(iris, include_plots = FALSE)
  expect_s3_class(result, "data_summary")
  expect_equal(length(result$numeric_summary), 4)  # 4 numeric variables in iris
  expect_equal(length(result$categorical_summary), 1)  # 1 categorical variable in iris
})

test_that("data_summary handles different column types correctly", {
  test_data <- setup_test_data()
  result <- data_summary(test_data, include_plots = FALSE)
  
  # Check structure
  expect_s3_class(result, "data_summary")
  
  # Check the structure based on examining the result
  str_result <- capture.output(str(result))
  expect_true(any(grepl("numeric_summary", str_result)))
  expect_true(any(grepl("categorical_summary", str_result)))
  
  # Check numeric summary exists for numeric columns
  expect_true("num1" %in% names(result$numeric_summary))
  expect_true("num2" %in% names(result$numeric_summary))
  
  # Check categorical summary exists for categorical columns
  expect_true("cat1" %in% names(result$categorical_summary))
  expect_true("cat2" %in% names(result$categorical_summary))
  
  # Verify at least some missing values are detected
  expect_true(any(sapply(result$numeric_summary, function(x) !is.null(x$missing) && x$missing > 0)))
})

test_that("data_summary generates plots when requested", {
  test_data <- setup_test_data()
  result <- data_summary(test_data, include_plots = TRUE)
  
  # Check plots are created
  expect_true(!is.null(result$plots))
  expect_true(length(result$plots$numeric) > 0)
  expect_true(length(result$plots$categorical) > 0)
})

# Tests for missing_analysis function
test_that("missing_analysis works with airquality dataset", {
  # airquality has some missing values
  result <- missing_analysis(airquality, plot = FALSE)
  expect_s3_class(result, "missing_analysis")
  expect_true(result$overall$missing_cells > 0)
})

test_that("missing_analysis correctly identifies missing patterns", {
  test_data <- setup_test_data()
  result <- missing_analysis(test_data, plot = FALSE)
  
  # Check structure
  expect_s3_class(result, "missing_analysis")
  
  # Examine the structure 
  str_result <- capture.output(str(result))
  
  # Check for presence of components rather than exact structure
  expect_true(any(grepl("overall|summary", str_result, ignore.case = TRUE)))
  expect_true(any(grepl("column|variable", str_result, ignore.case = TRUE)))
  
  # Verify missing values are detected
  # Extract data on missing values by looking at the object structure
  expect_true(any(grepl("missing|NA", str_result, ignore.case = TRUE)))
  
  # Check if num1 and cat1 are identified as having missing values
  missing_info <- capture.output(print(result))
  expect_true(any(grepl("num1", missing_info)))
  expect_true(any(grepl("cat1", missing_info)))
})

test_that("missing_analysis generates plots when requested", {
  test_data <- setup_test_data()
  result <- missing_analysis(test_data, plot = TRUE)
  
  # Check plots are created
  expect_true(!is.null(result$plots))
  expect_true(length(result$plots) > 0)
})

# Tests for correlation_analysis function
test_that("correlation_analysis works with mtcars dataset", {
  result <- correlation_analysis(mtcars, plot = FALSE)
  expect_s3_class(result, "correlation_analysis")
  expect_equal(dim(result$numeric_correlation), c(11, 11))  # mtcars has 11 variables
})

test_that("correlation_analysis handles different correlation methods", {
  test_data <- setup_test_data()
  
  # Test different methods
  result_pearson <- correlation_analysis(test_data, method = "pearson", plot = FALSE)
  result_spearman <- correlation_analysis(test_data, method = "spearman", plot = FALSE)
  result_kendall <- correlation_analysis(test_data, method = "kendall", plot = FALSE)
  
  # Check structure for each method
  expect_s3_class(result_pearson, "correlation_analysis")
  expect_s3_class(result_spearman, "correlation_analysis")
  expect_s3_class(result_kendall, "correlation_analysis")
  
  # Check dimensions - 2 numeric variables
  expect_equal(dim(result_pearson$numeric_correlation), c(2, 2))
  
  # Check categorical correlations (Cramer's V)
  expect_true(!is.null(result_pearson$categorical_correlation))
  
  # Check mixed correlations (correlation ratio)
  expect_true(!is.null(result_pearson$mixed_correlation))
})

test_that("correlation_analysis identifies strong correlations", {
  # Create dataset with known strong correlation
  set.seed(123)
  x <- rnorm(100)
  strong_data <- data.frame(
    var1 = x,
    var2 = x * 0.9 + rnorm(100, 0, 0.1),  # Strong positive correlation
    var3 = rnorm(100)  # Uncorrelated
  )
  
  result <- correlation_analysis(strong_data, threshold = 0.7, plot = FALSE)
  
  # Should detect strong correlation between var1 and var2
  expect_true(nrow(result$strong_correlations) > 0)
  strong_pairs <- paste(result$strong_correlations$var1, result$strong_correlations$var2, sep="_")
  expect_true("var1_var2" %in% strong_pairs || "var2_var1" %in% strong_pairs)
})

# Tests for outlier_detection function
test_that("outlier_detection works with basic dataset", {
  # Create dataset with obvious outliers
  test_data <- data.frame(
    x = c(1, 2, 3, 4, 5, 100),  # outlier at 100
    y = c(10, 20, 30, 40, 50, 55)
  )
  result <- outlier_detection(test_data, methods = "iqr", plot = FALSE)
  expect_s3_class(result, "outlier_detection")
  expect_true(!is.null(result$outliers$iqr$x))  # Should detect outlier in x
  expect_equal(length(result$outliers$iqr$x$indices), 1)  # 1 outlier in x
})

test_that("outlier_detection works with different methods", {
  test_data <- setup_test_data()
  
  # Test individual methods
  result_iqr <- outlier_detection(test_data, methods = "iqr", plot = FALSE)
  result_zscore <- outlier_detection(test_data, methods = "zscore", plot = FALSE)
  result_mzscore <- outlier_detection(test_data, methods = "modified_zscore", plot = FALSE)
  
  # Check structure
  expect_s3_class(result_iqr, "outlier_detection")
  expect_s3_class(result_zscore, "outlier_detection")
  expect_s3_class(result_mzscore, "outlier_detection")
  
  # Should detect outliers in num2
  expect_true(!is.null(result_iqr$outliers$iqr$num2))
  expect_true(!is.null(result_zscore$outliers$zscore$num2))
  expect_true(!is.null(result_mzscore$outliers$modified_zscore$num2))
  
  # Should detect the 2 outliers we introduced
  expect_equal(length(result_iqr$outliers$iqr$num2$indices), 2)
})

test_that("outlier_detection respects variable selection", {
  # Create dataset with outliers in multiple columns
  test_data <- data.frame(
    a = c(1, 2, 3, 4, 5, 100),
    b = c(10, 20, 30, 40, 500, 50),
    c = c(100, 200, 3000, 400, 500, 600)
  )
  
  # Test with specific variables
  result <- outlier_detection(test_data, methods = "iqr", variables = c("a", "b"), plot = FALSE)
  
  # Should only analyze the selected variables
  expect_true(!is.null(result$outliers$iqr$a))
  expect_true(!is.null(result$outliers$iqr$b))
  expect_true(is.null(result$outliers$iqr$c))
})

# Tests for model_recommendations function
test_that("model_recommendations works with basic dataset", {
  skip_if_not_installed("openai")  # Skip if openai package not available
  test_data <- iris[, -5]  # Remove target to simulate prediction task
  target <- iris[, 5]
  
  # Use local only to avoid API dependency
  result <- model_recommendations(
    data = test_data,
    question = "Predict iris species based on these measurements",
    local_only = TRUE,
    include_code = TRUE
  )
  
  # Check structure
  expect_s3_class(result, "model_recommendations")
  expect_named(result, c("question", "data_summary", "task_identification", 
                         "recommended_models", "evaluation_metrics", "implementation"))
  
  # Check task identification
  expect_true(!is.null(result$task_identification$task_type))
  
  # Check model recommendations
  expect_true(length(result$recommended_models) > 0)
  
  # Check evaluation metrics
  expect_true(length(result$evaluation_metrics) > 0)
  
  # Check implementation
  expect_true(!is.null(result$implementation$code_examples))
})

# Tests for generate_eda_report function
test_that("generate_eda_report generates a valid report", {
  skip_if_not_installed("rmarkdown")  # Skip if rmarkdown not available
  skip_if_not(rmarkdown::pandoc_available(), "Pandoc is not available")
  
  test_data <- setup_test_data()
  temp_file <- tempfile(fileext = ".html")
  
  result <- generate_eda_report(
    data = test_data,
    output_file = temp_file,
    title = "Test EDA Report",
    include_code = TRUE
  )
  
  # Check if file was created
  expect_true(file.exists(temp_file))
  
  # Check if the returned value is the file path
  expect_equal(result, temp_file)
  
  # Clean up
  if (file.exists(temp_file)) file.remove(temp_file)
})