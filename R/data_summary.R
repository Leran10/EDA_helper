#' Generate comprehensive summary statistics for a dataset
#'
#' This function creates a detailed summary of a dataframe, including basic statistics,
#' data types, and value distributions.
#'
#' @param data A dataframe to analyze
#' @param include_plots Logical, whether to include distribution plots (default: TRUE)
#' @return A list containing summary information
#' @export
#'
#' @examples
#' data_summary(iris)
#' data_summary(mtcars, include_plots = FALSE)
data_summary <- function(data, include_plots = TRUE) {
  if (!is.data.frame(data)) {
    stop("Input must be a dataframe")
  }
  
  # Create basic structure for the results
  result <- list()
  
  # Basic dataset information
  result$overview <- list(
    dimensions = dim(data),
    column_names = names(data),
    column_types = sapply(data, class),
    memory_usage = utils::object.size(data)
  )
  
  # Identify numeric and categorical columns
  numeric_cols <- sapply(data, is.numeric)
  # Handle date types separately
  date_cols <- sapply(data, inherits, "Date") | sapply(data, inherits, "POSIXct") | sapply(data, inherits, "POSIXlt")
  categorical_cols <- !numeric_cols & !date_cols
  
  # Numeric summaries
  if (any(numeric_cols)) {
    num_data <- data[, numeric_cols, drop = FALSE]
    result$numeric_summary <- lapply(num_data, function(x) {
      stats <- list(
        mean = mean(x, na.rm = TRUE),
        median = stats::median(x, na.rm = TRUE),
        min = min(x, na.rm = TRUE),
        max = max(x, na.rm = TRUE),
        range = max(x, na.rm = TRUE) - min(x, na.rm = TRUE),
        sd = stats::sd(x, na.rm = TRUE),
        q1 = stats::quantile(x, 0.25, na.rm = TRUE),
        q3 = stats::quantile(x, 0.75, na.rm = TRUE),
        iqr = stats::IQR(x, na.rm = TRUE),
        missing = sum(is.na(x)),
        percent_missing = sum(is.na(x)) / length(x) * 100,
        zeros = sum(x == 0, na.rm = TRUE),
        negative = sum(x < 0, na.rm = TRUE),
        unique_count = length(unique(x[!is.na(x)]))
      )
      return(stats)
    })
  } else {
    result$numeric_summary <- NULL
  }
  
  # Categorical summaries
  if (any(categorical_cols)) {
    cat_data <- data[, categorical_cols, drop = FALSE]
    result$categorical_summary <- lapply(cat_data, function(x) {
      freq_table <- table(x, useNA = "ifany")
      stats <- list(
        n_unique = length(unique(x[!is.na(x)])),
        mode = names(sort(freq_table, decreasing = TRUE)[1]),
        frequencies = freq_table,
        missing = sum(is.na(x)),
        percent_missing = sum(is.na(x)) / length(x) * 100
      )
      return(stats)
    })
  } else {
    result$categorical_summary <- NULL
  }
  
  # Add distribution plots if requested
  if (include_plots && requireNamespace("ggplot2", quietly = TRUE)) {
    result$plots <- list()
    
    # Numeric plots
    if (any(numeric_cols)) {
      num_data <- data[, numeric_cols, drop = FALSE]
      result$plots$numeric <- lapply(names(num_data), function(col_name) {
        p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[col_name]])) +
          ggplot2::geom_histogram(fill = "steelblue", bins = 30) +
          ggplot2::labs(title = paste("Distribution of", col_name),
                        x = col_name, y = "Count") +
          ggplot2::theme_minimal()
        return(p)
      })
      names(result$plots$numeric) <- names(num_data)
    }
    
    # Categorical plots
    if (any(categorical_cols)) {
      cat_data <- data[, categorical_cols, drop = FALSE]
      result$plots$categorical <- lapply(names(cat_data), function(col_name) {
        if (length(unique(cat_data[[col_name]])) <= 30) {
          p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[col_name]])) +
            ggplot2::geom_bar(fill = "steelblue") +
            ggplot2::labs(title = paste("Distribution of", col_name),
                          x = col_name, y = "Count") +
            ggplot2::theme_minimal() +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
          return(p)
        } else {
          return(NULL)  # Too many categories for a meaningful bar plot
        }
      })
      names(result$plots$categorical) <- names(cat_data)
    }
  }
  
  class(result) <- c("data_summary", "list")
  return(result)
}

#' Print method for data_summary objects
#'
#' @param x A data_summary object
#' @param ... Additional arguments
#' @export
print.data_summary <- function(x, ...) {
  cat("Dataset Summary\n")
  cat("==============\n\n")
  
  cat("Dimensions:", x$overview$dimensions[1], "rows,", x$overview$dimensions[2], "columns\n")
  cat("Memory usage:", format(x$overview$memory_usage, units = "auto"), "\n\n")
  
  cat("Column Types:\n")
  type_table <- table(unlist(x$overview$column_types))
  for (type in names(type_table)) {
    cat(" -", type, ":", type_table[type], "columns\n")
  }
  cat("\n")
  
  if (!is.null(x$numeric_summary)) {
    cat("Numeric Variables Summary:\n")
    cat("-------------------------\n")
    for (col in names(x$numeric_summary)) {
      cat(col, ":\n")
      cat(" - Mean:", round(x$numeric_summary[[col]]$mean, 2), "\n")
      cat(" - Median:", round(x$numeric_summary[[col]]$median, 2), "\n")
      cat(" - Min/Max:", round(x$numeric_summary[[col]]$min, 2), "/", 
          round(x$numeric_summary[[col]]$max, 2), "\n")
      cat(" - Std Dev:", round(x$numeric_summary[[col]]$sd, 2), "\n")
      if (x$numeric_summary[[col]]$missing > 0) {
        cat(" - Missing:", x$numeric_summary[[col]]$missing, 
            "(", round(x$numeric_summary[[col]]$percent_missing, 1), "%)\n")
      }
      cat("\n")
    }
    
    # Add normalization recommendations
    cat("Normalization Recommendations:\n")
    cat("----------------------------\n")
    
    # Collect data for recommendations
    skewed_vars <- character(0)
    large_range_vars <- character(0)
    zero_centered_vars <- character(0)
    binary_vars <- character(0)
    normal_vars <- character(0)
    
    for (col in names(x$numeric_summary)) {
      # Calculate skewness
      if (!is.null(x$numeric_summary[[col]]$mean) && 
          !is.null(x$numeric_summary[[col]]$median) && 
          !is.null(x$numeric_summary[[col]]$sd)) {
        
        # Check if the variable is approximately binary (has only 0/1 or two unique values)
        if (x$numeric_summary[[col]]$unique_count <= 2 || 
            (abs(x$numeric_summary[[col]]$min) < 0.001 && abs(x$numeric_summary[[col]]$max - 1) < 0.001)) {
          binary_vars <- c(binary_vars, col)
          next
        }
        
        # Check if the variable is already approximately normally distributed
        # Using a simplified rule based on mean-median difference
        mean_median_diff <- abs(x$numeric_summary[[col]]$mean - x$numeric_summary[[col]]$median) / x$numeric_summary[[col]]$sd
        if (mean_median_diff < 0.2) {
          normal_vars <- c(normal_vars, col)
        }
        
        # Check for skewed variables
        if (mean_median_diff >= 0.3) {
          skewed_vars <- c(skewed_vars, col)
        }
        
        # Check for variables with large ranges
        range <- x$numeric_summary[[col]]$max - x$numeric_summary[[col]]$min
        if (range > 10 * x$numeric_summary[[col]]$sd) {
          large_range_vars <- c(large_range_vars, col)
        }
        
        # Check if variable is approximately zero-centered
        if (abs(x$numeric_summary[[col]]$mean) < 0.1 * x$numeric_summary[[col]]$sd) {
          zero_centered_vars <- c(zero_centered_vars, col)
        }
      }
    }
    
    # General recommendation based on variable types
    cat("1. General Recommendations:\n")
    
    # Case: Binary variables
    if (length(binary_vars) > 0) {
      cat("   - Binary variables (", paste(binary_vars, collapse = ", "), 
          "): No normalization needed for most algorithms.\n", sep = "")
    }
    
    # Case: Already normally distributed
    if (length(normal_vars) > 0) {
      cat("   - Normally distributed variables (", paste(normal_vars, collapse = ", "), 
          "): Standard scaling (z-score) is suitable.\n", sep = "")
    }
    
    # Case: Skewed variables
    if (length(skewed_vars) > 0) {
      cat("   - Skewed variables (", paste(skewed_vars, collapse = ", "), 
          "): Consider log, square root, or Box-Cox transformation.\n", sep = "")
    }
    
    # Case: Large range variables
    if (length(large_range_vars) > 0) {
      cat("   - Variables with large ranges (", paste(large_range_vars, collapse = ", "), 
          "): Consider min-max scaling or robust scaling.\n", sep = "")
    }
    
    # Normalization for different algorithms
    cat("\n2. Normalization by Algorithm Type:\n")
    cat("   - Distance-based algorithms (k-means, kNN, SVM): Use StandardScaler or MinMaxScaler\n")
    cat("   - Linear regression, logistic regression: Consider StandardScaler\n")
    cat("   - Tree-based models (Random Forest, XGBoost): Normalization typically not required\n")
    cat("   - Neural Networks: StandardScaler or MinMaxScaler (to range [-1, 1] or [0, 1])\n")
    cat("   - PCA, LDA and other dimension reduction: StandardScaler strongly recommended\n")
    
    # Scaling methods and their code examples
    cat("\n3. Implementation in R:\n")
    cat("   - Standard scaling (z-score):\n")
    cat("     ```r\n")
    cat("     # Base R\n")
    cat("     data_scaled <- scale(data[, numeric_cols])\n")
    cat("     \n")
    cat("     # Using caret\n")
    cat("     preproc <- caret::preProcess(data[, numeric_cols], method = c(\"center\", \"scale\"))\n")
    cat("     data_scaled <- predict(preproc, data[, numeric_cols])\n")
    cat("     ```\n")
    
    cat("   - Min-Max scaling:\n")
    cat("     ```r\n")
    cat("     # Base R\n")
    cat("     min_max_scale <- function(x) {\n")
    cat("       (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))\n")
    cat("     }\n")
    cat("     data_scaled <- as.data.frame(lapply(data[, numeric_cols], min_max_scale))\n")
    cat("     \n")
    cat("     # Using caret\n")
    cat("     preproc <- caret::preProcess(data[, numeric_cols], method = c(\"range\"))\n")
    cat("     data_scaled <- predict(preproc, data[, numeric_cols])\n")
    cat("     ```\n")
    
    cat("   - Log transformation (for skewed data):\n")
    cat("     ```r\n")
    cat("     # For strictly positive data\n")
    cat("     data_transformed <- log(data[, skewed_cols])\n")
    cat("     \n")
    cat("     # For data with zeros or negative values\n")
    cat("     data_transformed <- log(data[, skewed_cols] + offset) # where offset > |min(data)|\n")
    cat("     ```\n")
    
    cat("   - Box-Cox transformation (for skewed data):\n")
    cat("     ```r\n")
    cat("     # Using the MASS package\n")
    cat("     library(MASS)\n")
    cat("     bc <- boxcox(lm(var ~ 1, data = data))\n")
    cat("     lambda <- bc$x[which.max(bc$y)]\n")
    cat("     data_transformed <- (data$var^lambda - 1)/lambda # if lambda != 0\n")
    cat("     # or\n")
    cat("     data_transformed <- log(data$var) # if lambda == 0\n")
    cat("     ```\n")
    
    cat("\n4. Important Considerations:\n")
    cat("   - Apply the same transformation to training and test sets\n")
    cat("   - Store transformation parameters from training for later use\n")
    cat("   - Consider the interpretability needs of your project\n")
    cat("   - For highly skewed data, try multiple transformations and evaluate\n")
  }
  
  if (!is.null(x$categorical_summary)) {
    cat("Categorical Variables Summary:\n")
    cat("----------------------------\n")
    for (col in names(x$categorical_summary)) {
      cat(col, ":\n")
      cat(" - Unique values:", x$categorical_summary[[col]]$n_unique, "\n")
      cat(" - Mode:", x$categorical_summary[[col]]$mode, "\n")
      if (x$categorical_summary[[col]]$missing > 0) {
        cat(" - Missing:", x$categorical_summary[[col]]$missing, 
            "(", round(x$categorical_summary[[col]]$percent_missing, 1), "%)\n")
      }
      
      # Show frequency table for variables with few categories
      if (x$categorical_summary[[col]]$n_unique <= 10) {
        cat(" - Frequencies:\n")
        freq <- x$categorical_summary[[col]]$frequencies
        for (i in 1:length(freq)) {
          cat("   * ", names(freq)[i], ": ", freq[i], 
              " (", round(100 * freq[i] / sum(freq), 1), "%)\n", sep = "")
        }
      }
      cat("\n")
    }
  }
  
  if (!is.null(x$plots)) {
    cat("Plots are available in the result object. Access with result$plots\n")
  }
}