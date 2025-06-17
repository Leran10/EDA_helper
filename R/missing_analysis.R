#' Analyze missing values in a dataset
#'
#' This function provides a comprehensive analysis of missing values in a dataframe,
#' including summary statistics and visualizations.
#'
#' @param data A dataframe to analyze
#' @param plot Logical, whether to create visualizations (default: TRUE)
#' @return A list containing missing value analysis results
#' @export
#'
#' @examples
#' # Create a dataset with missing values
#' df <- data.frame(
#'   x = c(1, 2, NA, 4, 5),
#'   y = c(NA, 2, 3, NA, 5),
#'   z = c(1, NA, 3, 4, NA)
#' )
#' missing_analysis(df)
missing_analysis <- function(data, plot = TRUE) {
  if (!is.data.frame(data)) {
    stop("Input must be a dataframe")
  }
  
  # Create the result structure
  result <- list()
  
  # Store the original data for later use in recommendations
  result$data <- data
  
  # Overall missing summary
  total_cells <- prod(dim(data))
  missing_cells <- sum(is.na(data))
  result$overall <- list(
    total_cells = total_cells,
    missing_cells = missing_cells,
    missing_percent = missing_cells / total_cells * 100,
    complete_rows = sum(complete.cases(data)),
    incomplete_rows = nrow(data) - sum(complete.cases(data)),
    incomplete_row_percent = (nrow(data) - sum(complete.cases(data))) / nrow(data) * 100
  )
  
  # Column-wise missing values
  result$columns <- lapply(data, function(x) {
    list(
      missing_count = sum(is.na(x)),
      missing_percent = sum(is.na(x)) / length(x) * 100
    )
  })
  
  # Missing patterns
  # Create a binary matrix of missing values (TRUE/FALSE)
  na_matrix <- is.na(data)
  
  # Identify unique patterns of missingness
  pattern_strings <- apply(na_matrix, 1, function(x) paste(as.integer(x), collapse = ""))
  unique_patterns <- unique(pattern_strings)
  
  # Count occurrences of each pattern
  pattern_counts <- table(pattern_strings)
  
  # Decode patterns back to TRUE/FALSE matrices
  patterns <- lapply(names(pattern_counts), function(p) {
    pattern <- as.integer(strsplit(p, "")[[1]]) == 1
    names(pattern) <- colnames(data)
    return(pattern)
  })
  
  result$patterns <- list(
    unique_count = length(unique_patterns),
    patterns = patterns,
    pattern_counts = as.vector(pattern_counts),
    pattern_percentages = as.vector(pattern_counts) / nrow(data) * 100
  )
  
  # Missing value correlation
  if (missing_cells > 0 && ncol(data) > 1) {
    # Create a matrix of missingness indicators
    na_indicators <- as.data.frame(lapply(data, function(x) as.integer(is.na(x))))
    
    # Calculate correlation between missingness indicators
    result$correlation <- stats::cor(na_indicators)
  } else {
    result$correlation <- NULL
  }
  
  # Create visualizations if requested
  if (plot && requireNamespace("ggplot2", quietly = TRUE)) {
    result$plots <- list()
    
    # Only create plots if there are missing values
    if (missing_cells > 0) {
      # Convert the data to long format for plotting
      if (requireNamespace("tidyr", quietly = TRUE)) {
        # Prepare data for missing values heatmap
        missing_data <- data.frame(
          row = rep(1:nrow(data), ncol(data)),
          col = rep(colnames(data), each = nrow(data)),
          is_missing = as.vector(is.na(data))
        )
        
        # Missing values heatmap
        result$plots$heatmap <- ggplot2::ggplot(missing_data, 
                                               ggplot2::aes(x = col, y = row, fill = is_missing)) +
          ggplot2::geom_tile() +
          ggplot2::scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "steelblue"),
                                    name = "Missing") +
          ggplot2::labs(title = "Missing Values Heatmap",
                       x = "Variables", y = "Observations") +
          ggplot2::theme_minimal() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
        
        # Missing values by column
        col_missing <- data.frame(
          column = names(result$columns),
          missing_percent = sapply(result$columns, function(x) x$missing_percent)
        )
        
        result$plots$column_plot <- ggplot2::ggplot(col_missing, 
                                                   ggplot2::aes(x = stats::reorder(column, missing_percent), 
                                                              y = missing_percent)) +
          ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
          ggplot2::labs(title = "Missing Values by Column",
                       x = "Column", y = "Missing (%)") +
          ggplot2::theme_minimal() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
          ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey")
        
        # Missing correlation heatmap
        if (!is.null(result$correlation)) {
          corr_data <- as.data.frame(as.table(result$correlation))
          names(corr_data) <- c("Var1", "Var2", "Correlation")
          
          result$plots$correlation <- ggplot2::ggplot(corr_data, 
                                                     ggplot2::aes(x = Var1, y = Var2, fill = Correlation)) +
            ggplot2::geom_tile() +
            ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                                         midpoint = 0, limits = c(-1, 1)) +
            ggplot2::labs(title = "Missing Value Correlation Heatmap") +
            ggplot2::theme_minimal() +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
        }
      }
    }
  }
  
  class(result) <- c("missing_analysis", "list")
  return(result)
}

#' Print method for missing_analysis objects
#'
#' @param x A missing_analysis object
#' @param ... Additional arguments
#' @export
print.missing_analysis <- function(x, ...) {
  cat("Missing Value Analysis\n")
  cat("====================\n\n")
  
  cat("Overall Summary:\n")
  cat("--------------\n")
  cat("Total cells:", x$overall$total_cells, "\n")
  cat("Missing cells:", x$overall$missing_cells, 
      "(", round(x$overall$missing_percent, 1), "%)\n")
  cat("Complete rows:", x$overall$complete_rows, 
      "(", round(100 - x$overall$incomplete_row_percent, 1), "%)\n")
  cat("Incomplete rows:", x$overall$incomplete_rows, 
      "(", round(x$overall$incomplete_row_percent, 1), "%)\n\n")
  
  cat("Missing Values by Column:\n")
  cat("----------------------\n")
  for (col in names(x$columns)) {
    if (x$columns[[col]]$missing_count > 0) {
      cat(col, ":", x$columns[[col]]$missing_count, 
          "(", round(x$columns[[col]]$missing_percent, 1), "%)\n")
    } else {
      cat(col, ": No missing values\n")
    }
  }
  cat("\n")
  
  if (x$patterns$unique_count > 1) {
    cat("Missing Value Patterns:\n")
    cat("--------------------\n")
    cat("Found", x$patterns$unique_count, "unique missingness patterns\n")
    
    for (i in 1:min(5, length(x$patterns$patterns))) {
      cat("Pattern", i, ":", x$patterns$pattern_counts[i], "rows", 
          "(", round(x$patterns$pattern_percentages[i], 1), "%)\n")
      missing_cols <- names(which(x$patterns$patterns[[i]]))
      if (length(missing_cols) > 0) {
        cat(" - Missing in:", paste(missing_cols, collapse = ", "), "\n")
      } else {
        cat(" - All values present\n")
      }
    }
    
    if (length(x$patterns$patterns) > 5) {
      cat("(Only showing top 5 patterns)\n")
    }
    cat("\n")
  }
  
  if (!is.null(x$correlation)) {
    cat("Missing Value Correlation:\n")
    cat("----------------------\n")
    cat("Correlation matrix is available in the result object\n")
    cat("Access with result$correlation\n\n")
  }
  
  if (!is.null(x$plots)) {
    cat("Plots are available in the result object\n")
    cat("Access with result$plots\n")
  }
  
  # Add recommendations for handling missing values
  if (x$overall$missing_cells > 0) {
    cat("\nRecommendations for Handling Missing Values:\n")
    cat("------------------------------------------\n")
    
    # Assess severity of missingness
    severe_missingness <- FALSE
    severe_cols <- character(0)
    for (col in names(x$columns)) {
      if (x$columns[[col]]$missing_percent > 20) {
        severe_missingness <- TRUE
        severe_cols <- c(severe_cols, col)
      }
    }
    
    # Check if missing values are MCAR, MAR, or MNAR
    if (!is.null(x$correlation)) {
      has_correlation <- any(abs(x$correlation[upper.tri(x$correlation)]) > 0.3, na.rm = TRUE)
    } else {
      has_correlation <- FALSE
    }
    
    # Recommendations based on amount of missingness
    if (x$overall$missing_percent < 5) {
      cat("1. Low overall missingness (", round(x$overall$missing_percent, 1), 
          "%) - You have several options:\n", sep = "")
    } else if (x$overall$missing_percent < 15) {
      cat("1. Moderate overall missingness (", round(x$overall$missing_percent, 1), 
          "%) - Consider these approaches:\n", sep = "")
    } else {
      cat("1. High overall missingness (", round(x$overall$missing_percent, 1), 
          "%) - This requires careful handling:\n", sep = "")
    }
    
    # Listwise deletion (complete case analysis)
    if (x$overall$incomplete_row_percent < 10) {
      cat("   - Row deletion (complete case analysis) is a reasonable option since only ", 
          round(x$overall$incomplete_row_percent, 1), "% of rows have missing values.\n", sep = "")
    } else {
      cat("   - Row deletion is not recommended as it would remove ", 
          round(x$overall$incomplete_row_percent, 1), "% of your data.\n", sep = "")
    }
    
    # Column deletion
    if (length(severe_cols) > 0) {
      cat("   - Consider removing columns with excessive missingness: ", 
          paste(severe_cols, collapse = ", "), 
          " (if these variables are not critical for your analysis).\n", sep = "")
    }
    
    # Imputation recommendations
    cat("\n2. Imputation methods to consider:\n")
    
    # Numeric vs categorical columns with missing values
    missing_numeric_cols <- character(0)
    missing_categorical_cols <- character(0)
    
    for (col in names(x$columns)) {
      if (x$columns[[col]]$missing_count > 0) {
        if (is.numeric(x$data[[col]])) {
          missing_numeric_cols <- c(missing_numeric_cols, col)
        } else {
          missing_categorical_cols <- c(missing_categorical_cols, col)
        }
      }
    }
    
    # For numeric columns
    if (length(missing_numeric_cols) > 0) {
      cat("   For numeric variables (", paste(missing_numeric_cols, collapse = ", "), "):\n", sep = "")
      
      if (has_correlation) {
        cat("   - Multiple Imputation (MI): Recommended due to correlations between missingness patterns.\n")
        cat("     Use the 'mice' package: mice::mice(data, m=5, method='pmm')\n")
        cat("   - KNN imputation: Works well for data with correlations.\n")
        cat("     Use the 'VIM' package: VIM::kNN(data)\n")
      } else {
        cat("   - Mean/median imputation: Simple approach for randomly missing values.\n")
        cat("     Use: data$column[is.na(data$column)] <- mean(data$column, na.rm=TRUE)\n")
        cat("   - Predictive mean matching: More robust than simple mean imputation.\n")
        cat("     Use the 'mice' package with method='pmm'\n")
      }
    }
    
    # For categorical columns
    if (length(missing_categorical_cols) > 0) {
      cat("   For categorical variables (", paste(missing_categorical_cols, collapse = ", "), "):\n", sep = "")
      cat("   - Mode imputation: Replace with most frequent category.\n")
      cat("   - Create a new 'Missing' category: Treats missingness as informative.\n")
      cat("   - Multiple Imputation: Use the 'mice' package with method='polyreg' for factors.\n")
    }
    
    # Advanced methods
    cat("\n3. Advanced considerations:\n")
    if (has_correlation) {
      cat("   - Your missing data appears to have patterns (MAR - Missing At Random).\n")
      cat("   - Consider multiple imputation approaches that account for relationships between variables.\n")
    } else {
      cat("   - Your missing data may be MCAR (Missing Completely At Random).\n")
      cat("   - Simpler imputation methods may be sufficient.\n")
    }
    
    if (x$patterns$unique_count > 2) {
      cat("   - Multiple missingness patterns detected, which suggests complex mechanisms.\n")
      cat("   - Consider using multiple imputation methods that can handle this complexity.\n")
    }
    
    # Package recommendations
    cat("\n4. Recommended R packages for missing data:\n")
    cat("   - mice: Multiple Imputation by Chained Equations\n")
    cat("   - VIM: Visualization and Imputation of Missing Values\n")
    cat("   - missForest: Non-parametric Missing Value Imputation using Random Forest\n")
    cat("   - naniar: Data Structures, Summaries, and Visualizations for Missing Data\n")
    cat("   - Amelia: Multiple Imputation of Incomplete Multivariate Data\n")
  }
}