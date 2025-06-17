#' Analyze correlations between variables in a dataset
#'
#' This function computes and visualizes correlations between variables in a dataframe.
#' It handles numeric, categorical, and mixed data types.
#'
#' @param data A dataframe to analyze
#' @param method The correlation method to use for numeric variables: "pearson" (default), "spearman", or "kendall"
#' @param plot Logical, whether to create visualizations (default: TRUE)
#' @param threshold Numeric, threshold for highlighting strong correlations (default: 0.7)
#' @return A list containing correlation analysis results
#' @export
#'
#' @examples
#' correlation_analysis(mtcars)
#' correlation_analysis(iris, method = "spearman")
correlation_analysis <- function(data, method = "pearson", plot = TRUE, threshold = 0.7) {
  if (!is.data.frame(data)) {
    stop("Input must be a dataframe")
  }
  
  if (!method %in% c("pearson", "spearman", "kendall")) {
    stop("Method must be one of: 'pearson', 'spearman', 'kendall'")
  }
  
  # Create the result structure
  result <- list()
  
  # Identify numeric and categorical columns
  numeric_cols <- sapply(data, is.numeric)
  categorical_cols <- !numeric_cols
  
  # Numeric correlations
  if (sum(numeric_cols) > 1) {
    num_data <- data[, numeric_cols, drop = FALSE]
    result$numeric_correlation <- stats::cor(num_data, method = method, use = "pairwise.complete.obs")
    
    # Find strong correlations
    cor_matrix <- result$numeric_correlation
    diag(cor_matrix) <- 0  # Ignore self-correlations
    strong_cors <- which(abs(cor_matrix) >= threshold, arr.ind = TRUE)
    
    if (nrow(strong_cors) > 0) {
      # Create a dataframe with strong correlations
      strong_cors_df <- data.frame(
        var1 = colnames(num_data)[strong_cors[, 1]],
        var2 = colnames(num_data)[strong_cors[, 2]],
        correlation = cor_matrix[strong_cors]
      )
      
      # Remove duplicates (since the correlation matrix is symmetric)
      strong_cors_df <- strong_cors_df[!duplicated(t(apply(strong_cors_df[, 1:2], 1, sort))), ]
      
      # Sort by absolute correlation (strongest first)
      strong_cors_df <- strong_cors_df[order(-abs(strong_cors_df$correlation)), ]
      
      result$strong_correlations <- strong_cors_df
    } else {
      result$strong_correlations <- NULL
    }
  } else {
    result$numeric_correlation <- NULL
    result$strong_correlations <- NULL
  }
  
  # Categorical correlations (using Cramer's V)
  if (sum(categorical_cols) > 1) {
    cat_data <- data[, categorical_cols, drop = FALSE]
    cat_names <- names(cat_data)
    n_cat <- length(cat_names)
    
    # Initialize matrix for categorical correlations
    cat_cor <- matrix(1, nrow = n_cat, ncol = n_cat)
    colnames(cat_cor) <- cat_names
    rownames(cat_cor) <- cat_names
    
    # Compute Cramer's V for each pair of categorical variables
    for (i in 1:(n_cat-1)) {
      for (j in (i+1):n_cat) {
        # Create contingency table
        tbl <- table(cat_data[[cat_names[i]]], cat_data[[cat_names[j]]])
        
        # Calculate Cramer's V
        n <- sum(tbl)
        chi2 <- stats::chisq.test(tbl, correct = FALSE)$statistic
        phi2 <- chi2 / n
        
        # Get dimensions of table
        r <- nrow(tbl)
        c <- ncol(tbl)
        
        # Calculate Cramer's V
        cramer_v <- sqrt(phi2 / min(r-1, c-1))
        
        # Store in the matrix
        cat_cor[i, j] <- cramer_v
        cat_cor[j, i] <- cramer_v
      }
    }
    
    result$categorical_correlation <- cat_cor
  } else {
    result$categorical_correlation <- NULL
  }
  
  # Correlation between numeric and categorical (using correlation ratio eta)
  if (sum(numeric_cols) > 0 && sum(categorical_cols) > 0) {
    num_data <- data[, numeric_cols, drop = FALSE]
    cat_data <- data[, categorical_cols, drop = FALSE]
    
    num_names <- names(num_data)
    cat_names <- names(cat_data)
    
    # Initialize matrix for mixed correlations
    mixed_cor <- matrix(NA, nrow = length(num_names), ncol = length(cat_names))
    rownames(mixed_cor) <- num_names
    colnames(mixed_cor) <- cat_names
    
    # Calculate correlation ratio for each pair
    for (i in seq_along(num_names)) {
      for (j in seq_along(cat_names)) {
        # Get the numeric and categorical variables
        num_var <- num_data[[num_names[i]]]
        cat_var <- cat_data[[cat_names[j]]]
        
        # Skip if there are NAs
        if (sum(is.na(num_var)) > 0 || sum(is.na(cat_var)) > 0) {
          complete_idx <- complete.cases(num_var, cat_var)
          num_var <- num_var[complete_idx]
          cat_var <- cat_var[complete_idx]
        }
        
        # Calculate correlation ratio (eta)
        # Formula: eta² = (total variance - within group variance) / total variance
        total_variance <- stats::var(num_var)
        
        # Skip if variance is zero or near zero
        if (total_variance < .Machine$double.eps) {
          mixed_cor[i, j] <- 0
          next
        }
        
        # Calculate within-group variance
        group_means <- tapply(num_var, cat_var, mean)
        group_sizes <- tapply(num_var, cat_var, length)
        
        within_var <- 0
        for (group in names(group_means)) {
          group_idx <- cat_var == group
          group_var <- stats::var(num_var[group_idx])
          within_var <- within_var + group_var * (group_sizes[group] - 1)
        }
        within_var <- within_var / (length(num_var) - 1)
        
        # Calculate eta²
        eta_squared <- (total_variance - within_var) / total_variance
        
        # Calculate eta
        eta <- sqrt(eta_squared)
        
        # Store in the matrix
        mixed_cor[i, j] <- eta
      }
    }
    
    result$mixed_correlation <- mixed_cor
  } else {
    result$mixed_correlation <- NULL
  }
  
  # Create visualizations if requested
  if (plot) {
    result$plots <- list()
    
    if (!is.null(result$numeric_correlation) && requireNamespace("corrplot", quietly = TRUE)) {
      # Create a correlation plot using corrplot
      result$plots$numeric <- function() {
        corrplot::corrplot(result$numeric_correlation, method = "circle", 
                           type = "upper", tl.col = "black", tl.srt = 45)
      }
    }
    
    if (!is.null(result$categorical_correlation) && requireNamespace("corrplot", quietly = TRUE)) {
      # Create a correlation plot for categorical variables
      result$plots$categorical <- function() {
        corrplot::corrplot(result$categorical_correlation, method = "circle", 
                           type = "upper", tl.col = "black", tl.srt = 45, 
                           title = "Categorical Correlations (Cramer's V)")
      }
    }
    
    if (!is.null(result$mixed_correlation) && requireNamespace("corrplot", quietly = TRUE)) {
      # Create a correlation plot for mixed variables
      result$plots$mixed <- function() {
        corrplot::corrplot(result$mixed_correlation, method = "circle", 
                           tl.col = "black", tl.srt = 45, 
                           title = "Numeric-Categorical Correlations (Eta)")
      }
    }
    
    # Generate a pairplot for numeric variables if ggplot2 is available
    if (sum(numeric_cols) > 1 && requireNamespace("ggplot2", quietly = TRUE)) {
      # Convert to long format for easier plotting
      num_data <- data[, numeric_cols, drop = FALSE]
      
      # Only create pairplot if there are fewer than 10 numeric variables
      if (ncol(num_data) < 10) {
        result$plots$pairplot <- function() {
          # Create simple pairplot using pairs function
          pairs(num_data, pch = 19, col = "steelblue")
        }
      }
    }
  }
  
  class(result) <- c("correlation_analysis", "list")
  return(result)
}

#' Print method for correlation_analysis objects
#'
#' @param x A correlation_analysis object
#' @param ... Additional arguments
#' @export
print.correlation_analysis <- function(x, ...) {
  cat("Correlation Analysis\n")
  cat("===================\n\n")
  
  if (!is.null(x$numeric_correlation)) {
    cat("Numeric Variable Correlations:\n")
    cat("----------------------------\n")
    print(round(x$numeric_correlation, 2))
    cat("\n")
    
    if (!is.null(x$strong_correlations)) {
      cat("Strong Correlations:\n")
      cat("------------------\n")
      for (i in 1:nrow(x$strong_correlations)) {
        cat(x$strong_correlations$var1[i], "and", x$strong_correlations$var2[i], ":",
            round(x$strong_correlations$correlation[i], 2), "\n")
      }
      cat("\n")
    }
  }
  
  if (!is.null(x$categorical_correlation)) {
    cat("Categorical Variable Correlations (Cramer's V):\n")
    cat("--------------------------------------------\n")
    print(round(x$categorical_correlation, 2))
    cat("\n")
  }
  
  if (!is.null(x$mixed_correlation)) {
    cat("Numeric-Categorical Correlations (Correlation Ratio - Eta):\n")
    cat("-------------------------------------------------------\n")
    print(round(x$mixed_correlation, 2))
    cat("\n")
  }
  
  if (!is.null(x$plots)) {
    cat("Plots are available in the result object. Access with result$plots\n")
    cat("To display a plot, use: result$plots$numeric() or similar\n")
  }
}

#' Create a correlation plot for a dataset
#'
#' This is a simplified wrapper around correlation_analysis that directly returns a visualization.
#'
#' @param data A dataframe to analyze
#' @param method The correlation method to use for numeric variables: "pearson" (default), "spearman", or "kendall"
#' @param type The type of correlation to visualize: "numeric" (default), "categorical", "mixed", or "all"
#' @return A correlation plot
#' @export
#'
#' @examples
#' correlation_plot(mtcars)
#' correlation_plot(iris, method = "spearman", type = "all")
correlation_plot <- function(data, method = "pearson", type = "numeric") {
  if (!requireNamespace("corrplot", quietly = TRUE)) {
    stop("The 'corrplot' package is required but not installed. Install with install.packages('corrplot')")
  }
  
  # Run the correlation analysis
  result <- correlation_analysis(data, method = method, plot = TRUE)
  
  # Determine which plot to show
  if (type == "numeric" && !is.null(result$plots$numeric)) {
    result$plots$numeric()
  } else if (type == "categorical" && !is.null(result$plots$categorical)) {
    result$plots$categorical()
  } else if (type == "mixed" && !is.null(result$plots$mixed)) {
    result$plots$mixed()
  } else if (type == "all") {
    # Create a multi-panel plot
    if (!is.null(result$plots$numeric)) {
      result$plots$numeric()
    }
    if (!is.null(result$plots$categorical)) {
      result$plots$categorical()
    }
    if (!is.null(result$plots$mixed)) {
      result$plots$mixed()
    }
  } else {
    stop("No correlation plot available for the requested type or data")
  }
}