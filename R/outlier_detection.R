#' Detect outliers in a dataset using multiple methods
#'
#' This function implements several outlier detection methods and provides
#' visualizations to help identify outliers in the data.
#'
#' @param data A dataframe to analyze
#' @param methods Character vector of outlier detection methods to use:
#'   "iqr" (interquartile range), "zscore", "modified_zscore", "dbscan", or "all"
#' @param variables Character vector of specific variables to analyze for outliers.
#'   If NULL (default), all numeric variables will be analyzed.
#' @param threshold Numeric threshold for outlier detection. For z-score methods,
#'   this is the number of standard deviations. For IQR, this is the multiplier of IQR.
#' @param plot Logical, whether to create visualizations (default: TRUE)
#' @return A list containing outlier detection results
#' @export
#'
#' @examples
#' outlier_detection(mtcars)
#' outlier_detection(iris, methods = c("iqr", "zscore"))
outlier_detection <- function(data, methods = "all", variables = NULL, 
                             threshold = 3, plot = TRUE) {
  if (!is.data.frame(data)) {
    stop("Input must be a dataframe")
  }
  
  # Set all methods if requested
  if (identical(methods, "all")) {
    methods <- c("iqr", "zscore", "modified_zscore")
    
    # Only add DBSCAN if the dbscan package is available
    if (requireNamespace("dbscan", quietly = TRUE)) {
      methods <- c(methods, "dbscan")
    }
  }
  
  # Validate methods
  valid_methods <- c("iqr", "zscore", "modified_zscore", "dbscan")
  invalid_methods <- setdiff(methods, valid_methods)
  if (length(invalid_methods) > 0) {
    stop("Invalid methods: ", paste(invalid_methods, collapse = ", "), 
         ". Valid methods are: ", paste(valid_methods, collapse = ", "))
  }
  
  # Check if DBSCAN is requested but not available
  if ("dbscan" %in% methods && !requireNamespace("dbscan", quietly = TRUE)) {
    warning("The 'dbscan' package is required for DBSCAN outlier detection but is not installed. ",
            "DBSCAN method will be skipped. Install with install.packages('dbscan')")
    methods <- setdiff(methods, "dbscan")
  }
  
  # Identify numeric columns
  numeric_cols <- sapply(data, is.numeric)
  if (sum(numeric_cols) == 0) {
    stop("No numeric columns found in the data")
  }
  
  # If specific variables are provided, use only those that are numeric
  if (!is.null(variables)) {
    if (!all(variables %in% names(data))) {
      stop("Some specified variables are not present in the data")
    }
    numeric_cols <- numeric_cols & (names(data) %in% variables)
    if (sum(numeric_cols) == 0) {
      stop("None of the specified variables are numeric")
    }
  }
  
  num_data <- data[, numeric_cols, drop = FALSE]
  
  # Create the result structure
  result <- list()
  result$outliers <- list()
  
  # IQR method
  if ("iqr" %in% methods) {
    result$outliers$iqr <- lapply(num_data, function(x) {
      # Skip if variable has no variance
      if (all(is.na(x)) || stats::var(x, na.rm = TRUE) < .Machine$double.eps) {
        return(NULL)
      }
      
      q1 <- stats::quantile(x, 0.25, na.rm = TRUE)
      q3 <- stats::quantile(x, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      
      lower_bound <- q1 - threshold * iqr
      upper_bound <- q3 + threshold * iqr
      
      outliers <- which(x < lower_bound | x > upper_bound)
      
      if (length(outliers) == 0) {
        return(NULL)
      }
      
      list(
        indices = outliers,
        values = x[outliers],
        lower_bound = lower_bound,
        upper_bound = upper_bound
      )
    })
  }
  
  # Z-score method
  if ("zscore" %in% methods) {
    result$outliers$zscore <- lapply(num_data, function(x) {
      # Skip if variable has no variance
      if (all(is.na(x)) || stats::var(x, na.rm = TRUE) < .Machine$double.eps) {
        return(NULL)
      }
      
      mean_x <- mean(x, na.rm = TRUE)
      sd_x <- stats::sd(x, na.rm = TRUE)
      
      z_scores <- abs((x - mean_x) / sd_x)
      outliers <- which(z_scores > threshold)
      
      if (length(outliers) == 0) {
        return(NULL)
      }
      
      list(
        indices = outliers,
        values = x[outliers],
        z_scores = z_scores[outliers]
      )
    })
  }
  
  # Modified Z-score method
  if ("modified_zscore" %in% methods) {
    result$outliers$modified_zscore <- lapply(num_data, function(x) {
      # Skip if variable has no variance
      if (all(is.na(x)) || stats::var(x, na.rm = TRUE) < .Machine$double.eps) {
        return(NULL)
      }
      
      # Calculate median and MAD
      med_x <- stats::median(x, na.rm = TRUE)
      mad_x <- stats::mad(x, na.rm = TRUE)
      
      # If MAD is 0, can't calculate modified z-scores
      if (mad_x < .Machine$double.eps) {
        return(NULL)
      }
      
      # Calculate modified z-scores
      mod_z_scores <- 0.6745 * abs(x - med_x) / mad_x
      outliers <- which(mod_z_scores > threshold)
      
      if (length(outliers) == 0) {
        return(NULL)
      }
      
      list(
        indices = outliers,
        values = x[outliers],
        mod_z_scores = mod_z_scores[outliers]
      )
    })
  }
  
  # DBSCAN method (requires the dbscan package)
  if ("dbscan" %in% methods) {
    # Only use numeric columns with non-zero variance
    valid_cols <- sapply(num_data, function(x) {
      !all(is.na(x)) && stats::var(x, na.rm = TRUE) > .Machine$double.eps
    })
    
    if (sum(valid_cols) > 0) {
      # Prepare data for DBSCAN (normalize and handle missing values)
      db_data <- as.data.frame(lapply(num_data[, valid_cols, drop = FALSE], function(x) {
        # Normalize to [0,1] range
        (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
      }))
      
      # Handle missing values
      if (any(is.na(db_data))) {
        complete_rows <- complete.cases(db_data)
        if (sum(complete_rows) < 3) {
          # Not enough complete cases for DBSCAN
          result$outliers$dbscan <- NULL
        } else {
          # Perform DBSCAN on complete cases
          db_result <- dbscan::dbscan(db_data[complete_rows, , drop = FALSE], eps = 0.5, minPts = 3)
          
          # Outliers are points with cluster label 0
          outliers <- which(complete_rows)[db_result$cluster == 0]
          
          if (length(outliers) == 0) {
            result$outliers$dbscan <- NULL
          } else {
            result$outliers$dbscan <- list(
              indices = outliers,
              values = data[outliers, , drop = FALSE]
            )
          }
        }
      } else {
        # No missing values, perform DBSCAN on all data
        db_result <- dbscan::dbscan(db_data, eps = 0.5, minPts = 3)
        
        # Outliers are points with cluster label 0
        outliers <- which(db_result$cluster == 0)
        
        if (length(outliers) == 0) {
          result$outliers$dbscan <- NULL
        } else {
          result$outliers$dbscan <- list(
            indices = outliers,
            values = data[outliers, , drop = FALSE]
          )
        }
      }
    } else {
      result$outliers$dbscan <- NULL
    }
  }
  
  # Summarize outliers
  result$summary <- list()
  
  # Count outliers per method
  for (method in names(result$outliers)) {
    # Skip NULL entries
    if (is.null(result$outliers[[method]])) {
      result$summary[[method]] <- 0
      next
    }
    
    # Count unique indices across all variables
    all_indices <- unique(unlist(lapply(result$outliers[[method]], function(x) {
      if (is.null(x)) return(NULL)
      x$indices
    })))
    
    result$summary[[method]] <- length(all_indices)
  }
  
  # Count outliers per variable
  result$summary$per_variable <- sapply(names(num_data), function(var) {
    counts <- sapply(names(result$outliers), function(method) {
      if (is.null(result$outliers[[method]][[var]])) {
        return(0)
      }
      length(result$outliers[[method]][[var]]$indices)
    })
    counts
  })
  
  # Create visualizations if requested
  if (plot && requireNamespace("ggplot2", quietly = TRUE)) {
    result$plots <- list()
    
    # Boxplots for each variable
    for (var in names(num_data)) {
      # Skip variables with no data or variance
      if (all(is.na(num_data[[var]])) || 
          stats::var(num_data[[var]], na.rm = TRUE) < .Machine$double.eps) {
        next
      }
      
      # Create a data frame for plotting
      plot_data <- data.frame(
        value = num_data[[var]],
        row = 1:nrow(data)
      )
      
      # Add outlier flags from different methods
      for (method in names(result$outliers)) {
        if (!is.null(result$outliers[[method]][[var]])) {
          flag_col <- paste0("outlier_", method)
          plot_data[[flag_col]] <- FALSE
          plot_data[result$outliers[[method]][[var]]$indices, flag_col] <- TRUE
        }
      }
      
      # Create a boxplot
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = "", y = value)) +
        ggplot2::geom_boxplot(outlier.color = "red") +
        ggplot2::labs(title = paste("Distribution of", var),
                     x = "", y = var) +
        ggplot2::theme_minimal()
      
      result$plots[[var]] <- p
      
      # Add a scatterplot if there are outliers
      outlier_indices <- unique(unlist(lapply(result$outliers, function(method_result) {
        if (is.null(method_result[[var]])) return(NULL)
        method_result[[var]]$indices
      })))
      
      if (length(outlier_indices) > 0) {
        plot_data$is_outlier <- FALSE
        plot_data$is_outlier[outlier_indices] <- TRUE
        
        p_scatter <- ggplot2::ggplot(plot_data, ggplot2::aes(x = row, y = value, color = is_outlier)) +
          ggplot2::geom_point() +
          ggplot2::scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "red")) +
          ggplot2::labs(title = paste("Outliers in", var),
                       x = "Observation", y = var) +
          ggplot2::theme_minimal()
        
        result$plots[[paste0(var, "_scatter")]] <- p_scatter
      }
    }
    
    # If DBSCAN was used and there are at least 2 valid numeric variables, create a scatterplot matrix
    if ("dbscan" %in% methods && sum(valid_cols) >= 2) {
      # Get outlier indices from DBSCAN
      dbscan_outliers <- if (!is.null(result$outliers$dbscan)) result$outliers$dbscan$indices else NULL
      
      if (!is.null(dbscan_outliers) && length(dbscan_outliers) > 0) {
        # Create a color vector for points
        colors <- rep("steelblue", nrow(data))
        colors[dbscan_outliers] <- "red"
        
        # Create pairwise scatterplots
        result$plots$dbscan_pairplot <- function() {
          pairs(num_data[, valid_cols], pch = 19, col = colors,
                main = "DBSCAN Outlier Detection")
        }
      }
    }
  }
  
  class(result) <- c("outlier_detection", "list")
  return(result)
}

#' Print method for outlier_detection objects
#'
#' @param x An outlier_detection object
#' @param ... Additional arguments
#' @export
print.outlier_detection <- function(x, ...) {
  cat("Outlier Detection Results\n")
  cat("=======================\n\n")
  
  # Summary of outliers by method
  cat("Outliers by Method:\n")
  cat("-----------------\n")
  for (method in names(x$summary)) {
    if (method != "per_variable") {
      cat(method, ":", x$summary[[method]], "outliers\n")
    }
  }
  cat("\n")
  
  # Summary of outliers by variable
  if (!is.null(x$summary$per_variable) && is.matrix(x$summary$per_variable) && ncol(x$summary$per_variable) > 0) {
    cat("Outliers by Variable:\n")
    cat("------------------\n")
    
    for (var in colnames(x$summary$per_variable)) {
      var_total <- sum(x$summary$per_variable[, var] > 0, na.rm = TRUE)
      if (var_total > 0) {
        cat(var, ":", var_total, "method(s) detected outliers\n")
        
        for (method in rownames(x$summary$per_variable)) {
          if (!is.na(x$summary$per_variable[method, var]) && x$summary$per_variable[method, var] > 0) {
            cat(" -", method, ":", x$summary$per_variable[method, var], "outliers\n")
            
            # Show some example outliers
            outliers <- x$outliers[[method]][[var]]
            if (!is.null(outliers)) {
              n_show <- min(5, length(outliers$indices))
              cat("   Examples: ")
              for (i in 1:n_show) {
                cat("row", outliers$indices[i], "=", round(outliers$values[i], 2))
                if (i < n_show) cat(", ")
              }
              if (length(outliers$indices) > n_show) cat(", ...")
              cat("\n")
            }
          }
        }
        cat("\n")
      }
    }
  }
  
  if (!is.null(x$plots)) {
    cat("Plots are available in the result object. Access with result$plots\n")
  }
}