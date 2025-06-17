#' Preprocess data with imputation and encoding
#'
#' This function performs common data preprocessing tasks, including imputation
#' of missing values and encoding of categorical variables.
#'
#' @param data A dataframe to preprocess
#' @param impute_numeric Character, the method for imputing numeric missing values:
#'   "mean", "median", "mode", "knn", or "none" (default: "median")
#' @param impute_categorical Character, the method for imputing categorical missing values:
#'   "mode", "new_category", or "none" (default: "mode")
#' @param encode_categorical Character, the method for encoding categorical variables:
#'   "one_hot", "label", "binary", or "none" (default: "none")
#' @param drop_original Logical, whether to drop original categorical columns after encoding (default: TRUE)
#' @param scale Logical, whether to scale numeric variables (default: FALSE)
#' @param center Logical, whether to center numeric variables (default: FALSE)
#' @param drop_cols Character vector of column names to drop (default: NULL)
#' @param fill_value Numeric or character, the value to use for filling missing values manually (default: NULL)
#' @param impute_threshold Numeric, the maximum percentage of missing values to impute (default: 50)
#' @param knn_k Integer, the number of neighbors to consider for KNN imputation (default: 5)
#' @param verbose Logical, whether to print progress messages (default: TRUE)
#'
#' @return A list with:
#'  \itemize{
#'    \item processed_data: The preprocessed dataframe
#'    \item preprocessing_info: Information about the preprocessing steps
#'    \item original_data: The original dataframe (for reference)
#'  }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic imputation and encoding
#' processed <- preprocess_data(
#'   iris,
#'   impute_numeric = "median",
#'   impute_categorical = "mode",
#'   encode_categorical = "one_hot"
#' )
#'
#' # With scaling and centering
#' processed <- preprocess_data(
#'   mtcars,
#'   impute_numeric = "knn",
#'   scale = TRUE,
#'   center = TRUE
#' )
#' }
preprocess_data <- function(data, 
                            impute_numeric = "median",
                            impute_categorical = "mode",
                            encode_categorical = "none",
                            drop_original = TRUE,
                            scale = FALSE,
                            center = FALSE,
                            drop_cols = NULL,
                            fill_value = NULL,
                            impute_threshold = 50,
                            knn_k = 5,
                            verbose = TRUE) {
  
  # Validate input parameters
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  if (!impute_numeric %in% c("mean", "median", "mode", "knn", "none")) {
    stop("impute_numeric must be one of: 'mean', 'median', 'mode', 'knn', or 'none'")
  }
  
  if (!impute_categorical %in% c("mode", "new_category", "none")) {
    stop("impute_categorical must be one of: 'mode', 'new_category', or 'none'")
  }
  
  if (!encode_categorical %in% c("one_hot", "label", "binary", "none")) {
    stop("encode_categorical must be one of: 'one_hot', 'label', 'binary', or 'none'")
  }
  
  # Store original data
  original_data <- data
  
  # Initialize result object to track preprocessing info
  preprocessing_info <- list(
    columns_dropped = character(0),
    numeric_imputation = list(method = impute_numeric, columns = list()),
    categorical_imputation = list(method = impute_categorical, columns = list()),
    encoding = list(method = encode_categorical, columns = list()),
    scaling = list(applied = scale, columns = character(0)),
    centering = list(applied = center, columns = character(0))
  )
  
  # Make a copy of the data to avoid modifying the original
  processed_data <- data
  
  # Drop columns if specified
  if (!is.null(drop_cols) && length(drop_cols) > 0) {
    valid_drop_cols <- drop_cols[drop_cols %in% names(processed_data)]
    if (length(valid_drop_cols) > 0) {
      processed_data <- processed_data[, !names(processed_data) %in% valid_drop_cols, drop = FALSE]
      preprocessing_info$columns_dropped <- valid_drop_cols
      if (verbose) {
        message("Dropped columns: ", paste(valid_drop_cols, collapse = ", "))
      }
    }
  }
  
  # Identify column types
  numeric_cols <- sapply(processed_data, is.numeric)
  categorical_cols <- sapply(processed_data, function(x) is.factor(x) || is.character(x) || is.logical(x))
  date_cols <- sapply(processed_data, function(x) inherits(x, "Date") || inherits(x, "POSIXct"))
  
  # Calculate missing percentages
  missing_percentages <- sapply(processed_data, function(x) sum(is.na(x)) / length(x) * 100)
  
  # Process numeric columns
  if (any(numeric_cols) && impute_numeric != "none") {
    numeric_col_names <- names(processed_data)[numeric_cols]
    
    for (col in numeric_col_names) {
      # Skip columns with too many missing values
      if (missing_percentages[col] > impute_threshold) {
        if (verbose) {
          message("Skipping imputation for '", col, "' (missing: ", 
                  round(missing_percentages[col], 1), "% > threshold: ", impute_threshold, "%)")
        }
        next
      }
      
      # Skip if no missing values
      if (sum(is.na(processed_data[[col]])) == 0) {
        next
      }
      
      # Store column info before imputation
      preprocessing_info$numeric_imputation$columns[[col]] <- list(
        missing_count = sum(is.na(processed_data[[col]])),
        missing_percent = missing_percentages[col]
      )
      
      # Impute based on chosen method
      if (impute_numeric == "mean") {
        impute_value <- mean(processed_data[[col]], na.rm = TRUE)
        processed_data[[col]][is.na(processed_data[[col]])] <- impute_value
        preprocessing_info$numeric_imputation$columns[[col]]$impute_value <- impute_value
        
      } else if (impute_numeric == "median") {
        impute_value <- median(processed_data[[col]], na.rm = TRUE)
        processed_data[[col]][is.na(processed_data[[col]])] <- impute_value
        preprocessing_info$numeric_imputation$columns[[col]]$impute_value <- impute_value
        
      } else if (impute_numeric == "mode") {
        # Calculate mode
        tab <- table(processed_data[[col]])
        impute_value <- as.numeric(names(tab)[which.max(tab)])
        processed_data[[col]][is.na(processed_data[[col]])] <- impute_value
        preprocessing_info$numeric_imputation$columns[[col]]$impute_value <- impute_value
        
      } else if (impute_numeric == "knn") {
        # For KNN imputation, use only numeric columns
        # We need the VIM package for KNN imputation
        if (!requireNamespace("VIM", quietly = TRUE)) {
          warning("Package 'VIM' is required for KNN imputation but not installed. Falling back to median imputation.")
          impute_value <- median(processed_data[[col]], na.rm = TRUE)
          processed_data[[col]][is.na(processed_data[[col]])] <- impute_value
          preprocessing_info$numeric_imputation$columns[[col]]$impute_value <- impute_value
          preprocessing_info$numeric_imputation$columns[[col]]$method <- "median (fallback)"
        } else {
          # Extract numeric columns only for KNN
          numeric_data <- processed_data[, numeric_cols, drop = FALSE]
          
          # Check if we have enough complete cases for KNN
          complete_cases <- sum(complete.cases(numeric_data))
          if (complete_cases < knn_k + 1) {
            warning("Not enough complete cases for KNN imputation. Falling back to median imputation.")
            impute_value <- median(processed_data[[col]], na.rm = TRUE)
            processed_data[[col]][is.na(processed_data[[col]])] <- impute_value
            preprocessing_info$numeric_imputation$columns[[col]]$impute_value <- impute_value
            preprocessing_info$numeric_imputation$columns[[col]]$method <- "median (fallback)"
          } else {
            # Perform KNN imputation
            tryCatch({
              imputed <- VIM::kNN(numeric_data, k = knn_k)
              processed_data[[col]] <- imputed[[col]]
              preprocessing_info$numeric_imputation$columns[[col]]$method <- paste0("knn (k=", knn_k, ")")
            }, error = function(e) {
              warning("Error in KNN imputation: ", e$message, ". Falling back to median imputation.")
              impute_value <- median(processed_data[[col]], na.rm = TRUE)
              processed_data[[col]][is.na(processed_data[[col]])] <- impute_value
              preprocessing_info$numeric_imputation$columns[[col]]$impute_value <- impute_value
              preprocessing_info$numeric_imputation$columns[[col]]$method <- "median (fallback)"
            })
          }
        }
      }
    }
    
    if (verbose) {
      imputed_cols <- names(preprocessing_info$numeric_imputation$columns)
      if (length(imputed_cols) > 0) {
        message("Imputed missing values in numeric columns: ", 
                paste(imputed_cols, collapse = ", "), 
                " using method: ", impute_numeric)
      }
    }
  }
  
  # Process categorical columns
  if (any(categorical_cols) && impute_categorical != "none") {
    categorical_col_names <- names(processed_data)[categorical_cols]
    
    for (col in categorical_col_names) {
      # Skip columns with too many missing values
      if (missing_percentages[col] > impute_threshold) {
        if (verbose) {
          message("Skipping imputation for '", col, "' (missing: ", 
                  round(missing_percentages[col], 1), "% > threshold: ", impute_threshold, "%)")
        }
        next
      }
      
      # Skip if no missing values
      if (sum(is.na(processed_data[[col]])) == 0) {
        next
      }
      
      # Store column info before imputation
      preprocessing_info$categorical_imputation$columns[[col]] <- list(
        missing_count = sum(is.na(processed_data[[col]])),
        missing_percent = missing_percentages[col]
      )
      
      # Impute based on chosen method
      if (impute_categorical == "mode") {
        # Calculate mode
        tab <- table(processed_data[[col]], useNA = "no")
        if (length(tab) > 0) {
          impute_value <- names(tab)[which.max(tab)]
          processed_data[[col]][is.na(processed_data[[col]])] <- impute_value
          preprocessing_info$categorical_imputation$columns[[col]]$impute_value <- impute_value
        }
        
      } else if (impute_categorical == "new_category") {
        # Create a new category for missing values
        if (is.factor(processed_data[[col]])) {
          # For factors, add a new level
          processed_data[[col]] <- factor(processed_data[[col]], 
                                        levels = c(levels(processed_data[[col]]), "Missing"))
          processed_data[[col]][is.na(processed_data[[col]])] <- "Missing"
        } else {
          # For character or logical, convert to character and set to "Missing"
          processed_data[[col]] <- as.character(processed_data[[col]])
          processed_data[[col]][is.na(processed_data[[col]])] <- "Missing"
        }
        preprocessing_info$categorical_imputation$columns[[col]]$impute_value <- "Missing"
      }
    }
    
    if (verbose) {
      imputed_cols <- names(preprocessing_info$categorical_imputation$columns)
      if (length(imputed_cols) > 0) {
        message("Imputed missing values in categorical columns: ", 
                paste(imputed_cols, collapse = ", "), 
                " using method: ", impute_categorical)
      }
    }
  }
  
  # Manual filling of missing values (overrides other methods)
  if (!is.null(fill_value)) {
    # Find columns with missing values
    cols_with_na <- names(which(sapply(processed_data, function(x) any(is.na(x)))))
    
    if (length(cols_with_na) > 0) {
      for (col in cols_with_na) {
        # Skip columns with too many missing values
        if (missing_percentages[col] > impute_threshold) {
          next
        }
        
        # Attempt to fill with the provided value
        tryCatch({
          processed_data[[col]][is.na(processed_data[[col]])] <- fill_value
          method_key <- ifelse(is.numeric(processed_data[[col]]), "numeric_imputation", "categorical_imputation")
          preprocessing_info[[method_key]]$columns[[col]] <- list(
            missing_count = sum(is.na(original_data[[col]])),
            missing_percent = missing_percentages[col],
            impute_value = fill_value,
            method = "manual"
          )
        }, error = function(e) {
          warning("Could not fill '", col, "' with the provided value: ", e$message)
        })
      }
      
      if (verbose) {
        message("Manually filled missing values with: ", fill_value)
      }
    }
  }
  
  # Encoding categorical variables
  if (any(categorical_cols) && encode_categorical != "none") {
    categorical_col_names <- names(processed_data)[categorical_cols]
    
    if (encode_categorical == "one_hot") {
      # Check if we have the model.matrix function
      for (col in categorical_col_names) {
        # Skip columns with too many unique values
        n_unique <- length(unique(na.omit(processed_data[[col]])))
        if (n_unique > 10) {
          if (verbose) {
            message("Skipping one-hot encoding for '", col, "' (", n_unique, " unique values > 10)")
          }
          next
        }
        
        # Convert to factor if character
        if (is.character(processed_data[[col]])) {
          processed_data[[col]] <- as.factor(processed_data[[col]])
        }
        
        # Get dummy variables
        tryCatch({
          # Create formula for model.matrix
          formula_str <- paste0("~ 0 + ", col)
          formula_obj <- stats::as.formula(formula_str)
          
          # Create dummy variables
          dummy_vars <- stats::model.matrix(formula_obj, data = processed_data)
          
          # Add dummy variables to processed data
          for (i in 1:ncol(dummy_vars)) {
            new_col_name <- gsub(" ", "_", colnames(dummy_vars)[i])
            processed_data[[new_col_name]] <- dummy_vars[, i]
          }
          
          # Record info
          preprocessing_info$encoding$columns[[col]] <- list(
            method = "one_hot",
            new_columns = colnames(dummy_vars)
          )
          
          # Drop original column if requested
          if (drop_original) {
            processed_data[[col]] <- NULL
          }
          
        }, error = function(e) {
          warning("Error in one-hot encoding for '", col, "': ", e$message)
        })
      }
      
    } else if (encode_categorical == "label") {
      # Label encoding: convert to numeric
      for (col in categorical_col_names) {
        # Skip if already numeric
        if (is.numeric(processed_data[[col]])) {
          next
        }
        
        # Create new column with label encoding
        new_col_name <- paste0(col, "_label")
        
        # Convert to factor and then to numeric
        if (!is.factor(processed_data[[col]])) {
          processed_data[[col]] <- as.factor(processed_data[[col]])
        }
        
        # Get numeric codes
        processed_data[[new_col_name]] <- as.numeric(processed_data[[col]])
        
        # Record info
        preprocessing_info$encoding$columns[[col]] <- list(
          method = "label",
          new_columns = new_col_name,
          mapping = data.frame(
            original = levels(processed_data[[col]]),
            encoded = 1:length(levels(processed_data[[col]]))
          )
        )
        
        # Drop original column if requested
        if (drop_original) {
          processed_data[[col]] <- NULL
        }
      }
      
    } else if (encode_categorical == "binary") {
      # Binary encoding for categorical variables with exactly 2 levels
      for (col in categorical_col_names) {
        # Get unique values (excluding NA)
        unique_vals <- unique(na.omit(processed_data[[col]]))
        
        # Only apply to binary variables
        if (length(unique_vals) == 2) {
          # Create new column name
          new_col_name <- paste0(col, "_binary")
          
          # Create factor and convert to 0/1
          processed_data[[new_col_name]] <- ifelse(processed_data[[col]] == unique_vals[1], 0, 1)
          
          # Record info
          preprocessing_info$encoding$columns[[col]] <- list(
            method = "binary",
            new_columns = new_col_name,
            mapping = data.frame(
              original = unique_vals,
              encoded = c(0, 1)
            )
          )
          
          # Drop original column if requested
          if (drop_original) {
            processed_data[[col]] <- NULL
          }
        } else {
          if (verbose) {
            message("Skipping binary encoding for '", col, "' (not binary, has ", length(unique_vals), " unique values)")
          }
        }
      }
    }
    
    if (verbose) {
      encoded_cols <- names(preprocessing_info$encoding$columns)
      if (length(encoded_cols) > 0) {
        message("Encoded categorical columns: ", 
                paste(encoded_cols, collapse = ", "), 
                " using method: ", encode_categorical)
      }
    }
  }
  
  # Scale numeric variables
  if (scale || center) {
    numeric_col_names <- names(processed_data)[sapply(processed_data, is.numeric)]
    
    if (length(numeric_col_names) > 0) {
      # Create a matrix of numeric columns for scaling
      numeric_matrix <- as.matrix(processed_data[, numeric_col_names, drop = FALSE])
      
      # Scale/center the matrix
      scaled_matrix <- scale(numeric_matrix, center = center, scale = scale)
      
      # Replace columns in the processed data
      for (col in numeric_col_names) {
        processed_data[[col]] <- scaled_matrix[, col]
      }
      
      # Record scaling info
      if (scale) {
        preprocessing_info$scaling$applied <- TRUE
        preprocessing_info$scaling$columns <- numeric_col_names
      }
      
      # Record centering info
      if (center) {
        preprocessing_info$centering$applied <- TRUE
        preprocessing_info$centering$columns <- numeric_col_names
      }
      
      if (verbose) {
        transformations <- c()
        if (center) transformations <- c(transformations, "centered")
        if (scale) transformations <- c(transformations, "scaled")
        
        message(paste(transformations, collapse = " and "), " numeric columns: ", 
                paste(numeric_col_names, collapse = ", "))
      }
    }
  }
  
  # Return the processed data and preprocessing info
  result <- list(
    processed_data = processed_data,
    preprocessing_info = preprocessing_info,
    original_data = original_data
  )
  
  # Set class for print method
  class(result) <- c("preprocessing_result", "list")
  
  return(result)
}

#' Print method for preprocessing results
#'
#' @param x A preprocessing result object
#' @param ... Additional arguments passed to print
#'
#' @return Invisibly returns the preprocessing result object
#' @export
print.preprocessing_result <- function(x, ...) {
  cat("Data Preprocessing Summary\n")
  cat("------------------------\n\n")
  
  # Original data dimensions
  cat("Original data dimensions: ", nrow(x$original_data), " rows, ", ncol(x$original_data), " columns\n", sep = "")
  cat("Processed data dimensions: ", nrow(x$processed_data), " rows, ", ncol(x$processed_data), " columns\n\n", sep = "")
  
  # Dropped columns
  if (length(x$preprocessing_info$columns_dropped) > 0) {
    cat("Dropped columns: ", paste(x$preprocessing_info$columns_dropped, collapse = ", "), "\n\n", sep = "")
  }
  
  # Numeric imputation
  if (x$preprocessing_info$numeric_imputation$method != "none" && 
      length(x$preprocessing_info$numeric_imputation$columns) > 0) {
    cat("Numeric imputation (method: ", x$preprocessing_info$numeric_imputation$method, "):\n", sep = "")
    for (col in names(x$preprocessing_info$numeric_imputation$columns)) {
      info <- x$preprocessing_info$numeric_imputation$columns[[col]]
      cat("  - ", col, ": ", info$missing_count, " values (", round(info$missing_percent, 1), "%)", sep = "")
      if (!is.null(info$impute_value)) {
        cat(", imputed with ", info$impute_value, sep = "")
      }
      cat("\n")
    }
    cat("\n")
  }
  
  # Categorical imputation
  if (x$preprocessing_info$categorical_imputation$method != "none" && 
      length(x$preprocessing_info$categorical_imputation$columns) > 0) {
    cat("Categorical imputation (method: ", x$preprocessing_info$categorical_imputation$method, "):\n", sep = "")
    for (col in names(x$preprocessing_info$categorical_imputation$columns)) {
      info <- x$preprocessing_info$categorical_imputation$columns[[col]]
      cat("  - ", col, ": ", info$missing_count, " values (", round(info$missing_percent, 1), "%)", sep = "")
      if (!is.null(info$impute_value)) {
        cat(", imputed with '", info$impute_value, "'", sep = "")
      }
      cat("\n")
    }
    cat("\n")
  }
  
  # Encoding
  if (x$preprocessing_info$encoding$method != "none" && 
      length(x$preprocessing_info$encoding$columns) > 0) {
    cat("Categorical encoding (method: ", x$preprocessing_info$encoding$method, "):\n", sep = "")
    for (col in names(x$preprocessing_info$encoding$columns)) {
      info <- x$preprocessing_info$encoding$columns[[col]]
      cat("  - ", col, " → ", paste(info$new_columns, collapse = ", "), "\n", sep = "")
    }
    cat("\n")
  }
  
  # Scaling and centering
  if (x$preprocessing_info$scaling$applied) {
    cat("Scaled columns: ", paste(x$preprocessing_info$scaling$columns, collapse = ", "), "\n", sep = "")
  }
  
  if (x$preprocessing_info$centering$applied) {
    cat("Centered columns: ", paste(x$preprocessing_info$centering$columns, collapse = ", "), "\n", sep = "")
  }
  
  invisible(x)
}


#' Detect data types in a dataframe
#'
#' @param data A dataframe to analyze
#'
#' @return A list with categorized column names
#' @export
#'
#' @examples
#' detect_data_types(iris)
detect_data_types <- function(data) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  # Identify column types
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  integer_cols <- names(data)[sapply(data, is.integer)]
  factor_cols <- names(data)[sapply(data, is.factor)]
  character_cols <- names(data)[sapply(data, is.character)]
  logical_cols <- names(data)[sapply(data, is.logical)]
  date_cols <- names(data)[sapply(data, function(x) inherits(x, "Date"))]
  datetime_cols <- names(data)[sapply(data, function(x) inherits(x, "POSIXct") || inherits(x, "POSIXlt"))]
  
  # Group columns into higher-level categories
  result <- list(
    numeric = numeric_cols,
    integer = integer_cols,
    categorical = c(factor_cols, character_cols, logical_cols),
    factor = factor_cols,
    character = character_cols,
    logical = logical_cols,
    date = date_cols,
    datetime = datetime_cols,
    temporal = c(date_cols, datetime_cols)
  )
  
  return(result)
}


#' Create dummy variables (one-hot encoding)
#'
#' @param data A dataframe
#' @param columns Character vector of column names to encode (default: NULL for all categorical)
#' @param drop_original Logical, whether to drop original columns (default: TRUE)
#' @param max_levels Integer, maximum number of unique values to encode (default: 10)
#'
#' @return A dataframe with dummy variables
#' @export
#'
#' @examples
#' # Encode all categorical variables
#' create_dummies(iris)
#'
#' # Encode specific column
#' create_dummies(iris, columns = "Species")
create_dummies <- function(data, columns = NULL, drop_original = TRUE, max_levels = 10) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  # Identify categorical columns if not specified
  if (is.null(columns)) {
    categorical_cols <- sapply(data, function(x) is.factor(x) || is.character(x) || is.logical(x))
    columns <- names(data)[categorical_cols]
  } else {
    # Validate specified columns
    invalid_cols <- columns[!columns %in% names(data)]
    if (length(invalid_cols) > 0) {
      stop("Invalid column names: ", paste(invalid_cols, collapse = ", "))
    }
  }
  
  # If no categorical columns, return original data
  if (length(columns) == 0) {
    message("No categorical columns found for dummy encoding")
    return(data)
  }
  
  # Create a copy of the data
  result <- data
  
  # Process each column
  for (col in columns) {
    # Skip if not categorical
    if (!is.factor(data[[col]]) && !is.character(data[[col]]) && !is.logical(data[[col]])) {
      message("Skipping ", col, " as it's not categorical")
      next
    }
    
    # Check number of unique values
    n_unique <- length(unique(na.omit(data[[col]])))
    if (n_unique > max_levels) {
      message("Skipping ", col, " as it has ", n_unique, " unique values (max: ", max_levels, ")")
      next
    }
    
    # Convert to factor
    factor_col <- if (is.factor(data[[col]])) data[[col]] else factor(data[[col]])
    
    # Create dummy variables
    dummy_data <- stats::model.matrix(~ 0 + factor_col)
    colnames(dummy_data) <- gsub("factor_col", col, colnames(dummy_data))
    
    # Add dummy variables to result
    for (i in 1:ncol(dummy_data)) {
      # Clean column name
      clean_name <- gsub("[^a-zA-Z0-9_]", "_", colnames(dummy_data)[i])
      result[[clean_name]] <- dummy_data[, i]
    }
    
    # Drop original column if requested
    if (drop_original) {
      result[[col]] <- NULL
    }
  }
  
  return(result)
}


#' Impute missing values in a dataframe
#'
#' @param data A dataframe
#' @param numeric_method Character, method for numeric imputation: "mean", "median", "mode", "knn", or "none"
#' @param categorical_method Character, method for categorical imputation: "mode", "new_category", or "none"
#' @param knn_k Integer, number of neighbors for KNN imputation
#' @param threshold Numeric, maximum percentage of missing values to impute
#'
#' @return A dataframe with imputed values
#' @export
#'
#' @examples
#' # Create data with missing values
#' df <- data.frame(
#'   a = c(1, 2, NA, 4, 5),
#'   b = c("x", NA, "z", "x", NA)
#' )
#'
#' # Impute missing values
#' impute_missing(df)
impute_missing <- function(data, 
                          numeric_method = "median", 
                          categorical_method = "mode",
                          knn_k = 5,
                          threshold = 50) {
  
  # Use the main preprocessing function with specific parameters
  result <- preprocess_data(
    data = data,
    impute_numeric = numeric_method,
    impute_categorical = categorical_method,
    encode_categorical = "none",
    scale = FALSE,
    center = FALSE,
    knn_k = knn_k,
    impute_threshold = threshold
  )
  
  return(result$processed_data)
}


#' Scale and center numeric variables
#'
#' @param data A dataframe
#' @param columns Character vector of column names to scale (default: NULL for all numeric)
#' @param center Logical, whether to center the variables (default: TRUE)
#' @param scale Logical, whether to scale the variables (default: TRUE)
#'
#' @return A dataframe with scaled/centered variables
#' @export
#'
#' @examples
#' # Scale all numeric variables
#' scale_variables(iris)
#'
#' # Scale specific columns
#' scale_variables(iris, columns = c("Sepal.Length", "Sepal.Width"))
scale_variables <- function(data, columns = NULL, center = TRUE, scale = TRUE) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  # Identify numeric columns if not specified
  if (is.null(columns)) {
    numeric_cols <- sapply(data, is.numeric)
    columns <- names(data)[numeric_cols]
  } else {
    # Validate specified columns
    invalid_cols <- columns[!columns %in% names(data)]
    if (length(invalid_cols) > 0) {
      stop("Invalid column names: ", paste(invalid_cols, collapse = ", "))
    }
    
    # Check if specified columns are numeric
    non_numeric <- columns[!sapply(data[, columns, drop = FALSE], is.numeric)]
    if (length(non_numeric) > 0) {
      stop("Non-numeric columns: ", paste(non_numeric, collapse = ", "))
    }
  }
  
  # If no numeric columns, return original data
  if (length(columns) == 0) {
    message("No numeric columns found for scaling")
    return(data)
  }
  
  # Create a copy of the data
  result <- data
  
  # Create a matrix of numeric columns for scaling
  numeric_matrix <- as.matrix(data[, columns, drop = FALSE])
  
  # Scale/center the matrix
  scaled_matrix <- scale(numeric_matrix, center = center, scale = scale)
  
  # Replace columns in the result
  for (col in columns) {
    result[[col]] <- scaled_matrix[, col]
  }
  
  # Add attributes with scaling information
  attr(result, "scaled") <- list(
    columns = columns,
    center = attr(scaled_matrix, "scaled:center"),
    scale = attr(scaled_matrix, "scaled:scale")
  )
  
  return(result)
}