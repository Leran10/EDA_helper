#' Provide model recommendations based on a scientific question
#'
#' This function analyzes a dataset and a scientific question to provide
#' model recommendations, explanations, and implementation suggestions.
#'
#' @param data A dataframe to analyze
#' @param question A character string describing the scientific question or analysis goal
#' @param api_key Optional OpenAI API key for enhanced recommendations (default: NULL)
#' @param local_only Logical, whether to use only local logic without API calls (default: FALSE)
#' @param include_code Logical, whether to include example code in recommendations (default: TRUE)
#' @return A list containing model recommendations and explanations
#' @export
#'
#' @examples
#' \dontrun{
#' # Classification example
#' model_recommendations(iris, "Can I predict flower species based on measurements?")
#'
#' # Regression example
#' model_recommendations(mtcars, "What factors influence car fuel efficiency (mpg)?")
#'
#' # Clustering example
#' model_recommendations(USArrests, "Can I group states by crime rates?")
#' }
model_recommendations <- function(data, question, api_key = NULL, local_only = FALSE, include_code = TRUE) {
  if (!is.data.frame(data)) {
    stop("Input must be a dataframe")
  }
  
  if (!is.character(question) || length(question) != 1) {
    stop("Question must be a single character string")
  }
  
  # Create basic structure for the results
  result <- list(
    question = question,
    data_summary = list(),
    task_identification = list(),
    recommended_models = list(),
    evaluation_metrics = character(),
    implementation = list()
  )
  
  # Get basic data summary
  result$data_summary$dimensions <- dim(data)
  result$data_summary$column_types <- sapply(data, class)
  result$data_summary$numeric_count <- sum(sapply(data, is.numeric))
  result$data_summary$categorical_count <- sum(!sapply(data, is.numeric))
  result$data_summary$missing_values <- sum(is.na(data))
  result$data_summary$missing_percent <- sum(is.na(data)) / prod(dim(data)) * 100
  
  # Try to identify target variable if mentioned in the question
  column_names <- names(data)
  possible_targets <- character(0)
  
  # Look for column names in the question
  for (col in column_names) {
    if (grepl(paste0("\\b", col, "\\b"), question, ignore.case = TRUE)) {
      possible_targets <- c(possible_targets, col)
    }
  }
  
  # If we have OpenAI API key and not forced to local only, use the API
  if (!is.null(api_key) && !local_only && requireNamespace("httr", quietly = TRUE) && 
      requireNamespace("jsonlite", quietly = TRUE)) {
    
    # Prepare API request
    api_url <- "https://api.openai.com/v1/chat/completions"
    
    # Create context from data summary
    data_summary <- paste0(
      "Dataset dimensions: ", result$data_summary$dimensions[1], " rows, ", 
      result$data_summary$dimensions[2], " columns\n",
      "Column names: ", paste(column_names, collapse = ", "), "\n",
      "Column types: ", paste(names(result$data_summary$column_types), ":", 
                              result$data_summary$column_types, collapse = ", "), "\n",
      "Numeric columns: ", result$data_summary$numeric_count, "\n",
      "Categorical columns: ", result$data_summary$categorical_count, "\n",
      "Missing values: ", result$data_summary$missing_percent, "%\n"
    )
    
    # Create the prompt
    prompt <- paste0(
      "Based on the following dataset and scientific question, recommend the most appropriate models,",
      " explain why they are suitable, suggest evaluation metrics, and provide implementation advice.",
      "\n\nDATASET SUMMARY:\n", data_summary,
      "\n\nSCIENTIFIC QUESTION:\n", question,
      "\n\nPlease respond with the following sections:\n",
      "1. Task Type: Identify whether this is classification, regression, clustering, etc.\n",
      "2. Recommended Models: List 3-5 appropriate models in order of recommendation\n",
      "3. Explanations: For each model, explain why it's suitable\n",
      "4. Evaluation Metrics: Suggest appropriate metrics\n",
      "5. Implementation Considerations: Important factors, preprocessing needed, etc.\n",
      if (include_code) "6. Example Code: Brief R code snippets for the top 2 recommended models\n" else ""
    )
    
    # Make API request
    headers <- c(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", api_key)
    )
    
    body <- list(
      model = "gpt-4-turbo",
      messages = list(
        list(
          role = "system",
          content = "You are a data science expert who specializes in statistical modeling and machine learning."
        ),
        list(
          role = "user",
          content = prompt
        )
      ),
      temperature = 0.2,
      max_tokens = 1500
    )
    
    tryCatch({
      response <- httr::POST(
        url = api_url,
        httr::add_headers(.headers = headers),
        body = jsonlite::toJSON(body, auto_unbox = TRUE),
        encode = "json"
      )
      
      if (httr::status_code(response) == 200) {
        response_content <- httr::content(response, "text", encoding = "UTF-8")
        response_json <- jsonlite::fromJSON(response_content, simplifyVector = FALSE)
        ai_response <- response_json$choices[[1]]$message$content
        
        # Parse the AI response
        result$ai_response <- ai_response
        
        # Return the result
        class(result) <- c("model_recommendations", "list")
        return(result)
      } else {
        warning("API request failed with status code: ", httr::status_code(response))
      }
    }, error = function(e) {
      warning("API request failed: ", e$message)
    })
  }
  
  # Local recommendation logic (used if API call fails or is not requested)
  
  # Try to identify the task type based on the question
  task_type <- "unknown"
  
  # Classification keywords
  if (grepl("\\b(classif|categori|predict.*class|identif.*type|which (group|category)|what (group|category)|predict.*species)\\b", 
            question, ignore.case = TRUE)) {
    task_type <- "classification"
  } 
  # Regression keywords
  else if (grepl("\\b(regress|predict.*value|predict.*continuous|estimate.*amount|relation|impact|influence|affect|how much|quantify)\\b", 
                 question, ignore.case = TRUE)) {
    task_type <- "regression"
  } 
  # Clustering keywords
  else if (grepl("\\b(cluster|group|segment|similar|like|partition|categorize without|natural group|unsupervised)\\b", 
                 question, ignore.case = TRUE)) {
    task_type <- "clustering"
  } 
  # Time series keywords
  else if (grepl("\\b(time series|forecast|predict.*future|trend|seasonal|over time|temporal)\\b", 
                 question, ignore.case = TRUE)) {
    task_type <- "time_series"
  }
  # Dimensionality reduction keywords
  else if (grepl("\\b(dimension|reduce.*dimension|feature extraction|principal component|pca|feature reduction|compress)\\b", 
                 question, ignore.case = TRUE)) {
    task_type <- "dimensionality_reduction"
  }
  
  result$task_identification$type <- task_type
  
  # Identify potential target variables
  if (length(possible_targets) > 0) {
    result$task_identification$possible_targets <- possible_targets
  }
  
  # Model recommendations based on task type
  if (task_type == "classification") {
    # Check if we have binary or multi-class classification
    is_binary <- FALSE
    if (length(possible_targets) > 0) {
      target_col <- possible_targets[1]
      if (target_col %in% names(data) && !is.numeric(data[[target_col]])) {
        classes <- unique(data[[target_col]][!is.na(data[[target_col]])])
        is_binary <- length(classes) == 2
      }
    }
    
    # Recommended models
    if (is_binary) {
      result$recommended_models <- list(
        list(
          name = "Logistic Regression",
          description = "A linear model for binary classification that estimates the probability of the target class. It works well when the relationship between features and target is approximately linear.",
          pros = c("Interpretable", "Fast to train", "Probabilistic output", "Works well with small datasets"),
          cons = c("Assumes linear relationship", "May underperform with complex, non-linear patterns"),
          when_to_use = "When you need an interpretable model and have well-behaved data without complex interactions."
        ),
        list(
          name = "Random Forest",
          description = "An ensemble of decision trees that improves prediction accuracy and controls overfitting. It works well with a mix of feature types and captures non-linear relationships.",
          pros = c("Handles non-linearity well", "Robust to overfitting", "Can handle mixed feature types", "Provides feature importance"),
          cons = c("Less interpretable than logistic regression", "Requires more computational resources"),
          when_to_use = "When you need high accuracy and have complex relationships in your data."
        ),
        list(
          name = "Support Vector Machine",
          description = "Finds an optimal hyperplane to separate classes in feature space. Can handle non-linear boundaries using kernel functions.",
          pros = c("Effective in high-dimensional spaces", "Handles non-linear data with kernels", "Memory efficient"),
          cons = c("Can be slow to train on large datasets", "Sensitive to parameter tuning"),
          when_to_use = "When you have complex decision boundaries and higher-dimensional data."
        ),
        list(
          name = "Gradient Boosting",
          description = "Builds trees sequentially where each tree corrects errors of the previous ones. Known for high accuracy.",
          pros = c("Often achieves state-of-the-art accuracy", "Handles mixed data types", "Built-in handling of missing values"),
          cons = c("Risk of overfitting", "Computationally intensive", "Requires careful tuning"),
          when_to_use = "When prediction accuracy is the primary goal and interpretability is secondary."
        )
      )
    } else {
      # Multi-class classification
      result$recommended_models <- list(
        list(
          name = "Random Forest",
          description = "An ensemble of decision trees that works well for multi-class problems and captures complex relationships in the data.",
          pros = c("Naturally handles multi-class problems", "Robust to overfitting", "Can handle mixed feature types", "Provides feature importance"),
          cons = c("Less interpretable than simpler models", "Requires more computational resources"),
          when_to_use = "When you need high accuracy with minimal preprocessing for multi-class problems."
        ),
        list(
          name = "Multinomial Logistic Regression",
          description = "An extension of logistic regression for multi-class classification. Estimates probability for each class.",
          pros = c("Interpretable", "Fast to train", "Probabilistic output"),
          cons = c("Assumes linear relationship", "May underperform with complex patterns"),
          when_to_use = "When interpretability is important and relationships are approximately linear."
        ),
        list(
          name = "Support Vector Machine (with one-vs-rest strategy)",
          description = "Applies binary SVM multiple times for multi-class classification, finding optimal hyperplanes between each class and the rest.",
          pros = c("Effective in high-dimensional spaces", "Can handle complex decision boundaries with kernels"),
          cons = c("Can be slow to train", "Requires careful parameter tuning"),
          when_to_use = "When you have complex, possibly non-linear class boundaries."
        ),
        list(
          name = "Neural Network",
          description = "A flexible model capable of learning complex patterns for multi-class classification.",
          pros = c("Can capture highly complex patterns", "Works well with large datasets", "Adaptable to various data types"),
          cons = c("Requires substantial data", "Computationally intensive", "Prone to overfitting without proper regularization"),
          when_to_use = "When you have a large dataset with complex patterns and computational resources aren't a concern."
        )
      )
    }
    
    # Evaluation metrics
    result$evaluation_metrics <- c(
      "Accuracy", 
      "Precision", 
      "Recall", 
      "F1-score", 
      "AUC-ROC (for binary classification)", 
      "Confusion Matrix"
    )
    
  } else if (task_type == "regression") {
    result$recommended_models <- list(
      list(
        name = "Linear Regression",
        description = "Models the relationship between variables using a linear equation. Simple, interpretable, and works well when the relationship is approximately linear.",
        pros = c("Simple and interpretable", "Fast to train", "Provides coefficient significance"),
        cons = c("Assumes linear relationship", "Sensitive to outliers", "Assumes independence of features"),
        when_to_use = "When relationships between variables are approximately linear and interpretability is important."
      ),
      list(
        name = "Random Forest Regression",
        description = "An ensemble of decision trees for regression that captures non-linear relationships and interactions between variables.",
        pros = c("Handles non-linearity well", "Robust to outliers", "Provides feature importance", "Low risk of overfitting"),
        cons = c("Less interpretable than linear models", "Computationally more intensive"),
        when_to_use = "When you have complex, non-linear relationships and prediction accuracy is more important than interpretability."
      ),
      list(
        name = "Gradient Boosting Regression",
        description = "Builds trees sequentially to correct errors from previous ones, often providing state-of-the-art performance.",
        pros = c("High prediction accuracy", "Handles mixed data types", "Can capture complex patterns"),
        cons = c("Risk of overfitting", "Computationally intensive", "Less interpretable"),
        when_to_use = "When prediction accuracy is the primary goal and you're willing to tune hyperparameters carefully."
      ),
      list(
        name = "Support Vector Regression",
        description = "Adapts SVM for regression tasks by finding a function that deviates from actual values by no more than a specified margin.",
        pros = c("Works well in high-dimensional spaces", "Handles non-linear relationships with kernels", "Robust to outliers"),
        cons = c("Sensitive to parameter tuning", "Computationally intensive for large datasets"),
        when_to_use = "When you have a moderate-sized dataset with complex relationships and want robustness to outliers."
      )
    )
    
    # Evaluation metrics
    result$evaluation_metrics <- c(
      "Mean Squared Error (MSE)", 
      "Root Mean Squared Error (RMSE)", 
      "Mean Absolute Error (MAE)", 
      "R-squared (coefficient of determination)", 
      "Adjusted R-squared"
    )
    
  } else if (task_type == "clustering") {
    result$recommended_models <- list(
      list(
        name = "K-means Clustering",
        description = "Partitions data into k clusters by minimizing the distance between data points and cluster centers.",
        pros = c("Simple and fast", "Works well with large datasets", "Easily interpretable results"),
        cons = c("Requires specifying number of clusters", "Sensitive to initial centers", "Assumes spherical clusters"),
        when_to_use = "When you have roughly spherical clusters of similar sizes and numeric features only."
      ),
      list(
        name = "Hierarchical Clustering",
        description = "Builds a hierarchy of clusters either by merging (agglomerative) or splitting (divisive) clusters.",
        pros = c("No need to specify number of clusters in advance", "Produces a dendrogram visualization", "Can handle various distance metrics"),
        cons = c("Computationally intensive for large datasets", "Results can vary based on linkage method"),
        when_to_use = "When you want to explore different numbers of clusters and need a visual hierarchy of relationships."
      ),
      list(
        name = "DBSCAN",
        description = "Density-based clustering that groups together points that are closely packed and marks outliers in low-density regions.",
        pros = c("No need to specify number of clusters", "Can find arbitrarily shaped clusters", "Robust to outliers"),
        cons = c("Sensitive to parameters", "Struggles with varying density clusters"),
        when_to_use = "When you have irregularly shaped clusters and potential outliers in your data."
      ),
      list(
        name = "Gaussian Mixture Models",
        description = "Assumes data comes from a mixture of several Gaussian distributions and tries to identify their parameters.",
        pros = c("Soft assignment of points to clusters", "Can handle overlapping clusters", "Provides probability scores"),
        cons = c("Assumes Gaussian distributions", "Can converge to local optima"),
        when_to_use = "When you expect your data to come from a mixture of normal distributions and want probabilistic assignments."
      )
    )
    
    # Evaluation metrics
    result$evaluation_metrics <- c(
      "Silhouette Score", 
      "Davies-Bouldin Index", 
      "Calinski-Harabasz Index", 
      "Within-cluster Sum of Squares", 
      "Between-cluster Sum of Squares"
    )
    
  } else if (task_type == "time_series") {
    result$recommended_models <- list(
      list(
        name = "ARIMA (AutoRegressive Integrated Moving Average)",
        description = "Models time series data using past values (AR), differences (I), and error terms (MA).",
        pros = c("Well-established method", "Works well for stationary data", "Interpretable components"),
        cons = c("Assumes linearity", "Limited to univariate time series", "Requires stationarity"),
        when_to_use = "When you have a stationary time series (or can make it stationary) and want an interpretable model."
      ),
      list(
        name = "Prophet",
        description = "Developed by Facebook, decomposes time series into trend, seasonality, and holidays components.",
        pros = c("Handles seasonality well", "Robust to missing data and outliers", "Easy to interpret"),
        cons = c("May not capture complex patterns", "Limited customization for special cases"),
        when_to_use = "When you have strong seasonality, missing data, or outliers, and need a quick, reliable forecast."
      ),
      list(
        name = "LSTM (Long Short-Term Memory) Neural Networks",
        description = "A type of recurrent neural network capable of learning long-term dependencies in time series data.",
        pros = c("Captures complex patterns", "Can handle multivariate time series", "No stationarity requirement"),
        cons = c("Requires substantial data", "Computationally intensive", "Less interpretable"),
        when_to_use = "When you have a large dataset with complex patterns, especially with multiple variables affecting the outcome."
      ),
      list(
        name = "Exponential Smoothing",
        description = "Weighted averages of past observations with exponentially decreasing weights for older observations.",
        pros = c("Simple and intuitive", "Works well with trends and seasonality", "Computationally efficient"),
        cons = c("Less effective for complex patterns", "Limited with exogenous variables"),
        when_to_use = "When you need a reliable, simple model and your data has clear trends or seasonal patterns."
      )
    )
    
    # Evaluation metrics
    result$evaluation_metrics <- c(
      "Mean Absolute Error (MAE)", 
      "Mean Squared Error (MSE)", 
      "Root Mean Squared Error (RMSE)", 
      "Mean Absolute Percentage Error (MAPE)", 
      "AIC/BIC (for model comparison)"
    )
    
  } else if (task_type == "dimensionality_reduction") {
    result$recommended_models <- list(
      list(
        name = "Principal Component Analysis (PCA)",
        description = "Linear dimensionality reduction technique that projects data onto orthogonal axes of maximum variance.",
        pros = c("Simple and fast", "Preserves maximum variance", "Uncorrelated components"),
        cons = c("Limited to linear relationships", "Sensitive to scaling", "Hard to interpret components"),
        when_to_use = "When relationships between variables are mostly linear and you want to maximize variance retention."
      ),
      list(
        name = "t-SNE (t-Distributed Stochastic Neighbor Embedding)",
        description = "Non-linear technique that preserves local relationships, especially useful for visualization.",
        pros = c("Preserves local structure", "Great for visualization", "Works well with clusters"),
        cons = c("Computationally intensive", "Stochastic results", "Not suitable for feature engineering"),
        when_to_use = "When you need to visualize high-dimensional data and preserve cluster structures."
      ),
      list(
        name = "UMAP (Uniform Manifold Approximation and Projection)",
        description = "Modern non-linear dimensionality reduction that balances local and global structure preservation.",
        pros = c("Faster than t-SNE", "Preserves both local and global structure", "Scalable to large datasets"),
        cons = c("Complex algorithm", "Results can vary with parameters"),
        when_to_use = "When you need both local and global structure preservation and have a large dataset."
      ),
      list(
        name = "Autoencoder Neural Networks",
        description = "Neural networks that learn a compressed representation of the data through an encoder-decoder architecture.",
        pros = c("Can capture highly non-linear relationships", "Adaptable to various data types", "Can be specialized (e.g., for images)"),
        cons = c("Requires substantial data", "Computationally intensive", "Complex to tune"),
        when_to_use = "When you have large amounts of complex data, especially images or text, and need non-linear dimensionality reduction."
      )
    )
    
    # Evaluation metrics
    result$evaluation_metrics <- c(
      "Explained Variance Ratio", 
      "Reconstruction Error", 
      "Silhouette Score (if used for clustering)",
      "Trustworthiness and Continuity", 
      "Downstream Task Performance"
    )
    
  } else {
    # Unknown task type
    result$recommended_models <- list(
      list(
        name = "Exploratory Data Analysis",
        description = "Before applying specific models, conduct thorough exploratory analysis to understand the data and refine your question.",
        pros = c("Helps identify appropriate models", "Reveals data characteristics", "Guides preprocessing decisions"),
        cons = c("Not a predictive model itself"),
        when_to_use = "Always start with EDA before selecting specific models."
      ),
      list(
        name = "Random Forest",
        description = "A versatile ensemble method that works well for both classification and regression and provides feature importance.",
        pros = c("Versatile for many tasks", "Handles non-linearity", "Provides feature importance"),
        cons = c("Less interpretable than simpler models"),
        when_to_use = "When you're uncertain about the task type but need a robust starting point."
      )
    )
    
    # General evaluation metrics
    result$evaluation_metrics <- c(
      "Cross-validation", 
      "Train/test split performance", 
      "Task-specific metrics will depend on clarifying your objective"
    )
  }
  
  # Implementation advice based on data characteristics
  implementation_advice <- character(0)
  
  # Check for missing values
  if (result$data_summary$missing_percent > 0) {
    implementation_advice <- c(implementation_advice, 
                              paste0("Handle missing values (", round(result$data_summary$missing_percent, 1), 
                                    "% of data is missing). Consider imputation methods appropriate for your data."))
  }
  
  # Check for categorical variables
  if (result$data_summary$categorical_count > 0) {
    implementation_advice <- c(implementation_advice,
                              paste0("Encode categorical variables (", result$data_summary$categorical_count, 
                                    " categorical columns). Use one-hot encoding for nominal variables or ordinal encoding for ordered categories."))
  }
  
  # Feature scaling advice
  if (task_type %in% c("classification", "regression", "clustering", "dimensionality_reduction")) {
    implementation_advice <- c(implementation_advice,
                              "Consider feature scaling (standardization or normalization) especially for distance-based models.")
  }
  
  # Dataset size considerations
  if (result$data_summary$dimensions[1] < 100) {
    implementation_advice <- c(implementation_advice,
                              "Small dataset detected. Consider simpler models to avoid overfitting, or use cross-validation with regularization.")
  } else if (result$data_summary$dimensions[1] > 10000) {
    implementation_advice <- c(implementation_advice,
                              "Large dataset detected. Consider memory-efficient implementations or sampling techniques for model tuning.")
  }
  
  # Feature count considerations
  if (result$data_summary$dimensions[2] > 20) {
    implementation_advice <- c(implementation_advice,
                              "High feature count. Consider feature selection or dimensionality reduction techniques.")
  }
  
  result$implementation$advice <- implementation_advice
  
  # Example code for the top 2 recommended models
  if (include_code && length(result$recommended_models) >= 2) {
    code_examples <- list()
    
    # Determine target variable placeholder
    target_var <- "target_variable"
    if (length(possible_targets) > 0) {
      target_var <- possible_targets[1]
    }
    
    # Classification models code
    if (task_type == "classification") {
      if (result$recommended_models[[1]]$name == "Logistic Regression") {
        code_examples$logistic_regression <- paste0(
          "# Logistic Regression\n",
          "library(glmnet)  # For regularized logistic regression\n\n",
          "# Split data into training and testing sets\n",
          "set.seed(123)\n",
          "train_indices <- sample(1:nrow(data), 0.7 * nrow(data))\n",
          "train_data <- data[train_indices, ]\n",
          "test_data <- data[-train_indices, ]\n\n",
          "# Fit logistic regression model\n",
          "model <- glm(", target_var, " ~ ., data = train_data, family = 'binomial')\n\n",
          "# Make predictions\n",
          "predictions <- predict(model, newdata = test_data, type = 'response')\n",
          "predicted_classes <- ifelse(predictions > 0.5, 1, 0)\n\n",
          "# Evaluate model\n",
          "confusion_matrix <- table(predicted_classes, test_data$", target_var, ")\n",
          "accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)\n",
          "print(paste0(\"Accuracy: \", round(accuracy, 3)))"
        )
      }
      
      if (result$recommended_models[[1]]$name == "Random Forest" || result$recommended_models[[2]]$name == "Random Forest") {
        code_examples$random_forest <- paste0(
          "# Random Forest\n",
          "library(randomForest)\n\n",
          "# Split data into training and testing sets\n",
          "set.seed(123)\n",
          "train_indices <- sample(1:nrow(data), 0.7 * nrow(data))\n",
          "train_data <- data[train_indices, ]\n",
          "test_data <- data[-train_indices, ]\n\n",
          "# Fit random forest model\n",
          "model <- randomForest(", target_var, " ~ ., data = train_data, ntree = 500)\n\n",
          "# Make predictions\n",
          "predictions <- predict(model, newdata = test_data)\n\n",
          "# Evaluate model\n",
          "confusion_matrix <- table(predictions, test_data$", target_var, ")\n",
          "accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)\n",
          "print(paste0(\"Accuracy: \", round(accuracy, 3)))\n\n",
          "# View feature importance\n",
          "importance(model)\n",
          "varImpPlot(model)"
        )
      }
      
      if (result$recommended_models[[1]]$name == "Support Vector Machine" || result$recommended_models[[2]]$name == "Support Vector Machine") {
        code_examples$svm <- paste0(
          "# Support Vector Machine\n",
          "library(e1071)\n\n",
          "# Split data into training and testing sets\n",
          "set.seed(123)\n",
          "train_indices <- sample(1:nrow(data), 0.7 * nrow(data))\n",
          "train_data <- data[train_indices, ]\n",
          "test_data <- data[-train_indices, ]\n\n",
          "# Fit SVM model\n",
          "model <- svm(", target_var, " ~ ., data = train_data, kernel = 'radial', probability = TRUE)\n\n",
          "# Make predictions\n",
          "predictions <- predict(model, newdata = test_data)\n\n",
          "# Evaluate model\n",
          "confusion_matrix <- table(predictions, test_data$", target_var, ")\n",
          "accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)\n",
          "print(paste0(\"Accuracy: \", round(accuracy, 3)))"
        )
      }
    } 
    # Regression models code
    else if (task_type == "regression") {
      if (result$recommended_models[[1]]$name == "Linear Regression" || result$recommended_models[[2]]$name == "Linear Regression") {
        code_examples$linear_regression <- paste0(
          "# Linear Regression\n",
          "# Split data into training and testing sets\n",
          "set.seed(123)\n",
          "train_indices <- sample(1:nrow(data), 0.7 * nrow(data))\n",
          "train_data <- data[train_indices, ]\n",
          "test_data <- data[-train_indices, ]\n\n",
          "# Fit linear regression model\n",
          "model <- lm(", target_var, " ~ ., data = train_data)\n\n",
          "# View model summary\n",
          "summary(model)\n\n",
          "# Make predictions\n",
          "predictions <- predict(model, newdata = test_data)\n\n",
          "# Evaluate model\n",
          "rmse <- sqrt(mean((predictions - test_data$", target_var, ")^2))\n",
          "print(paste0(\"RMSE: \", round(rmse, 3)))\n",
          "r_squared <- cor(predictions, test_data$", target_var, ")^2\n",
          "print(paste0(\"R-squared: \", round(r_squared, 3)))"
        )
      }
      
      if (result$recommended_models[[1]]$name == "Random Forest Regression" || result$recommended_models[[2]]$name == "Random Forest Regression") {
        code_examples$rf_regression <- paste0(
          "# Random Forest Regression\n",
          "library(randomForest)\n\n",
          "# Split data into training and testing sets\n",
          "set.seed(123)\n",
          "train_indices <- sample(1:nrow(data), 0.7 * nrow(data))\n",
          "train_data <- data[train_indices, ]\n",
          "test_data <- data[-train_indices, ]\n\n",
          "# Fit random forest model\n",
          "model <- randomForest(", target_var, " ~ ., data = train_data, ntree = 500)\n\n",
          "# Make predictions\n",
          "predictions <- predict(model, newdata = test_data)\n\n",
          "# Evaluate model\n",
          "rmse <- sqrt(mean((predictions - test_data$", target_var, ")^2))\n",
          "print(paste0(\"RMSE: \", round(rmse, 3)))\n",
          "r_squared <- cor(predictions, test_data$", target_var, ")^2\n",
          "print(paste0(\"R-squared: \", round(r_squared, 3)))\n\n",
          "# View feature importance\n",
          "importance(model)\n",
          "varImpPlot(model)"
        )
      }
    }
    # Clustering models code
    else if (task_type == "clustering") {
      if (result$recommended_models[[1]]$name == "K-means Clustering" || result$recommended_models[[2]]$name == "K-means Clustering") {
        code_examples$kmeans <- paste0(
          "# K-means Clustering\n",
          "# Select only numeric columns for clustering\n",
          "numeric_data <- data[, sapply(data, is.numeric)]\n\n",
          "# Scale the data\n",
          "scaled_data <- scale(numeric_data)\n\n",
          "# Determine optimal number of clusters using the elbow method\n",
          "wss <- sapply(1:10, function(k) {\n",
          "  kmeans(scaled_data, centers = k, nstart = 25)$tot.withinss\n",
          "})\n",
          "plot(1:10, wss, type = 'b', xlab = 'Number of clusters', ylab = 'Within-cluster sum of squares')\n\n",
          "# Fit k-means with optimal k (example uses k=3)\n",
          "k <- 3  # Replace with your optimal k\n",
          "kmeans_result <- kmeans(scaled_data, centers = k, nstart = 25)\n\n",
          "# Add cluster assignments to original data\n",
          "data$cluster <- as.factor(kmeans_result$cluster)\n\n",
          "# Visualize clusters (for 2D projection)\n",
          "library(ggplot2)\n",
          "# If more than 2 dimensions, consider using PCA first\n",
          "pca_result <- prcomp(scaled_data)\n",
          "pca_data <- as.data.frame(pca_result$x[, 1:2])\n",
          "pca_data$cluster <- as.factor(kmeans_result$cluster)\n",
          "ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +\n",
          "  geom_point() +\n",
          "  theme_minimal() +\n",
          "  labs(title = 'K-means Clustering Results')"
        )
      }
      
      if (result$recommended_models[[1]]$name == "Hierarchical Clustering" || result$recommended_models[[2]]$name == "Hierarchical Clustering") {
        code_examples$hierarchical <- paste0(
          "# Hierarchical Clustering\n",
          "# Select only numeric columns for clustering\n",
          "numeric_data <- data[, sapply(data, is.numeric)]\n\n",
          "# Scale the data\n",
          "scaled_data <- scale(numeric_data)\n\n",
          "# Calculate distance matrix\n",
          "dist_matrix <- dist(scaled_data)\n\n",
          "# Perform hierarchical clustering\n",
          "hc_result <- hclust(dist_matrix, method = 'ward.D2')\n\n",
          "# Plot dendrogram\n",
          "plot(hc_result, main = 'Hierarchical Clustering Dendrogram', xlab = '', sub = '')\n\n",
          "# Cut the dendrogram to get clusters\n",
          "k <- 3  # Number of clusters\n",
          "clusters <- cutree(hc_result, k = k)\n\n",
          "# Add cluster assignments to original data\n",
          "data$cluster <- as.factor(clusters)\n\n",
          "# Visualize clusters (for 2D projection)\n",
          "library(ggplot2)\n",
          "# If more than 2 dimensions, consider using PCA first\n",
          "pca_result <- prcomp(scaled_data)\n",
          "pca_data <- as.data.frame(pca_result$x[, 1:2])\n",
          "pca_data$cluster <- as.factor(clusters)\n",
          "ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +\n",
          "  geom_point() +\n",
          "  theme_minimal() +\n",
          "  labs(title = 'Hierarchical Clustering Results')"
        )
      }
    }
    
    result$implementation$code_examples <- code_examples
  }
  
  class(result) <- c("model_recommendations", "list")
  return(result)
}

#' Print method for model_recommendations objects
#'
#' @param x A model_recommendations object
#' @param ... Additional arguments
#' @export
print.model_recommendations <- function(x, ...) {
  cat("Model Recommendations for Scientific Question\n")
  cat("===========================================\n\n")
  
  cat("Scientific Question:\n")
  cat(paste0('"', x$question, '"'), "\n\n")
  
  cat("Dataset Information:\n")
  cat("------------------\n")
  cat("Dimensions:", x$data_summary$dimensions[1], "rows,", x$data_summary$dimensions[2], "columns\n")
  cat("Numeric columns:", x$data_summary$numeric_count, "\n")
  cat("Categorical columns:", x$data_summary$categorical_count, "\n")
  if (x$data_summary$missing_values > 0) {
    cat("Missing values:", x$data_summary$missing_values, 
        "(", round(x$data_summary$missing_percent, 1), "%)\n")
  } else {
    cat("No missing values\n")
  }
  cat("\n")
  
  # If we have AI response, print it and return
  if (!is.null(x$ai_response)) {
    cat("Detailed Model Recommendations:\n")
    cat("----------------------------\n")
    cat(x$ai_response)
    return(invisible(NULL))
  }
  
  # Otherwise, print the local recommendations
  cat("Task Type:\n")
  cat("---------\n")
  task_name <- switch(x$task_identification$type,
                     "classification" = "Classification",
                     "regression" = "Regression",
                     "clustering" = "Clustering",
                     "time_series" = "Time Series Forecasting",
                     "dimensionality_reduction" = "Dimensionality Reduction",
                     "Unknown (further clarification needed)")
  cat(task_name, "\n\n")
  
  if (!is.null(x$task_identification$possible_targets)) {
    cat("Possible Target Variable(s):", paste(x$task_identification$possible_targets, collapse = ", "), "\n\n")
  }
  
  cat("Recommended Models:\n")
  cat("------------------\n")
  for (i in seq_along(x$recommended_models)) {
    model <- x$recommended_models[[i]]
    cat(i, ". ", model$name, "\n", sep = "")
    cat("   ", model$description, "\n", sep = "")
    cat("   Pros: ", paste(model$pros, collapse = ", "), "\n", sep = "")
    cat("   Cons: ", paste(model$cons, collapse = ", "), "\n", sep = "")
    cat("   When to use: ", model$when_to_use, "\n\n", sep = "")
  }
  
  cat("Evaluation Metrics:\n")
  cat("-----------------\n")
  for (metric in x$evaluation_metrics) {
    cat("- ", metric, "\n", sep = "")
  }
  cat("\n")
  
  cat("Implementation Considerations:\n")
  cat("---------------------------\n")
  for (advice in x$implementation$advice) {
    cat("- ", advice, "\n", sep = "")
  }
  cat("\n")
  
  if (!is.null(x$implementation$code_examples) && length(x$implementation$code_examples) > 0) {
    cat("Example Code for Top Models:\n")
    cat("-------------------------\n")
    for (model_name in names(x$implementation$code_examples)) {
      cat("\n", x$implementation$code_examples[[model_name]], "\n", sep = "")
    }
  }
}