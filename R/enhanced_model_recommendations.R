#' Provide enhanced model recommendations based on user preferences and study plan
#'
#' This function extends the basic model_recommendations function by adding
#' the ability to specify user priorities, preferences, and research goals.
#'
#' @param data A dataframe to analyze
#' @param study_plan A character string describing the research objectives or study plan
#' @param prioritize Character vector of priorities. Options: "accuracy", "interpretability", "speed", "explainability"
#' @param exclude_models Character vector of models to exclude from recommendations
#' @param model_family Optional character specifying a model family to focus on. 
#'        Options: "linear", "tree_based", "distance_based", "bayesian", "ensemble"
#' @param max_complexity Numeric from 1-5 indicating the maximum model complexity allowed (default: 5)
#' @param required_features Character vector of model features that are required
#' @param target_variable Optional character specifying the name of the target variable
#' @param api_key Optional OpenAI API key for enhanced recommendations (default: NULL)
#' @param local_only Logical, whether to use only local logic without API calls (default: FALSE)
#' @param include_code Logical, whether to include example code in recommendations (default: TRUE)
#' @return A list containing enhanced model recommendations and explanations
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage with study plan
#' enhanced_model_recommendations(iris, "Predict flower species with focus on interpretability")
#'
#' # Setting priorities
#' enhanced_model_recommendations(mtcars, "Analyze car efficiency",
#'                               prioritize = c("interpretability", "accuracy"),
#'                               target_variable = "mpg")
#'
#' # Excluding certain models and focusing on a model family
#' enhanced_model_recommendations(iris, "Classify iris species",
#'                               exclude_models = c("neural_networks", "svm"),
#'                               model_family = "tree_based")
#' }
enhanced_model_recommendations <- function(data, 
                                          study_plan, 
                                          prioritize = NULL,
                                          exclude_models = NULL,
                                          model_family = NULL,
                                          max_complexity = 5,
                                          required_features = NULL,
                                          target_variable = NULL,
                                          api_key = NULL, 
                                          local_only = FALSE, 
                                          include_code = TRUE) {
  
  if (!is.data.frame(data)) {
    stop("Input must be a dataframe")
  }
  
  if (!is.character(study_plan) || length(study_plan) != 1) {
    stop("Study plan must be a single character string")
  }
  
  # Validate priority options
  valid_priorities <- c("accuracy", "interpretability", "speed", "explainability")
  if (!is.null(prioritize)) {
    if (!all(prioritize %in% valid_priorities)) {
      invalid_priorities <- prioritize[!prioritize %in% valid_priorities]
      stop(paste("Invalid priorities:", paste(invalid_priorities, collapse = ", "), 
                "Valid options are:", paste(valid_priorities, collapse = ", ")))
    }
  }
  
  # Validate model family
  valid_families <- c("linear", "tree_based", "distance_based", "bayesian", "ensemble")
  if (!is.null(model_family) && !(model_family %in% valid_families)) {
    stop(paste("Invalid model family:", model_family, 
              "Valid options are:", paste(valid_families, collapse = ", ")))
  }
  
  # Validate max complexity
  if (!is.numeric(max_complexity) || max_complexity < 1 || max_complexity > 5) {
    stop("max_complexity must be a number between 1 and 5")
  }
  
  # Create structure for the results
  result <- list(
    study_plan = study_plan,
    data_summary = list(),
    task_identification = list(),
    prioritized_models = list(),
    excluded_models = exclude_models,
    model_family = model_family,
    evaluation_metrics = character(),
    implementation = list(),
    user_preferences = list(
      priorities = prioritize,
      max_complexity = max_complexity,
      required_features = required_features
    )
  )
  
  # Get basic data summary
  result$data_summary$dimensions <- dim(data)
  result$data_summary$column_types <- sapply(data, class)
  result$data_summary$numeric_count <- sum(sapply(data, is.numeric))
  result$data_summary$categorical_count <- sum(!sapply(data, is.numeric))
  result$data_summary$missing_values <- sum(is.na(data))
  result$data_summary$missing_percent <- sum(is.na(data)) / prod(dim(data)) * 100
  
  # Try to identify target variable if provided or mentioned in the study plan
  if (!is.null(target_variable) && target_variable %in% names(data)) {
    result$task_identification$target <- target_variable
  } else {
    # Try to infer from study plan
    column_names <- names(data)
    possible_targets <- character(0)
    
    # Look for column names in the study plan
    for (col in column_names) {
      if (grepl(paste0("\\b", col, "\\b"), study_plan, ignore.case = TRUE)) {
        possible_targets <- c(possible_targets, col)
      }
    }
    
    if (length(possible_targets) > 0) {
      result$task_identification$possible_targets <- possible_targets
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
      "Column names: ", paste(names(data), collapse = ", "), "\n",
      "Column types: ", paste(names(result$data_summary$column_types), ":", 
                              result$data_summary$column_types, collapse = ", "), "\n",
      "Numeric columns: ", result$data_summary$numeric_count, "\n",
      "Categorical columns: ", result$data_summary$categorical_count, "\n",
      "Missing values: ", result$data_summary$missing_percent, "%\n"
    )
    
    # Add user preferences to the context
    user_preferences <- paste0(
      "USER PREFERENCES:\n",
      if (!is.null(prioritize)) paste0("Priorities: ", paste(prioritize, collapse = ", "), "\n") else "",
      if (!is.null(exclude_models)) paste0("Excluded models: ", paste(exclude_models, collapse = ", "), "\n") else "",
      if (!is.null(model_family)) paste0("Model family focus: ", model_family, "\n") else "",
      "Maximum complexity (1-5): ", max_complexity, "\n",
      if (!is.null(required_features)) paste0("Required features: ", paste(required_features, collapse = ", "), "\n") else "",
      if (!is.null(target_variable)) paste0("Target variable: ", target_variable, "\n") else ""
    )
    
    # Create the prompt
    prompt <- paste0(
      "Based on the following dataset, study plan, and user preferences, recommend the most appropriate models,",
      " explain why they are suitable, suggest evaluation metrics, and provide implementation advice.",
      "\n\nDATASET SUMMARY:\n", data_summary,
      "\n\nSTUDY PLAN:\n", study_plan,
      "\n\n", user_preferences,
      "\n\nPlease respond with the following sections:\n",
      "1. Task Type: Identify whether this is classification, regression, clustering, etc.\n",
      "2. Recommended Models: List 3-5 appropriate models in order of preference, considering user priorities\n",
      "3. Explanations: For each model, explain why it's suitable and how it aligns with user preferences\n",
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
          content = "You are a data science expert who specializes in statistical modeling, machine learning, and research design."
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
        class(result) <- c("enhanced_model_recommendations", "list")
        return(result)
      } else {
        warning("API request failed with status code: ", httr::status_code(response))
      }
    }, error = function(e) {
      warning("API request failed: ", e$message)
    })
  }
  
  # Local recommendation logic (used if API call fails or is not requested)
  
  # Try to identify the task type based on the study plan
  task_type <- "unknown"
  
  # Classification keywords
  if (grepl("\\b(classif|categori|predict.*class|identif.*type|which (group|category)|what (group|category)|predict.*species)\\b", 
            study_plan, ignore.case = TRUE)) {
    task_type <- "classification"
  } 
  # Regression keywords
  else if (grepl("\\b(regress|predict.*value|predict.*continuous|estimate.*amount|relation|impact|influence|affect|how much|quantify)\\b", 
                 study_plan, ignore.case = TRUE)) {
    task_type <- "regression"
  } 
  # Clustering keywords
  else if (grepl("\\b(cluster|group|segment|similar|like|partition|categorize without|natural group|unsupervised)\\b", 
                 study_plan, ignore.case = TRUE)) {
    task_type <- "clustering"
  } 
  # Time series keywords
  else if (grepl("\\b(time series|forecast|predict.*future|trend|seasonal|over time|temporal)\\b", 
                 study_plan, ignore.case = TRUE)) {
    task_type <- "time_series"
  }
  # Dimensionality reduction keywords
  else if (grepl("\\b(dimension|reduce.*dimension|feature extraction|principal component|pca|feature reduction|compress)\\b", 
                 study_plan, ignore.case = TRUE)) {
    task_type <- "dimensionality_reduction"
  }
  
  result$task_identification$type <- task_type
  
  # Define model characteristics for scoring
  model_characteristics <- list(
    # Classification models
    logistic_regression = list(
      name = "Logistic Regression",
      family = "linear",
      accuracy = 3,
      interpretability = 5,
      speed = 5,
      explainability = 5,
      complexity = 1,
      handles_missing = 1,
      handles_categorical = 2,
      handles_imbalanced = 2,
      handles_nonlinear = 1,
      features = c("coefficients", "p_values", "confidence_intervals")
    ),
    random_forest_classifier = list(
      name = "Random Forest",
      family = "tree_based",
      accuracy = 4,
      interpretability = 3,
      speed = 3,
      explainability = 3,
      complexity = 3,
      handles_missing = 3,
      handles_categorical = 4,
      handles_imbalanced = 3,
      handles_nonlinear = 5,
      features = c("feature_importance", "bootstrapping", "ensemble")
    ),
    svm_classifier = list(
      name = "Support Vector Machine",
      family = "distance_based",
      accuracy = 4,
      interpretability = 2,
      speed = 2,
      explainability = 2,
      complexity = 4,
      handles_missing = 1,
      handles_categorical = 2,
      handles_imbalanced = 2,
      handles_nonlinear = 4,
      features = c("kernel_methods", "margin_maximization")
    ),
    gradient_boosting_classifier = list(
      name = "Gradient Boosting",
      family = "ensemble",
      accuracy = 5,
      interpretability = 2,
      speed = 2,
      explainability = 3,
      complexity = 4,
      handles_missing = 4,
      handles_categorical = 4,
      handles_imbalanced = 4,
      handles_nonlinear = 5,
      features = c("feature_importance", "ensemble", "boosting")
    ),
    neural_network_classifier = list(
      name = "Neural Network",
      family = "neural",
      accuracy = 5,
      interpretability = 1,
      speed = 1,
      explainability = 1,
      complexity = 5,
      handles_missing = 2,
      handles_categorical = 3,
      handles_imbalanced = 3,
      handles_nonlinear = 5,
      features = c("deep_learning", "transfer_learning", "representation_learning")
    ),
    naive_bayes = list(
      name = "Naive Bayes",
      family = "bayesian",
      accuracy = 3,
      interpretability = 4,
      speed = 5,
      explainability = 4,
      complexity = 2,
      handles_missing = 3,
      handles_categorical = 5,
      handles_imbalanced = 3,
      handles_nonlinear = 2,
      features = c("probabilistic", "independence_assumption")
    ),
    
    # Regression models
    linear_regression = list(
      name = "Linear Regression",
      family = "linear",
      accuracy = 3,
      interpretability = 5,
      speed = 5,
      explainability = 5,
      complexity = 1,
      handles_missing = 1,
      handles_categorical = 2,
      handles_imbalanced = 3,
      handles_nonlinear = 1,
      features = c("coefficients", "p_values", "confidence_intervals")
    ),
    random_forest_regressor = list(
      name = "Random Forest Regression",
      family = "tree_based",
      accuracy = 4,
      interpretability = 3,
      speed = 3,
      explainability = 3,
      complexity = 3,
      handles_missing = 3,
      handles_categorical = 4,
      handles_imbalanced = 4,
      handles_nonlinear = 5,
      features = c("feature_importance", "bootstrapping", "ensemble")
    ),
    gradient_boosting_regressor = list(
      name = "Gradient Boosting Regression",
      family = "ensemble",
      accuracy = 5,
      interpretability = 2,
      speed = 2,
      explainability = 3,
      complexity = 4,
      handles_missing = 4,
      handles_categorical = 4,
      handles_imbalanced = 4,
      handles_nonlinear = 5,
      features = c("feature_importance", "ensemble", "boosting")
    ),
    svm_regressor = list(
      name = "Support Vector Regression",
      family = "distance_based",
      accuracy = 4,
      interpretability = 2,
      speed = 2,
      explainability = 2,
      complexity = 4,
      handles_missing = 1,
      handles_categorical = 2,
      handles_imbalanced = 3,
      handles_nonlinear = 4,
      features = c("kernel_methods", "margin_maximization")
    ),
    neural_network_regressor = list(
      name = "Neural Network Regression",
      family = "neural",
      accuracy = 5,
      interpretability = 1,
      speed = 1,
      explainability = 1,
      complexity = 5,
      handles_missing = 2,
      handles_categorical = 3,
      handles_imbalanced = 3,
      handles_nonlinear = 5,
      features = c("deep_learning", "representation_learning")
    ),
    
    # Clustering models
    kmeans = list(
      name = "K-means Clustering",
      family = "distance_based",
      accuracy = 3,
      interpretability = 4,
      speed = 5,
      explainability = 4,
      complexity = 2,
      handles_missing = 1,
      handles_categorical = 1,
      handles_imbalanced = 2,
      handles_nonlinear = 2,
      features = c("centroid_based", "euclidean_distance")
    ),
    hierarchical_clustering = list(
      name = "Hierarchical Clustering",
      family = "distance_based",
      accuracy = 3,
      interpretability = 4,
      speed = 3,
      explainability = 4,
      complexity = 3,
      handles_missing = 2,
      handles_categorical = 2,
      handles_imbalanced = 3,
      handles_nonlinear = 3,
      features = c("dendrogram", "distance_based", "agglomerative")
    ),
    dbscan = list(
      name = "DBSCAN",
      family = "distance_based",
      accuracy = 4,
      interpretability = 3,
      speed = 4,
      explainability = 3,
      complexity = 3,
      handles_missing = 1,
      handles_categorical = 1,
      handles_imbalanced = 5,
      handles_nonlinear = 5,
      features = c("density_based", "noise_detection", "no_clusters_specification")
    ),
    gaussian_mixture = list(
      name = "Gaussian Mixture Models",
      family = "bayesian",
      accuracy = 4,
      interpretability = 3,
      speed = 3,
      explainability = 3,
      complexity = 4,
      handles_missing = 3,
      handles_categorical = 2,
      handles_imbalanced = 3,
      handles_nonlinear = 4,
      features = c("probabilistic", "soft_clustering", "distribution_based")
    ),
    
    # Time series models
    arima = list(
      name = "ARIMA",
      family = "time_series",
      accuracy = 3,
      interpretability = 4,
      speed = 4,
      explainability = 4,
      complexity = 3,
      handles_missing = 2,
      handles_categorical = 1,
      handles_imbalanced = 3,
      handles_nonlinear = 2,
      features = c("autoregressive", "moving_average", "integrated")
    ),
    prophet = list(
      name = "Prophet",
      family = "time_series",
      accuracy = 4,
      interpretability = 4,
      speed = 4,
      explainability = 4,
      complexity = 3,
      handles_missing = 4,
      handles_categorical = 2,
      handles_imbalanced = 3,
      handles_nonlinear = 4,
      features = c("trend", "seasonality", "holidays", "changepoints")
    ),
    lstm = list(
      name = "LSTM Neural Networks",
      family = "neural",
      accuracy = 5,
      interpretability = 1,
      speed = 1,
      explainability = 1,
      complexity = 5,
      handles_missing = 2,
      handles_categorical = 3,
      handles_imbalanced = 3,
      handles_nonlinear = 5,
      features = c("sequence_modeling", "memory_cells", "deep_learning")
    ),
    exponential_smoothing = list(
      name = "Exponential Smoothing",
      family = "time_series",
      accuracy = 3,
      interpretability = 5,
      speed = 5,
      explainability = 5,
      complexity = 2,
      handles_missing = 3,
      handles_categorical = 1,
      handles_imbalanced = 3,
      handles_nonlinear = 2,
      features = c("weighted_averages", "trend", "seasonality")
    ),
    
    # Dimensionality reduction models
    pca = list(
      name = "Principal Component Analysis",
      family = "linear",
      accuracy = 3,
      interpretability = 3,
      speed = 5,
      explainability = 3,
      complexity = 2,
      handles_missing = 1,
      handles_categorical = 1,
      handles_imbalanced = 3,
      handles_nonlinear = 1,
      features = c("variance_maximization", "orthogonal_components")
    ),
    tsne = list(
      name = "t-SNE",
      family = "distance_based",
      accuracy = 4,
      interpretability = 2,
      speed = 2,
      explainability = 2,
      complexity = 4,
      handles_missing = 1,
      handles_categorical = 2,
      handles_imbalanced = 4,
      handles_nonlinear = 5,
      features = c("nonlinear_reduction", "local_structure_preservation", "visualization")
    ),
    umap = list(
      name = "UMAP",
      family = "distance_based",
      accuracy = 4,
      interpretability = 2,
      speed = 3,
      explainability = 2,
      complexity = 4,
      handles_missing = 1,
      handles_categorical = 3,
      handles_imbalanced = 4,
      handles_nonlinear = 5,
      features = c("nonlinear_reduction", "manifold_learning", "topological")
    ),
    autoencoder = list(
      name = "Autoencoder Neural Networks",
      family = "neural",
      accuracy = 5,
      interpretability = 1,
      speed = 2,
      explainability = 1,
      complexity = 5,
      handles_missing = 2,
      handles_categorical = 3,
      handles_imbalanced = 3,
      handles_nonlinear = 5,
      features = c("encoder_decoder", "representation_learning", "deep_learning")
    )
  )
  
  # Function to filter and score models based on task type and user preferences
  filter_and_score_models <- function(task_type, model_characteristics, 
                                     prioritize = NULL, exclude_models = NULL, 
                                     model_family = NULL, max_complexity = 5,
                                     required_features = NULL) {
    
    # Start with all models
    all_models <- model_characteristics
    
    # Filter by task type
    models <- switch(task_type,
      "classification" = list(
        all_models$logistic_regression,
        all_models$random_forest_classifier,
        all_models$svm_classifier,
        all_models$gradient_boosting_classifier,
        all_models$neural_network_classifier,
        all_models$naive_bayes
      ),
      "regression" = list(
        all_models$linear_regression,
        all_models$random_forest_regressor,
        all_models$gradient_boosting_regressor,
        all_models$svm_regressor,
        all_models$neural_network_regressor
      ),
      "clustering" = list(
        all_models$kmeans,
        all_models$hierarchical_clustering,
        all_models$dbscan,
        all_models$gaussian_mixture
      ),
      "time_series" = list(
        all_models$arima,
        all_models$prophet,
        all_models$lstm,
        all_models$exponential_smoothing
      ),
      "dimensionality_reduction" = list(
        all_models$pca,
        all_models$tsne,
        all_models$umap,
        all_models$autoencoder
      ),
      # Default to a general set of models if task type is unknown
      list(
        all_models$random_forest_classifier,
        all_models$logistic_regression,
        all_models$kmeans,
        all_models$linear_regression,
        all_models$gradient_boosting_classifier
      )
    )
    
    # Apply model family filter if specified
    if (!is.null(model_family)) {
      models <- models[sapply(models, function(model) model$family == model_family)]
      
      # If no models left after filtering, give a warning and reset to all models for that task
      if (length(models) == 0) {
        warning(paste("No models in family", model_family, "for task type", task_type, "- using all models"))
        models <- switch(task_type,
          "classification" = list(
            all_models$logistic_regression,
            all_models$random_forest_classifier,
            all_models$svm_classifier,
            all_models$gradient_boosting_classifier,
            all_models$neural_network_classifier,
            all_models$naive_bayes
          ),
          "regression" = list(
            all_models$linear_regression,
            all_models$random_forest_regressor,
            all_models$gradient_boosting_regressor,
            all_models$svm_regressor,
            all_models$neural_network_regressor
          ),
          "clustering" = list(
            all_models$kmeans,
            all_models$hierarchical_clustering,
            all_models$dbscan,
            all_models$gaussian_mixture
          ),
          "time_series" = list(
            all_models$arima,
            all_models$prophet,
            all_models$lstm,
            all_models$exponential_smoothing
          ),
          "dimensionality_reduction" = list(
            all_models$pca,
            all_models$tsne,
            all_models$umap,
            all_models$autoencoder
          ),
          list(
            all_models$random_forest_classifier,
            all_models$logistic_regression,
            all_models$kmeans,
            all_models$linear_regression,
            all_models$gradient_boosting_classifier
          )
        )
      }
    }
    
    # Filter by complexity
    models <- models[sapply(models, function(model) model$complexity <= max_complexity)]
    
    # Filter by required features
    if (!is.null(required_features) && length(required_features) > 0) {
      models <- models[sapply(models, function(model) {
        all(required_features %in% model$features)
      })]
    }
    
    # Filter excluded models
    if (!is.null(exclude_models) && length(exclude_models) > 0) {
      # Create pattern to match excluded model names
      exclude_pattern <- paste0("\\b(", paste(exclude_models, collapse = "|"), ")\\b")
      
      # Filter models that don't match the exclusion pattern
      models <- models[!sapply(models, function(model) {
        grepl(exclude_pattern, tolower(model$name), ignore.case = TRUE)
      })]
    }
    
    # Score models based on user priorities
    if (!is.null(prioritize) && length(prioritize) > 0) {
      # Assign weights to each priority (first priority has highest weight)
      n_priorities <- length(prioritize)
      weights <- rev(seq_len(n_priorities))
      names(weights) <- prioritize
      
      # Score each model
      model_scores <- sapply(models, function(model) {
        # Base score starts at 0
        score <- 0
        
        # Add weighted scores for each priority
        for (priority in prioritize) {
          if (priority %in% names(weights)) {
            score <- score + model[[priority]] * weights[priority]
          }
        }
        
        return(score)
      })
      
      # Sort models by score
      sorted_indices <- order(model_scores, decreasing = TRUE)
      models <- models[sorted_indices]
      
      # Add scores to the models
      for (i in seq_along(models)) {
        models[[i]]$score <- model_scores[sorted_indices[i]]
      }
    }
    
    return(models)
  }
  
  # Get filtered and scored models
  filtered_models <- filter_and_score_models(
    task_type, 
    model_characteristics, 
    prioritize, 
    exclude_models, 
    model_family, 
    max_complexity,
    required_features
  )
  
  # Store models in result
  result$prioritized_models <- filtered_models
  
  # Set evaluation metrics based on task type
  result$evaluation_metrics <- switch(task_type,
    "classification" = c(
      "Accuracy", 
      "Precision", 
      "Recall", 
      "F1-score", 
      "AUC-ROC (for binary classification)", 
      "Confusion Matrix"
    ),
    "regression" = c(
      "Mean Squared Error (MSE)", 
      "Root Mean Squared Error (RMSE)", 
      "Mean Absolute Error (MAE)", 
      "R-squared (coefficient of determination)", 
      "Adjusted R-squared"
    ),
    "clustering" = c(
      "Silhouette Score", 
      "Davies-Bouldin Index", 
      "Calinski-Harabasz Index", 
      "Within-cluster Sum of Squares", 
      "Between-cluster Sum of Squares"
    ),
    "time_series" = c(
      "Mean Absolute Error (MAE)", 
      "Mean Squared Error (MSE)", 
      "Root Mean Squared Error (RMSE)", 
      "Mean Absolute Percentage Error (MAPE)", 
      "AIC/BIC (for model comparison)"
    ),
    "dimensionality_reduction" = c(
      "Explained Variance Ratio", 
      "Reconstruction Error", 
      "Silhouette Score (if used for clustering)",
      "Trustworthiness and Continuity", 
      "Downstream Task Performance"
    ),
    # Default metrics
    c(
      "Cross-validation", 
      "Train/test split performance", 
      "Task-specific metrics will depend on clarifying your objective"
    )
  )
  
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
  if (include_code && length(filtered_models) >= 2) {
    code_examples <- list()
    
    # Determine target variable placeholder
    target_var <- "target_variable"
    if (!is.null(target_variable)) {
      target_var <- target_variable
    } else if (!is.null(result$task_identification$possible_targets) && 
               length(result$task_identification$possible_targets) > 0) {
      target_var <- result$task_identification$possible_targets[1]
    }
    
    # Add the top 2 models' code examples
    for (i in 1:min(2, length(filtered_models))) {
      model <- filtered_models[[i]]
      model_name <- gsub(" ", "_", tolower(model$name))
      
      # Classification models
      if (task_type == "classification") {
        if (grepl("logistic", model_name)) {
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
        } else if (grepl("random forest", model_name)) {
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
        } else if (grepl("svm", model_name)) {
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
        } else if (grepl("gradient boosting", model_name)) {
          code_examples$gradient_boosting <- paste0(
            "# Gradient Boosting\n",
            "library(xgboost)\n\n",
            "# Split data into training and testing sets\n",
            "set.seed(123)\n",
            "train_indices <- sample(1:nrow(data), 0.7 * nrow(data))\n",
            "train_data <- data[train_indices, ]\n",
            "test_data <- data[-train_indices, ]\n\n",
            "# Convert data to matrix format (xgboost requirement)\n",
            "# Note: This assumes categorical variables are already encoded\n",
            "features <- setdiff(names(train_data), '", target_var, "')\n",
            "x_train <- as.matrix(train_data[, features])\n",
            "y_train <- as.numeric(as.factor(train_data$", target_var, ")) - 1  # 0-based indexing\n",
            "x_test <- as.matrix(test_data[, features])\n\n",
            "# Create DMatrix objects\n",
            "dtrain <- xgb.DMatrix(data = x_train, label = y_train)\n\n",
            "# Set parameters\n",
            "params <- list(\n",
            "  objective = 'binary:logistic',\n",
            "  eval_metric = 'logloss',\n",
            "  eta = 0.1,\n",
            "  max_depth = 6,\n",
            "  subsample = 0.8,\n",
            "  colsample_bytree = 0.8\n",
            ")\n\n",
            "# Train the model\n",
            "model <- xgb.train(\n",
            "  params = params,\n",
            "  data = dtrain,\n",
            "  nrounds = 100,\n",
            "  verbose = 0\n",
            ")\n\n",
            "# Make predictions\n",
            "predictions_prob <- predict(model, x_test)\n",
            "predictions <- as.numeric(predictions_prob > 0.5)\n\n",
            "# Evaluate model\n",
            "actual <- as.numeric(as.factor(test_data$", target_var, ")) - 1\n",
            "confusion_matrix <- table(predictions, actual)\n",
            "accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)\n",
            "print(paste0(\"Accuracy: \", round(accuracy, 3)))\n\n",
            "# Feature importance\n",
            "importance <- xgb.importance(model = model, feature_names = features)\n",
            "xgb.plot.importance(importance)"
          )
        } else if (grepl("naive bayes", model_name)) {
          code_examples$naive_bayes <- paste0(
            "# Naive Bayes\n",
            "library(e1071)\n\n",
            "# Split data into training and testing sets\n",
            "set.seed(123)\n",
            "train_indices <- sample(1:nrow(data), 0.7 * nrow(data))\n",
            "train_data <- data[train_indices, ]\n",
            "test_data <- data[-train_indices, ]\n\n",
            "# Fit Naive Bayes model\n",
            "model <- naiveBayes(", target_var, " ~ ., data = train_data)\n\n",
            "# Make predictions\n",
            "predictions <- predict(model, newdata = test_data)\n\n",
            "# Evaluate model\n",
            "confusion_matrix <- table(predictions, test_data$", target_var, ")\n",
            "accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)\n",
            "print(paste0(\"Accuracy: \", round(accuracy, 3)))"
          )
        }
      }
      # Regression models
      else if (task_type == "regression") {
        if (grepl("linear regression", model_name)) {
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
        } else if (grepl("random forest", model_name)) {
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
        } else if (grepl("gradient boosting", model_name)) {
          code_examples$gradient_boosting_regression <- paste0(
            "# Gradient Boosting Regression\n",
            "library(xgboost)\n\n",
            "# Split data into training and testing sets\n",
            "set.seed(123)\n",
            "train_indices <- sample(1:nrow(data), 0.7 * nrow(data))\n",
            "train_data <- data[train_indices, ]\n",
            "test_data <- data[-train_indices, ]\n\n",
            "# Convert data to matrix format (xgboost requirement)\n",
            "features <- setdiff(names(train_data), '", target_var, "')\n",
            "x_train <- as.matrix(train_data[, features])\n",
            "y_train <- train_data$", target_var, "\n",
            "x_test <- as.matrix(test_data[, features])\n\n",
            "# Create DMatrix objects\n",
            "dtrain <- xgb.DMatrix(data = x_train, label = y_train)\n\n",
            "# Set parameters\n",
            "params <- list(\n",
            "  objective = 'reg:squarederror',\n",
            "  eval_metric = 'rmse',\n",
            "  eta = 0.1,\n",
            "  max_depth = 6,\n",
            "  subsample = 0.8,\n",
            "  colsample_bytree = 0.8\n",
            ")\n\n",
            "# Train the model\n",
            "model <- xgb.train(\n",
            "  params = params,\n",
            "  data = dtrain,\n",
            "  nrounds = 100,\n",
            "  verbose = 0\n",
            ")\n\n",
            "# Make predictions\n",
            "predictions <- predict(model, x_test)\n\n",
            "# Evaluate model\n",
            "rmse <- sqrt(mean((predictions - test_data$", target_var, ")^2))\n",
            "print(paste0(\"RMSE: \", round(rmse, 3)))\n",
            "r_squared <- cor(predictions, test_data$", target_var, ")^2\n",
            "print(paste0(\"R-squared: \", round(r_squared, 3)))\n\n",
            "# Feature importance\n",
            "importance <- xgb.importance(model = model, feature_names = features)\n",
            "xgb.plot.importance(importance)"
          )
        }
      }
      # Clustering models
      else if (task_type == "clustering") {
        if (grepl("k-means", model_name)) {
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
        } else if (grepl("hierarchical", model_name)) {
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
        } else if (grepl("dbscan", model_name)) {
          code_examples$dbscan <- paste0(
            "# DBSCAN Clustering\n",
            "library(dbscan)\n\n",
            "# Select only numeric columns for clustering\n",
            "numeric_data <- data[, sapply(data, is.numeric)]\n\n",
            "# Scale the data\n",
            "scaled_data <- scale(numeric_data)\n\n",
            "# Find optimal eps parameter using k-distance graph\n",
            "kNNdistplot(scaled_data, k = 4)\n",
            "# Look for the 'knee' in the plot to determine eps value\n",
            "# For this example, we'll use eps = 0.5\n",
            "eps <- 0.5  # Replace with your optimal eps value\n\n",
            "# Perform DBSCAN clustering\n",
            "dbscan_result <- dbscan(scaled_data, eps = eps, minPts = 4)\n\n",
            "# Add cluster assignments to original data\n",
            "# Note: Cluster 0 represents noise points\n",
            "data$cluster <- as.factor(dbscan_result$cluster)\n\n",
            "# Visualize clusters (for 2D projection)\n",
            "library(ggplot2)\n",
            "# If more than 2 dimensions, consider using PCA first\n",
            "pca_result <- prcomp(scaled_data)\n",
            "pca_data <- as.data.frame(pca_result$x[, 1:2])\n",
            "pca_data$cluster <- as.factor(dbscan_result$cluster)\n",
            "ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +\n",
            "  geom_point() +\n",
            "  theme_minimal() +\n",
            "  labs(title = 'DBSCAN Clustering Results')"
          )
        }
      }
    }
    
    result$implementation$code_examples <- code_examples
  }
  
  class(result) <- c("enhanced_model_recommendations", "list")
  return(result)
}

#' Print method for enhanced_model_recommendations objects
#'
#' @param x An enhanced_model_recommendations object
#' @param ... Additional arguments
#' @export
print.enhanced_model_recommendations <- function(x, ...) {
  cat("Enhanced Model Recommendations for Study Plan\n")
  cat("============================================\n\n")
  
  cat("Study Plan:\n")
  cat(paste0('"', x$study_plan, '"'), "\n\n")
  
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
  
  # User preferences section
  cat("User Preferences:\n")
  cat("----------------\n")
  if (!is.null(x$user_preferences$priorities) && length(x$user_preferences$priorities) > 0) {
    cat("Priorities:", paste(x$user_preferences$priorities, collapse = ", "), "\n")
  } else {
    cat("No specific priorities provided\n")
  }
  
  if (!is.null(x$excluded_models) && length(x$excluded_models) > 0) {
    cat("Excluded models:", paste(x$excluded_models, collapse = ", "), "\n")
  }
  
  if (!is.null(x$model_family)) {
    cat("Model family focus:", x$model_family, "\n")
  }
  
  cat("Maximum complexity level:", x$user_preferences$max_complexity, "out of 5\n")
  
  if (!is.null(x$user_preferences$required_features) && length(x$user_preferences$required_features) > 0) {
    cat("Required features:", paste(x$user_preferences$required_features, collapse = ", "), "\n")
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
  
  if (!is.null(x$task_identification$target)) {
    cat("Target Variable:", x$task_identification$target, "\n\n")
  } else if (!is.null(x$task_identification$possible_targets)) {
    cat("Possible Target Variable(s):", paste(x$task_identification$possible_targets, collapse = ", "), "\n\n")
  }
  
  cat("Recommended Models:\n")
  cat("------------------\n")
  for (i in seq_along(x$prioritized_models)) {
    model <- x$prioritized_models[[i]]
    
    # Show model score if available
    score_text <- if (!is.null(model$score)) paste0(" (Score: ", round(model$score, 1), ")") else ""
    
    cat(i, ". ", model$name, score_text, "\n", sep = "")
    cat("   Family: ", model$family, "\n", sep = "")
    cat("   Characteristics:\n", sep = "")
    cat("     - Accuracy: ", paste(rep("", model$accuracy), collapse = ""), "\n", sep = "")
    cat("     - Interpretability: ", paste(rep("", model$interpretability), collapse = ""), "\n", sep = "")
    cat("     - Speed: ", paste(rep("", model$speed), collapse = ""), "\n", sep = "")
    cat("     - Explainability: ", paste(rep("", model$explainability), collapse = ""), "\n", sep = "")
    cat("     - Complexity: ", paste(rep("", model$complexity), collapse = ""), "\n", sep = "")
    cat("   Features: ", paste(model$features, collapse = ", "), "\n\n", sep = "")
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