library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(scales)

# Define UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "EDAhelper Interactive",
    tags$li(
      class = "dropdown",
      actionButton(
        "download_report_btn",
        label = "Download Report",
        icon = icon("file-export"),
        style = "margin-top: 8px; margin-right: 10px; background-color: #5cb85c; color: white; border-color: #4cae4c;"
      )
    ),
    tags$li(
      class = "dropdown",
      actionButton(
        "help_btn",
        label = "Help",
        icon = icon("question-circle"),
        style = "margin-top: 8px; margin-right: 10px; background-color: #3c8dbc; color: white; border-color: #367fa9;"
      )
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem("Data Upload", tabName = "data_upload", icon = icon("upload")),
      menuItem("Data Summary", tabName = "data_summary", icon = icon("table")),
      menuItem("Missing Data", tabName = "missing_data", icon = icon("question-circle")),
      menuItem("Correlations", tabName = "correlations", icon = icon("chart-line")),
      menuItem("Model Recommendations", tabName = "models", icon = icon("robot"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),  # Initialize shinyjs
    # Hidden UI elements
    uiOutput("download_ui"),
    
    tabItems(
      # Data Upload Tab
      tabItem(tabName = "data_upload",
        fluidRow(
          box(
            title = "Upload Data",
            width = 12,
            fileInput("file", "Choose CSV File",
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv"
                      )
            ),
            checkboxInput("header", "Header", TRUE),
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                         selected = ","),
            radioButtons("quote", "Quote",
                         choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                         selected = '"'),
            hr(),
            div(
              actionButton("load_demo", "Load Demo Data", 
                          icon = icon("database"),
                          style = "color: #fff; background-color: #5cb85c; border-color: #4cae4c"),
              helpText("Click to load a sample dataset with missing values, outliers, and correlations")
            )
          )
        ),
        fluidRow(
          box(
            title = "Data Preview",
            width = 12,
            DTOutput("data_preview")
          )
        ),
        fluidRow(
          infoBoxOutput("rows_info", width = 4),
          infoBoxOutput("cols_info", width = 4),
          infoBoxOutput("memory_info", width = 4)
        )
      ),
      
      # Data Summary Tab
      tabItem(tabName = "data_summary",
        fluidRow(
          box(
            title = "Variable Selection",
            width = 3,
            selectInput("summary_var", "Select Variable", choices = NULL),
            radioButtons("summary_type", "Plot Type",
                         choices = c("Histogram" = "hist", 
                                     "Boxplot" = "box", 
                                     "Density" = "density",
                                     "Bar Chart" = "bar"),
                         selected = "hist")
          ),
          box(
            title = "Plot",
            width = 9,
            plotlyOutput("summary_plot", height = "400px")
          )
        ),
        fluidRow(
          box(
            title = "Statistics",
            width = 12,
            DTOutput("summary_stats")
          )
        )
      ),
      
      # Missing Data Tab
      tabItem(tabName = "missing_data",
        fluidRow(
          box(
            title = "Missing Data Overview",
            width = 12,
            plotOutput("missing_overview", height = "400px")
          )
        ),
        fluidRow(
          box(
            title = "Missing Data by Variable",
            width = 6,
            plotOutput("missing_by_var", height = "400px")
          ),
          box(
            title = "Missing Data Patterns",
            width = 6,
            plotOutput("missing_patterns", height = "400px")
          )
        ),
        fluidRow(
          box(
            title = "Missing Data Statistics",
            width = 12,
            DTOutput("missing_stats")
          )
        )
      ),
      
      # Correlations Tab
      tabItem(tabName = "correlations",
        fluidRow(
          box(
            title = "Correlation Options",
            width = 3,
            radioButtons("corr_method", "Correlation Method",
                         choices = c("Pearson" = "pearson", 
                                     "Spearman" = "spearman", 
                                     "Kendall" = "kendall"),
                         selected = "pearson"),
            sliderInput("corr_threshold", "Correlation Threshold", 
                         min = 0, max = 1, value = 0.7, step = 0.05),
            checkboxInput("show_strong_only", "Show Strong Correlations Only", FALSE)
          ),
          box(
            title = "Correlation Plot",
            width = 9,
            plotlyOutput("corr_plot", height = "500px")
          )
        ),
        fluidRow(
          box(
            title = "Strong Correlations",
            width = 12,
            DTOutput("strong_corr_table")
          )
        )
      ),
      
      # Model Recommendations Tab
      tabItem(tabName = "models",
        fluidRow(
          tabBox(
            title = "Model Recommendation Settings",
            width = 12,
            
            # Basic Tab - Simple interface
            tabPanel("Basic",
              fluidRow(
                column(width = 12,
                  textAreaInput("model_question", "Analysis Question/Study Plan", 
                               value = "What model should I use to predict...?",
                               height = "100px"),
                  helpText("Describe your research goal or analysis question in detail"),
                  checkboxInput("model_local_only", "Use Local Logic Only", TRUE),
                  checkboxInput("model_include_code", "Include Code Examples", TRUE),
                  actionButton("get_basic_recommendations", "Get Recommendations", 
                             icon = icon("lightbulb"), 
                             style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                )
              )
            ),
            
            # Advanced Tab - Enhanced features
            tabPanel("Enhanced",
              h4("Study Plan and Preferences"),
              fluidRow(
                column(width = 6,
                  textAreaInput("study_plan", "Research Objective/Study Plan", 
                               value = "I want to predict...",
                               height = "80px"),
                  helpText("Describe what you're trying to achieve with your analysis"),
                  
                  selectInput("target_variable", "Target Variable (Optional)", 
                             choices = NULL, 
                             multiple = FALSE),
                  helpText("Select the variable you're trying to predict or analyze")
                ),
                column(width = 6,
                  checkboxGroupInput("prioritize", "Priorities (Order Matters)",
                                   choices = c(
                                     "Accuracy" = "accuracy",
                                     "Interpretability" = "interpretability", 
                                     "Speed" = "speed",
                                     "Explainability" = "explainability"
                                   ),
                                   selected = c("accuracy", "interpretability")),
                  helpText("Select and order your priorities (first is highest)")
                )
              ),
              
              fluidRow(
                column(width = 6,
                  sliderInput("max_complexity", "Maximum Model Complexity", 
                             min = 1, max = 5, value = 5, step = 1),
                  helpText("1 = Very simple, 5 = Highly complex")
                ),
                column(width = 6,
                  selectInput("model_family", "Focus on Model Family (Optional)",
                             choices = c(
                               "No Preference" = "",
                               "Linear Models" = "linear",
                               "Tree-based Models" = "tree_based",
                               "Distance-based Models" = "distance_based", 
                               "Bayesian Models" = "bayesian",
                               "Ensemble Models" = "ensemble"
                             ),
                             selected = "")
                )
              ),
              
              fluidRow(
                column(width = 6,
                  checkboxGroupInput("exclude_models", "Exclude These Models",
                                   choices = c(
                                     "Neural Networks" = "neural_networks",
                                     "Support Vector Machines" = "svm",
                                     "Gradient Boosting" = "gradient_boosting",
                                     "Linear/Logistic Regression" = "regression"
                                   ),
                                   selected = NULL)
                ),
                column(width = 6,
                  checkboxGroupInput("required_features", "Required Model Features",
                                   choices = c(
                                     "Feature Importance" = "feature_importance",
                                     "Confidence Intervals" = "confidence_intervals",
                                     "Probabilistic Output" = "probabilistic",
                                     "Kernel Methods" = "kernel_methods"
                                   ),
                                   selected = NULL),
                  helpText("Select features that your model must support")
                )
              ),
              
              fluidRow(
                column(width = 12,
                  hr(),
                  div(
                    style = "text-align: center;",
                    checkboxInput("enhanced_local_only", "Use Local Logic Only", TRUE),
                    checkboxInput("enhanced_include_code", "Include Code Examples", TRUE),
                    actionButton("get_enhanced_recommendations", "Get Enhanced Recommendations", 
                               icon = icon("robot"), 
                               style = "color: #fff; background-color: #5cb85c; border-color: #4cae4c; padding: 10px 20px;")
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Model Recommendations",
            width = 12,
            verbatimTextOutput("model_recommendations")
          )
        )
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Reactive value to store the uploaded data
  rv <- reactiveValues(
    data = NULL,
    data_summary = NULL,
    missing_analysis = NULL,
    correlation_analysis = NULL,
    model_recommendations = NULL
  )
  
  # Download Report Modal and Logic
  observeEvent(input$download_report_btn, {
    if (is.null(rv$data)) {
      showNotification("Please load data first before generating a report", type = "error")
      return()
    }
    
    # Show modal for report configuration
    showModal(modalDialog(
      title = "Generate Downloadable Report",
      size = "m",
      
      # Report options
      textInput("report_title", "Report Title", value = "EDA Report"),
      textInput("report_author", "Author Name (optional)", value = ""),
      
      checkboxGroupInput("report_sections", "Include Sections",
                       choices = list(
                         "Data Summary" = "data_summary",
                         "Missing Data Analysis" = "missing_data",
                         "Correlation Analysis" = "correlations",
                         "Model Recommendations" = "models"
                       ),
                       selected = c("data_summary", "missing_data", "correlations")),
      
      checkboxInput("report_include_code", "Include R Code", FALSE),
      
      selectInput("report_theme", "Report Theme", 
                 choices = list(
                   "Default (Cosmo)" = "cosmo",
                   "Cerulean" = "cerulean",
                   "Journal" = "journal",
                   "Flatly" = "flatly",
                   "Readable" = "readable",
                   "Spacelab" = "spacelab",
                   "United" = "united",
                   "Lumen" = "lumen",
                   "Paper" = "paper",
                   "Sandstone" = "sandstone",
                   "Simply" = "simply",
                   "Yeti" = "yeti"
                 ),
                 selected = "cosmo"),
      
      checkboxInput("report_toc", "Include Table of Contents", TRUE),
      
      # Report generation footer
      footer = tagList(
        modalButton("Cancel"),
        actionButton("generate_report", "Generate Report", 
                     icon = icon("file-export"),
                     style = "background-color: #5cb85c; color: white; border-color: #4cae4c")
      )
    ))
  })
  
  # Help button modal
  observeEvent(input$help_btn, {
    showModal(modalDialog(
      title = "EDAhelper App Help",
      size = "l",
      easyClose = TRUE,
      
      tabsetPanel(
        tabPanel("Getting Started",
          h4("Welcome to the EDAhelper Shiny App!"),
          p("This interactive app allows you to explore and analyze your data without writing code."),
          tags$ol(
            tags$li("Start by uploading a CSV file on the Data Upload tab, or use the Demo Data button."),
            tags$li("Navigate through the tabs on the left to access different EDA features."),
            tags$li("Visualize your data, analyze missing values, explore correlations, and more.")
          ),
          h4("Data Requirements"),
          p("For best results, ensure your data:"),
          tags$ul(
            tags$li("Is in CSV format with consistent delimiters"),
            tags$li("Has a header row with variable names"),
            tags$li("Contains a mix of numeric and categorical variables for full functionality")
          )
        ),
        
        tabPanel("Model Recommendations",
          h4("Using Model Recommendations"),
          p("The Model Recommendations feature offers two modes:"),
          tags$ol(
            tags$li(strong("Basic:"), "Simply describe your analysis goal in plain language"),
            tags$li(strong("Enhanced:"), "Specify detailed preferences about model characteristics")
          ),
          h4("Enhanced Recommendations"),
          tags$ul(
            tags$li(strong("Study Plan:"), "Describe your specific research objectives"),
            tags$li(strong("Priorities:"), "Choose what matters most (accuracy, interpretability, etc.)"),
            tags$li(strong("Target Variable:"), "Select the variable you want to predict"),
            tags$li(strong("Model Family:"), "Focus on specific types of models (tree-based, linear, etc.)"),
            tags$li(strong("Complexity:"), "Control how complex your models can be"),
            tags$li(strong("Exclusions:"), "Specify models you don't want to use")
          ),
          h4("Example Questions"),
          tags$ul(
            tags$li("Predict customer churn with interpretable models"),
            tags$li("Find clusters of similar patients"),
            tags$li("Forecast sales for the next 3 months")
          )
        )
      ),
      
      footer = modalButton("Close")
    ))
  })
  
  # Process data after loading
  process_data <- function(data) {
    # Update variable selection inputs
    updateSelectInput(session, "summary_var", choices = names(data))
    updateSelectInput(session, "target_variable", choices = c("None" = "", names(data)), selected = "")
    
    # Compute EDA analyses
    rv$data_summary <- data_summary(data, include_plots = FALSE)
    rv$missing_analysis <- missing_analysis(data, plot = TRUE)
    rv$correlation_analysis <- correlation_analysis(data, method = "pearson", plot = TRUE)
  }
  
  # Upload file
  observeEvent(input$file, {
    req(input$file)
    
    # Read the CSV file
    tryCatch({
      rv$data <- read.csv(
        input$file$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote
      )
      
      # Process the data
      process_data(rv$data)
      
    }, error = function(e) {
      showNotification(
        paste("Error reading file:", e$message),
        type = "error",
        duration = NULL
      )
    })
  })
  
  # Load demo data
  observeEvent(input$load_demo, {
    # Create a demo dataset
    set.seed(123)
    n <- 200
    
    # Create some correlated variables
    x1 <- rnorm(n)
    x2 <- x1 * 0.7 + rnorm(n, 0, 0.5)
    x3 <- rnorm(n)
    
    # Create a target variable with a non-linear relationship
    y <- 2 + 3 * x1 - 1.5 * x2 + 0.5 * x3^2 + rnorm(n, 0, 2)
    
    # Create categorical variables
    cat1 <- sample(c("A", "B", "C"), n, replace = TRUE, prob = c(0.6, 0.3, 0.1))
    cat2 <- sample(c("Low", "Medium", "High"), n, replace = TRUE)
    
    # Add some missing values
    x1[sample(1:n, 15)] <- NA
    x2[sample(1:n, 10)] <- NA
    cat1[sample(1:n, 12)] <- NA
    
    # Add some outliers
    x3[sample(1:n, 5)] <- x3[sample(1:n, 5)] + rnorm(5, 10, 2)
    y[sample(1:n, 3)] <- y[sample(1:n, 3)] * 3
    
    # Create a demo dataset
    demo_data <- data.frame(
      response = y,
      predictor_x1 = x1,
      predictor_x2 = x2,
      predictor_x3 = x3,
      category1 = cat1,
      category2 = cat2,
      registration_date = Sys.Date() - sample(1:500, n, replace = TRUE)
    )
    
    rv$data <- demo_data
    
    # Process the data
    process_data(rv$data)
    
    showNotification(
      "Demo data loaded successfully",
      type = "message",
      duration = 3
    )
  })
  
  # Data preview table
  output$data_preview <- renderDT({
    req(rv$data)
    datatable(
      rv$data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
  })
  
  # Data info boxes
  output$rows_info <- renderInfoBox({
    req(rv$data)
    infoBox(
      "Rows",
      nrow(rv$data),
      icon = icon("list"),
      color = "blue"
    )
  })
  
  output$cols_info <- renderInfoBox({
    req(rv$data)
    infoBox(
      "Columns",
      ncol(rv$data),
      icon = icon("table-columns"),
      color = "blue"
    )
  })
  
  output$memory_info <- renderInfoBox({
    req(rv$data)
    size_mb <- round(object.size(rv$data) / 1024^2, 2)
    infoBox(
      "Memory Usage",
      paste(size_mb, "MB"),
      icon = icon("memory"),
      color = "blue"
    )
  })
  
  # DATA SUMMARY TAB
  
  # Summary statistics
  output$summary_stats <- renderDT({
    req(rv$data, input$summary_var)
    
    var <- input$summary_var
    var_data <- rv$data[[var]]
    
    if (is.numeric(var_data)) {
      stats <- data.frame(
        Statistic = c("Min", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max", 
                     "Standard Deviation", "Missing Values"),
        Value = c(
          min(var_data, na.rm = TRUE),
          quantile(var_data, 0.25, na.rm = TRUE),
          median(var_data, na.rm = TRUE),
          mean(var_data, na.rm = TRUE),
          quantile(var_data, 0.75, na.rm = TRUE),
          max(var_data, na.rm = TRUE),
          sd(var_data, na.rm = TRUE),
          sum(is.na(var_data))
        )
      )
    } else {
      # For categorical variables
      freq_table <- table(var_data, useNA = "ifany")
      prop_table <- prop.table(freq_table) * 100
      
      stats <- data.frame(
        Category = names(freq_table),
        Frequency = as.numeric(freq_table),
        Percentage = as.numeric(prop_table)
      )
      
      # Handle NA category name
      if (any(is.na(stats$Category))) {
        stats$Category[is.na(stats$Category)] <- "Missing"
      }
    }
    
    datatable(stats, options = list(pageLength = 25))
  })
  
  # Summary plot
  output$summary_plot <- renderPlotly({
    req(rv$data, input$summary_var)
    
    var <- input$summary_var
    var_data <- rv$data[[var]]
    plot_type <- input$summary_type
    
    p <- NULL
    
    if (is.numeric(var_data)) {
      if (plot_type == "hist") {
        p <- ggplot(rv$data, aes_string(x = var)) +
          geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#69b3a2", alpha = 0.7) +
          geom_density(color = "#404080", linewidth = 1) +
          theme_minimal() +
          labs(title = paste("Histogram of", var), x = var, y = "Density")
      } else if (plot_type == "box") {
        p <- ggplot(rv$data, aes_string(y = var)) +
          geom_boxplot(fill = "#69b3a2", alpha = 0.7) +
          theme_minimal() +
          labs(title = paste("Boxplot of", var), y = var)
      } else if (plot_type == "density") {
        p <- ggplot(rv$data, aes_string(x = var)) +
          geom_density(fill = "#69b3a2", alpha = 0.7) +
          theme_minimal() +
          labs(title = paste("Density Plot of", var), x = var, y = "Density")
      }
    } else {
      # For categorical variables
      if (plot_type == "bar") {
        # Calculate counts
        counts <- table(var_data, useNA = "ifany")
        plot_data <- data.frame(
          Category = names(counts),
          Count = as.numeric(counts)
        )
        
        # Handle NA category name
        if (any(is.na(plot_data$Category))) {
          plot_data$Category[is.na(plot_data$Category)] <- "Missing"
        }
        
        p <- ggplot(plot_data, aes(x = reorder(Category, -Count), y = Count)) +
          geom_bar(stat = "identity", fill = "#69b3a2", alpha = 0.7) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(title = paste("Bar Chart of", var), x = var, y = "Count")
      }
    }
    
    if (!is.null(p)) {
      ggplotly(p)
    }
  })
  
  # MISSING DATA TAB
  
  # Missing data overview
  output$missing_overview <- renderPlot({
    req(rv$missing_analysis)
    
    if (!is.null(rv$missing_analysis$plots) && length(rv$missing_analysis$plots) > 0) {
      rv$missing_analysis$plots[[1]]
    }
  })
  
  # Missing data by variable
  output$missing_by_var <- renderPlot({
    req(rv$missing_analysis)
    
    if (!is.null(rv$missing_analysis$plots) && length(rv$missing_analysis$plots) > 1) {
      rv$missing_analysis$plots[[2]]
    }
  })
  
  # Missing data patterns
  output$missing_patterns <- renderPlot({
    req(rv$missing_analysis)
    
    if (!is.null(rv$missing_analysis$plots) && length(rv$missing_analysis$plots) > 2) {
      rv$missing_analysis$plots[[3]]
    }
  })
  
  # Missing data statistics
  output$missing_stats <- renderDT({
    req(rv$missing_analysis)
    
    if (!is.null(rv$missing_analysis$columns)) {
      missing_stats <- data.frame(
        Variable = names(rv$missing_analysis$columns),
        Missing_Count = sapply(rv$missing_analysis$columns, function(x) x$missing),
        Missing_Percent = sapply(rv$missing_analysis$columns, function(x) x$percent)
      )
      
      datatable(missing_stats, options = list(pageLength = 25))
    }
  })
  
  # CORRELATION TAB
  
  # Correlation plot
  output$corr_plot <- renderPlotly({
    req(rv$data)
    
    # Compute correlations with the selected method
    rv$correlation_analysis <- correlation_analysis(
      rv$data,
      method = input$corr_method,
      plot = TRUE,
      threshold = input$corr_threshold
    )
    
    if (!is.null(rv$correlation_analysis$plots) && length(rv$correlation_analysis$plots) > 0) {
      p <- rv$correlation_analysis$plots[[1]]
      ggplotly(p)
    }
  })
  
  # Strong correlations table
  output$strong_corr_table <- renderDT({
    req(rv$correlation_analysis)
    
    if (!is.null(rv$correlation_analysis$strong_correlations)) {
      datatable(
        rv$correlation_analysis$strong_correlations,
        options = list(pageLength = 10)
      )
    }
  })
  
  # MODEL RECOMMENDATIONS TAB
  
  # Basic model recommendations
  observeEvent(input$get_basic_recommendations, {
    req(rv$data, input$model_question)
    
    withProgress(message = 'Generating model recommendations...', {
      tryCatch({
        if (!exists("model_recommendations")) {
          # If function doesn't exist, load required functions
          source(system.file("R", "model_recommendations.R", package = "EDAhelper"))
        }
        
        rv$model_recommendations <- model_recommendations(
          rv$data,
          question = input$model_question,
          local_only = input$model_local_only,
          include_code = input$model_include_code
        )
      }, error = function(e) {
        showNotification(
          paste("Error generating recommendations:", e$message),
          type = "error",
          duration = NULL
        )
      })
    })
  })
  
  # Enhanced model recommendations
  observeEvent(input$get_enhanced_recommendations, {
    req(rv$data, input$study_plan)
    
    # Handle optional parameters
    target_var <- if (input$target_variable != "") input$target_variable else NULL
    model_fam <- if (input$model_family != "") input$model_family else NULL
    
    # Process required features
    req_features <- if (length(input$required_features) > 0) input$required_features else NULL
    
    # Process excluded models
    excl_models <- if (length(input$exclude_models) > 0) input$exclude_models else NULL
    
    withProgress(message = 'Generating enhanced model recommendations...', {
      tryCatch({
        if (!exists("enhanced_model_recommendations")) {
          # If function doesn't exist, load required functions
          source(system.file("R", "enhanced_model_recommendations.R", package = "EDAhelper"))
        }
        
        rv$model_recommendations <- enhanced_model_recommendations(
          data = rv$data,
          study_plan = input$study_plan,
          prioritize = input$prioritize,
          exclude_models = excl_models,
          model_family = model_fam,
          max_complexity = input$max_complexity,
          required_features = req_features,
          target_variable = target_var,
          local_only = input$enhanced_local_only,
          include_code = input$enhanced_include_code
        )
      }, error = function(e) {
        showNotification(
          paste("Error generating enhanced recommendations:", e$message),
          type = "error",
          duration = NULL
        )
      })
    })
  })
  
  # Display model recommendations (works for both basic and enhanced)
  output$model_recommendations <- renderPrint({
    req(rv$model_recommendations)
    
    # Detect if it's the enhanced version
    is_enhanced <- inherits(rv$model_recommendations, "enhanced_model_recommendations")
    
    # Format the output
    cat("## MODEL RECOMMENDATIONS\n\n")
    
    if (is_enhanced) {
      cat("Study Plan:", rv$model_recommendations$study_plan, "\n\n")
      
      # Print user preferences if available
      if (!is.null(rv$model_recommendations$user_preferences$priorities) && 
          length(rv$model_recommendations$user_preferences$priorities) > 0) {
        cat("Priorities:", paste(rv$model_recommendations$user_preferences$priorities, collapse = ", "), "\n")
      }
      
      if (!is.null(rv$model_recommendations$model_family)) {
        cat("Model Family:", rv$model_recommendations$model_family, "\n")
      }
      
      if (!is.null(rv$model_recommendations$excluded_models) && 
          length(rv$model_recommendations$excluded_models) > 0) {
        cat("Excluded Models:", paste(rv$model_recommendations$excluded_models, collapse = ", "), "\n")
      }
      cat("\n")
    } else {
      cat("Question:", rv$model_recommendations$question, "\n\n")
    }
    
    # Task Identification
    cat("### Task Identification\n")
    if (is_enhanced) {
      task_name <- switch(rv$model_recommendations$task_identification$type,
                         "classification" = "Classification",
                         "regression" = "Regression",
                         "clustering" = "Clustering",
                         "time_series" = "Time Series Forecasting",
                         "dimensionality_reduction" = "Dimensionality Reduction",
                         "unknown" = "Unknown (further clarification needed)")
      cat("Task Type:", task_name, "\n")
    } else {
      cat("Task Type:", rv$model_recommendations$task_identification$type, "\n")
    }
    
    # Target variables
    if (is_enhanced && !is.null(rv$model_recommendations$task_identification$target)) {
      cat("Target Variable:", rv$model_recommendations$task_identification$target, "\n")
    } else if (!is.null(rv$model_recommendations$task_identification$possible_targets)) {
      cat("Potential Target Variables:", paste(rv$model_recommendations$task_identification$possible_targets, collapse = ", "), "\n")
    }
    cat("\n")
    
    # Recommended Models
    cat("### Recommended Models\n")
    if (is_enhanced) {
      for (i in seq_along(rv$model_recommendations$prioritized_models)) {
        model <- rv$model_recommendations$prioritized_models[[i]]
        
        # Show model score if available
        score_text <- if (!is.null(model$score)) paste0(" (Score: ", round(model$score, 1), ")") else ""
        
        cat(i, ". ", model$name, score_text, "\n", sep = "")
        cat("   Family: ", model$family, "\n", sep = "")
        cat("   Characteristics:\n", sep = "")
        cat("     - Accuracy: ", paste(rep("★", model$accuracy), collapse = ""), "\n", sep = "")
        cat("     - Interpretability: ", paste(rep("★", model$interpretability), collapse = ""), "\n", sep = "")
        cat("     - Speed: ", paste(rep("★", model$speed), collapse = ""), "\n", sep = "")
        cat("     - Explainability: ", paste(rep("★", model$explainability), collapse = ""), "\n", sep = "")
        cat("     - Complexity: ", paste(rep("★", model$complexity), collapse = ""), "\n", sep = "")
        cat("   Features: ", paste(model$features, collapse = ", "), "\n\n", sep = "")
      }
    } else {
      for (i in seq_along(rv$model_recommendations$recommended_models)) {
        model <- rv$model_recommendations$recommended_models[[i]]
        cat(i, ". ", model$name, "\n", sep = "")
        cat("   ", model$description, "\n\n", sep = "")
      }
    }
    
    # Evaluation Metrics
    cat("### Evaluation Metrics\n")
    if (is_enhanced) {
      for (metric in rv$model_recommendations$evaluation_metrics) {
        cat("- ", metric, "\n", sep = "")
      }
    } else {
      for (i in seq_along(rv$model_recommendations$evaluation_metrics)) {
        metric <- rv$model_recommendations$evaluation_metrics[[i]]
        cat("- ", metric, "\n", sep = "")
      }
    }
    cat("\n")
    
    # Implementation Advice
    if (!is.null(rv$model_recommendations$implementation$advice) && 
        length(rv$model_recommendations$implementation$advice) > 0) {
      cat("### Implementation Considerations\n")
      for (advice in rv$model_recommendations$implementation$advice) {
        cat("- ", advice, "\n", sep = "")
      }
      cat("\n")
    }
    
    # Code Examples
    show_code <- (is_enhanced && input$enhanced_include_code) || 
                 (!is_enhanced && input$model_include_code)
    
    if (show_code && !is.null(rv$model_recommendations$implementation$code_examples)) {
      cat("### Implementation Code Examples\n\n")
      for (model_name in names(rv$model_recommendations$implementation$code_examples)) {
        cat(rv$model_recommendations$implementation$code_examples[[model_name]], "\n\n")
      }
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)