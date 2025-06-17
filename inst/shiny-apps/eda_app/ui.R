library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(ggplot2)
library(plotly)

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
      menuItem("Data Preprocessing", tabName = "preprocessing", icon = icon("wrench")),
      menuItem("Data Summary", tabName = "data_summary", icon = icon("table")),
      menuItem("Missing Data", tabName = "missing_data", icon = icon("question-circle")),
      menuItem("Correlations", tabName = "correlations", icon = icon("chart-line")),
      menuItem("Subgroup Analysis", tabName = "subgroup", icon = icon("object-group")),
      menuItem("Outliers", tabName = "outliers", icon = icon("exclamation-triangle")),
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
      
      # Data Preprocessing Tab
      tabItem(tabName = "preprocessing",
        fluidRow(
          box(
            title = "Preprocessing Options",
            width = 4,
            collapsible = TRUE,
            
            # Missing Value Imputation section
            h4("Missing Value Imputation"),
            
            # Numeric imputation options
            selectInput("impute_numeric", "Numeric Imputation Method", 
                      choices = c("None" = "none", 
                                 "Mean" = "mean", 
                                 "Median" = "median",
                                 "Mode" = "mode",
                                 "K-Nearest Neighbors" = "knn"),
                      selected = "median"),
            
            # Conditional KNN parameters
            conditionalPanel(
              condition = "input.impute_numeric == 'knn'",
              sliderInput("knn_k", "Number of Neighbors (k)", 
                         min = 1, max = 20, value = 5, step = 1)
            ),
            
            # Categorical imputation options
            selectInput("impute_categorical", "Categorical Imputation Method", 
                      choices = c("None" = "none", 
                                 "Mode (Most Common)" = "mode", 
                                 "New Category ('Missing')" = "new_category"),
                      selected = "mode"),
            
            # Threshold for imputation
            sliderInput("impute_threshold", "Maximum % Missing to Impute", 
                       min = 0, max = 100, value = 50, step = 5),
            
            # Manual fill value
            textInput("fill_value", "Manual Fill Value (Optional)", value = ""),
            helpText("If provided, this value will be used to fill all missing values, regardless of column type."),
            
            hr(),
            
            # Encoding section
            h4("Categorical Encoding"),
            
            selectInput("encode_categorical", "Encoding Method", 
                      choices = c("None" = "none", 
                                 "One-Hot Encoding" = "one_hot", 
                                 "Label Encoding" = "label",
                                 "Binary (for 2-level factors)" = "binary"),
                      selected = "none"),
            
            checkboxInput("drop_original", "Drop Original Columns After Encoding", TRUE),
            
            hr(),
            
            # Scaling and normalization
            h4("Scaling & Normalization"),
            
            checkboxInput("scale_data", "Scale Data (Standardize Variance)", FALSE),
            checkboxInput("center_data", "Center Data (Zero Mean)", FALSE),
            
            hr(),
            
            # Column selection and filtering
            h4("Column Selection"),
            
            selectInput("drop_cols", "Columns to Drop", choices = NULL, multiple = TRUE),
            
            hr(),
            
            # Apply preprocessing button
            actionButton("apply_preprocessing", "Apply Preprocessing", 
                       icon = icon("play"),
                       style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 100%")
          ),
          
          tabBox(
            title = "Results",
            width = 8,
            tabPanel("Data Preview", 
                    DTOutput("preprocessed_preview"),
                    hr(),
                    actionButton("use_preprocessed", "Use This Processed Data", 
                               icon = icon("check"),
                               style = "color: #fff; background-color: #5cb85c; border-color: #4cae4c")
            ),
            tabPanel("Preprocessing Summary",
                    verbatimTextOutput("preprocessing_summary")
            ),
            tabPanel("Column Changes",
                    DTOutput("column_changes")
            ),
            tabPanel("Data Types", 
                    DTOutput("data_types")
            )
          )
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
            plotOutput("corr_plot", height = "500px")
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
      
      # Subgroup Analysis Tab
      tabItem(tabName = "subgroup",
        fluidRow(
          box(
            title = "Subgroup Analysis Options",
            width = 3,
            selectInput("subgroup_var", "Target Variable", choices = NULL),
            selectInput("subgroup_by", "Group By Variable", choices = NULL),
            conditionalPanel(
              condition = "input.adv_filter == true",
              selectInput("subgroup_filter_var", "Filter By", choices = NULL),
              uiOutput("subgroup_filter_value")
            ),
            radioButtons("subgroup_plot_type", "Plot Type",
                         choices = c("Histogram" = "hist", 
                                     "Boxplot" = "box", 
                                     "Density" = "density",
                                     "Bar Chart" = "bar"),
                         selected = "hist"),
            checkboxInput("adv_filter", "Add Filter", FALSE),
            checkboxInput("subgroup_facet", "Use Facet Wrap", TRUE),
            checkboxInput("subgroup_normalize", "Normalize Values", FALSE),
            conditionalPanel(
              condition = "input.subgroup_plot_type == 'hist'",
              sliderInput("subgroup_bins", "Number of Bins", 
                          min = 5, max = 100, value = 30, step = 5)
            )
          ),
          box(
            title = "Subgroup Plot",
            width = 9,
            plotlyOutput("subgroup_plot", height = "500px")
          )
        ),
        fluidRow(
          box(
            title = "Subgroup Statistics",
            width = 12,
            DTOutput("subgroup_stats")
          )
        )
      ),
      
      # Outliers Tab
      tabItem(tabName = "outliers",
        fluidRow(
          box(
            title = "Outlier Detection Options",
            width = 3,
            selectInput("outlier_var", "Select Variable", choices = NULL),
            checkboxGroupInput("outlier_methods", "Detection Methods",
                               choices = c("IQR" = "iqr", 
                                           "Z-Score" = "zscore", 
                                           "Modified Z-Score" = "modified_zscore",
                                           "DBSCAN" = "dbscan"),
                               selected = c("iqr", "zscore")),
            sliderInput("outlier_threshold", "Threshold", 
                         min = 1, max = 5, value = 3, step = 0.5)
          ),
          box(
            title = "Outlier Plot",
            width = 9,
            plotlyOutput("outlier_plot", height = "400px")
          )
        ),
        fluidRow(
          box(
            title = "Outlier Statistics",
            width = 12,
            DTOutput("outlier_stats")
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