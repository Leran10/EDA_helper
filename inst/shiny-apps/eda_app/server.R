library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(EDAhelper)
library(scales)

server <- function(input, output, session) {
  
  # Reactive value to store the uploaded data
  rv <- reactiveValues(
    data = NULL,
    data_summary = NULL,
    missing_analysis = NULL,
    correlation_analysis = NULL,
    outlier_detection = NULL,
    model_recommendations = NULL,
    preprocessed_data = NULL,
    preprocessing_result = NULL
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
                         "Subgroup Analysis" = "subgroup",
                         "Outlier Detection" = "outliers",
                         "Model Recommendations" = "models"
                       ),
                       selected = c("data_summary", "missing_data", "correlations", "subgroup", "outliers")),
      
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
  
  # Generate report when button is clicked
  observeEvent(input$generate_report, {
    # Close the modal
    removeModal()
    
    # Generate report
    withProgress(message = 'Generating report...', value = 0, {
      # Variables based on user selections
      title <- input$report_title
      author <- input$report_author
      include_code <- input$report_include_code
      theme <- input$report_theme
      toc <- input$report_toc
      sections <- input$report_sections
      
      # Update progress
      incProgress(0.2, detail = "Preparing data...")
      
      # Create a temporary file for the report
      temp_file <- tempfile(fileext = ".html")
      
      # Make sure required analyses are run
      if ("data_summary" %in% sections && is.null(rv$data_summary)) {
        rv$data_summary <- data_summary(rv$data, include_plots = TRUE)
      }
      
      if ("missing_data" %in% sections && is.null(rv$missing_analysis)) {
        rv$missing_analysis <- missing_analysis(rv$data, plot = TRUE)
      }
      
      if ("correlations" %in% sections && is.null(rv$correlation_analysis)) {
        rv$correlation_analysis <- correlation_analysis(rv$data, method = input$corr_method, plot = TRUE)
      }
      
      if ("outliers" %in% sections && is.null(rv$outlier_detection) && !is.null(input$outlier_var)) {
        rv$outlier_detection <- outlier_detection(
          rv$data[, input$outlier_var, drop = FALSE],
          methods = input$outlier_methods,
          threshold = input$outlier_threshold,
          plot = TRUE
        )
      }
      
      incProgress(0.4, detail = "Generating report...")
      
      # Call the generate_eda_report function
      tryCatch({
        # Only show sections that were selected
        options <- list(
          include_summary = "data_summary" %in% sections,
          include_missing = "missing_data" %in% sections,
          include_correlations = "correlations" %in% sections,
          include_outliers = "outliers" %in% sections
        )
        
        # We'll pass the subgroup analysis as a special parameter if selected
        if ("subgroup" %in% sections && !is.null(input$subgroup_var) && !is.null(input$subgroup_by)) {
          options$include_subgroup <- TRUE
          options$subgroup_var <- input$subgroup_var
          options$subgroup_by <- input$subgroup_by
          options$subgroup_plot_type <- input$subgroup_plot_type
          options$subgroup_facet <- input$subgroup_facet
          options$subgroup_normalize <- input$subgroup_normalize
        } else {
          options$include_subgroup <- FALSE
        }
        
        # We'll pass model recommendations if selected and generated
        if ("models" %in% sections && !is.null(rv$model_recommendations)) {
          options$include_models <- TRUE
          options$model_recommendations <- rv$model_recommendations
        } else {
          options$include_models <- FALSE
        }
        
        # Generate the report
        report_path <- generate_eda_report(
          rv$data,
          output_file = temp_file,
          title = title,
          author = author,
          include_code = include_code,
          theme = theme,
          toc = toc,
          correlation_method = ifelse(is.null(input$corr_method), "pearson", input$corr_method),
          outlier_methods = ifelse(is.null(input$outlier_methods), c("iqr", "zscore"), input$outlier_methods),
          additional_options = options
        )
        
        # Update progress
        incProgress(0.8, detail = "Finalizing report...")
        
        # Create a download handler
        output$download_report <- downloadHandler(
          filename = function() {
            sanitized_title <- gsub("[^a-zA-Z0-9]", "_", input$report_title)
            paste0(sanitized_title, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
          },
          content = function(file) {
            file.copy(temp_file, file)
          }
        )
        
        # Trigger the download by clicking the download link
        shinyjs::runjs("$('#download_report_link')[0].click();")
        
      }, error = function(e) {
        showNotification(
          paste("Error generating report:", e$message),
          type = "error",
          duration = NULL
        )
      })
    })
  })
  
  # Create the download link (hidden)
  output$download_ui <- renderUI({
    downloadLink("download_report", "", id = "download_report_link", style = "display: none;")
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
        
        tabPanel("Subgroup Analysis",
          h4("Performing Subgroup Analysis"),
          p("The Subgroup Analysis feature allows you to explore how variables are distributed across different groups."),
          tags$ol(
            tags$li("Select a Target Variable to analyze"),
            tags$li("Choose a Group By Variable to create subgroups"),
            tags$li("Optional: Add a filter to focus on specific data"),
            tags$li("Select a plot type (histogram, boxplot, density, or bar chart)"),
            tags$li("Toggle 'Use Facet Wrap' to display separate plots for each group"),
            tags$li("Use 'Normalize Values' to compare distributions with different scales")
          ),
          h4("Example Use Cases"),
          tags$ul(
            tags$li("Compare income distributions across different regions"),
            tags$li("Analyze how age groups differ in their health metrics"),
            tags$li("Explore how categorical variables (like education level) are distributed by gender")
          )
        ),
        
        tabPanel("Tips & Tricks",
          h4("Useful Tips"),
          tags$ul(
            tags$li(strong("Interactive Plots:"), "Hover over plots to see details, zoom in/out, and download as images"),
            tags$li(strong("Tables:"), "Sort tables by clicking column headers, search using the search box"),
            tags$li(strong("Missing Data:"), "The Missing Data tab helps identify patterns in missing values"),
            tags$li(strong("Outliers:"), "Use multiple detection methods to confirm potential outliers"),
            tags$li(strong("Model Recommendations:"), "Enter a specific question for better model suggestions")
          ),
          h4("Example Questions for Model Recommendations"),
          tags$ul(
            tags$li("Predict income based on education, age and region"),
            tags$li("Classify patients into high and low health risk groups"),
            tags$li("Group customers into segments based on their characteristics")
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
    updateSelectInput(session, "outlier_var", choices = names(data)[sapply(data, is.numeric)])
    updateSelectInput(session, "subgroup_var", choices = names(data))
    updateSelectInput(session, "subgroup_by", choices = names(data))
    updateSelectInput(session, "subgroup_filter_var", choices = names(data))
    
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
    # Load the demo data
    tryCatch({
      # Try to use the package function if it's installed
      if (exists("load_demo_data", where = asNamespace("EDAhelper"))) {
        rv$data <- load_demo_data()
      } else {
        # Fallback to local file
        demo_file <- file.path("extdata", "demo_data.csv")
        rv$data <- read.csv(demo_file, stringsAsFactors = FALSE)
        rv$data$RegistrationDate <- as.Date(rv$data$RegistrationDate)
      }
      
      # Process the data
      process_data(rv$data)
      
      showNotification(
        "Demo data loaded successfully",
        type = "message",
        duration = 3
      )
      
    }, error = function(e) {
      showNotification(
        paste("Error loading demo data:", e$message),
        type = "error",
        duration = NULL
      )
    })
  })
  
  # PREPROCESSING TAB
  
  # Update drop columns selectInput when data changes
  observe({
    req(rv$data)
    updateSelectInput(session, "drop_cols", choices = names(rv$data))
  })
  
  # Apply preprocessing
  observeEvent(input$apply_preprocessing, {
    req(rv$data)
    
    # Parse fill value if provided
    fill_value <- NULL
    if (input$fill_value != "") {
      tryCatch({
        # Try to convert to numeric if it looks like a number
        if (grepl("^-?\\d*\\.?\\d+$", input$fill_value)) {
          fill_value <- as.numeric(input$fill_value)
        } else {
          # Otherwise use as character
          fill_value <- input$fill_value
        }
      }, error = function(e) {
        # If conversion fails, use as is
        fill_value <- input$fill_value
      })
    }
    
    # Process data
    withProgress(message = 'Preprocessing data...', value = 0, {
      tryCatch({
        incProgress(0.2, detail = "Applying preprocessing...")
        
        # Call preprocessing function
        rv$preprocessing_result <- preprocess_data(
          data = rv$data,
          impute_numeric = input$impute_numeric,
          impute_categorical = input$impute_categorical,
          encode_categorical = input$encode_categorical,
          drop_original = input$drop_original,
          scale = input$scale_data,
          center = input$center_data,
          drop_cols = input$drop_cols,
          fill_value = fill_value,
          impute_threshold = input$impute_threshold,
          knn_k = input$knn_k,
          verbose = FALSE  # Don't show messages in console
        )
        
        # Store the processed data
        rv$preprocessed_data <- rv$preprocessing_result$processed_data
        
        incProgress(0.8, detail = "Preparing results...")
        
        # Show success notification
        showNotification(
          "Data preprocessing completed successfully",
          type = "message",
          duration = 3
        )
        
      }, error = function(e) {
        showNotification(
          paste("Error in preprocessing:", e$message),
          type = "error",
          duration = NULL
        )
      })
    })
  })
  
  # Preview of preprocessed data
  output$preprocessed_preview <- renderDT({
    req(rv$preprocessed_data)
    datatable(
      rv$preprocessed_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
  })
  
  # Preprocessing summary
  output$preprocessing_summary <- renderPrint({
    req(rv$preprocessing_result)
    
    cat("Data Preprocessing Summary\n")
    cat("------------------------\n\n")
    
    # Original data dimensions
    cat("Original data dimensions: ", nrow(rv$preprocessing_result$original_data), " rows, ", 
        ncol(rv$preprocessing_result$original_data), " columns\n", sep = "")
    cat("Processed data dimensions: ", nrow(rv$preprocessing_result$processed_data), " rows, ", 
        ncol(rv$preprocessing_result$processed_data), " columns\n\n", sep = "")
    
    # Dropped columns
    if (length(rv$preprocessing_result$preprocessing_info$columns_dropped) > 0) {
      cat("Dropped columns: ", paste(rv$preprocessing_result$preprocessing_info$columns_dropped, collapse = ", "), "\n\n", sep = "")
    }
    
    # Numeric imputation
    info <- rv$preprocessing_result$preprocessing_info
    
    if (info$numeric_imputation$method != "none" && 
        length(info$numeric_imputation$columns) > 0) {
      cat("Numeric imputation (method: ", info$numeric_imputation$method, "):\n", sep = "")
      for (col in names(info$numeric_imputation$columns)) {
        col_info <- info$numeric_imputation$columns[[col]]
        cat("  - ", col, ": ", col_info$missing_count, " values (", 
            round(col_info$missing_percent, 1), "%)", sep = "")
        if (!is.null(col_info$impute_value)) {
          cat(", imputed with ", col_info$impute_value, sep = "")
        }
        cat("\n")
      }
      cat("\n")
    }
    
    # Categorical imputation
    if (info$categorical_imputation$method != "none" && 
        length(info$categorical_imputation$columns) > 0) {
      cat("Categorical imputation (method: ", info$categorical_imputation$method, "):\n", sep = "")
      
      # Explanation of imputation method
      if (info$categorical_imputation$method == "mode") {
        cat("Mode imputation replaces missing values with the most frequently occurring category.\n")
        cat("This preserves the natural distribution of values and is suitable when:\n")
        cat(" - The missing values are likely missing completely at random (MCAR)\n")
        cat(" - Maintaining the original categories is important\n")
        cat(" - The most frequent value is a sensible default\n\n")
      } else if (info$categorical_imputation$method == "new_category") {
        cat("New category imputation treats missingness as informative by creating a new 'Missing' category.\n")
        cat("This approach is suitable when:\n")
        cat(" - Missing values might not be random (MNAR)\n")
        cat(" - The fact that data is missing could be meaningful\n")
        cat(" - You want to explicitly track which values were originally missing\n\n")
      }
      
      for (col in names(info$categorical_imputation$columns)) {
        col_info <- info$categorical_imputation$columns[[col]]
        cat("  - ", col, ": ", col_info$missing_count, " values (", 
            round(col_info$missing_percent, 1), "%)", sep = "")
        if (!is.null(col_info$impute_value)) {
          cat(", imputed with '", col_info$impute_value, "'", sep = "")
        }
        cat("\n")
      }
      cat("\n")
    }
    
    # Encoding
    if (info$encoding$method != "none" && 
        length(info$encoding$columns) > 0) {
      cat("Categorical encoding (method: ", info$encoding$method, "):\n", sep = "")
      for (col in names(info$encoding$columns)) {
        col_info <- info$encoding$columns[[col]]
        cat("  - ", col, " → ", paste(col_info$new_columns, collapse = ", "), "\n", sep = "")
      }
      cat("\n")
    }
    
    # Scaling and centering
    if (info$scaling$applied) {
      cat("Scaled columns: ", paste(info$scaling$columns, collapse = ", "), "\n", sep = "")
    }
    
    if (info$centering$applied) {
      cat("Centered columns: ", paste(info$centering$columns, collapse = ", "), "\n", sep = "")
    }
  })
  
  # Column changes table
  output$column_changes <- renderDT({
    req(rv$preprocessing_result)
    
    orig_cols <- names(rv$preprocessing_result$original_data)
    processed_cols <- names(rv$preprocessing_result$processed_data)
    
    # Identify dropped and added columns
    dropped_cols <- setdiff(orig_cols, processed_cols)
    added_cols <- setdiff(processed_cols, orig_cols)
    unchanged_cols <- intersect(orig_cols, processed_cols)
    
    # Create a data frame showing column changes
    column_changes <- data.frame(
      Column = c(unchanged_cols, dropped_cols, added_cols),
      Status = c(rep("Unchanged", length(unchanged_cols)),
                rep("Dropped", length(dropped_cols)),
                rep("Added", length(added_cols)))
    )
    
    # Order by status for better display
    column_changes <- column_changes[order(column_changes$Status), ]
    
    datatable(column_changes)
  })
  
  # Data types table
  output$data_types <- renderDT({
    req(rv$preprocessed_data)
    
    # Get column types
    col_types <- sapply(rv$preprocessed_data, function(x) {
      if (is.numeric(x)) {
        if (is.integer(x)) "integer" else "numeric"
      } else if (is.factor(x)) {
        "factor"
      } else if (is.character(x)) {
        "character"
      } else if (is.logical(x)) {
        "logical"
      } else if (inherits(x, "Date")) {
        "date"
      } else if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
        "datetime"
      } else {
        "other"
      }
    })
    
    # Create data frame with column types
    data_types <- data.frame(
      Column = names(col_types),
      Type = unname(col_types),
      Missing = sapply(rv$preprocessed_data, function(x) sum(is.na(x))),
      Missing_Percent = sapply(rv$preprocessed_data, function(x) round(sum(is.na(x)) / length(x) * 100, 1)),
      Unique_Values = sapply(rv$preprocessed_data, function(x) length(unique(x)))
    )
    
    datatable(data_types)
  })
  
  # Use preprocessed data
  observeEvent(input$use_preprocessed, {
    req(rv$preprocessed_data)
    
    # Replace the main data with preprocessed data
    rv$data <- rv$preprocessed_data
    
    # Clear previous analyses
    rv$data_summary <- NULL
    rv$missing_analysis <- NULL
    rv$correlation_analysis <- NULL
    rv$outlier_detection <- NULL
    rv$model_recommendations <- NULL
    
    # Update variable selection inputs
    updateSelectInput(session, "summary_var", choices = names(rv$data))
    updateSelectInput(session, "outlier_var", choices = names(rv$data)[sapply(rv$data, is.numeric)])
    updateSelectInput(session, "subgroup_var", choices = names(rv$data))
    updateSelectInput(session, "subgroup_by", choices = names(rv$data))
    updateSelectInput(session, "subgroup_filter_var", choices = names(rv$data))
    
    # Show success notification
    showNotification(
      "Preprocessed data is now being used for analysis",
      type = "message",
      duration = 3
    )
    
    # Update tab to data summary
    updateTabItems(session, "tabs", "data_summary")
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
    
    if (!is.null(rv$missing_analysis$plots) && length(rv$missing_analysis$plots) > 0 && 
        !is.null(rv$missing_analysis$plots$heatmap)) {
      rv$missing_analysis$plots$heatmap
    } else {
      # Create a default plot when there's no missing data
      ggplot2::ggplot() + 
        ggplot2::annotate("text", x = 0.5, y = 0.5, 
                         label = "No missing values in the dataset", 
                         size = 6) +
        ggplot2::theme_void()
    }
  })
  
  # Missing data by variable
  output$missing_by_var <- renderPlot({
    req(rv$missing_analysis)
    
    if (!is.null(rv$missing_analysis$plots) && length(rv$missing_analysis$plots) > 0 && 
        !is.null(rv$missing_analysis$plots$column_plot)) {
      rv$missing_analysis$plots$column_plot
    } else {
      # Create a default plot when there's no missing data
      ggplot2::ggplot() + 
        ggplot2::annotate("text", x = 0.5, y = 0.5, 
                         label = "No missing values in the dataset", 
                         size = 6) +
        ggplot2::theme_void()
    }
  })
  
  # Missing data patterns
  output$missing_patterns <- renderPlot({
    req(rv$missing_analysis)
    
    if (!is.null(rv$missing_analysis$plots) && length(rv$missing_analysis$plots) > 0 && 
        !is.null(rv$missing_analysis$plots$correlation)) {
      rv$missing_analysis$plots$correlation
    } else {
      # Create a default plot when there's no missing data
      ggplot2::ggplot() + 
        ggplot2::annotate("text", x = 0.5, y = 0.5, 
                         label = "No missing value correlation to display", 
                         size = 6) +
        ggplot2::theme_void()
    }
  })
  
  # Missing data statistics
  output$missing_stats <- renderDT({
    req(rv$missing_analysis)
    
    if (!is.null(rv$missing_analysis$columns)) {
      # Fix the field names to match the missing_analysis.R structure
      missing_stats <- data.frame(
        Variable = names(rv$missing_analysis$columns),
        Missing_Count = sapply(rv$missing_analysis$columns, function(x) x$missing_count),
        Missing_Percent = sapply(rv$missing_analysis$columns, function(x) x$missing_percent)
      )
      
      datatable(missing_stats, options = list(pageLength = 25))
    } else {
      # Return an empty data frame with the correct structure if no missing data
      data.frame(
        Variable = character(0),
        Missing_Count = integer(0),
        Missing_Percent = numeric(0)
      )
    }
  })
  
  # CORRELATION TAB
  
  # Correlation plot
  output$corr_plot <- renderPlot({
    req(rv$data)
    
    # Compute correlations with the selected method
    rv$correlation_analysis <- correlation_analysis(
      rv$data,
      method = input$corr_method,
      plot = TRUE,
      threshold = input$corr_threshold
    )
    
    if (!is.null(rv$correlation_analysis$plots) && length(rv$correlation_analysis$plots) > 0) {
      # In correlation_analysis.R, plots are stored as functions that need to be called
      # Call the function to generate the plot rather than trying to convert to plotly
      rv$correlation_analysis$plots$numeric()
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
  
  # SUBGROUP ANALYSIS TAB
  
  # Dynamic UI for filter values
  output$subgroup_filter_value <- renderUI({
    req(rv$data, input$subgroup_filter_var)
    
    var <- input$subgroup_filter_var
    var_data <- rv$data[[var]]
    
    if (is.numeric(var_data)) {
      # For numeric variables, create a slider
      min_val <- min(var_data, na.rm = TRUE)
      max_val <- max(var_data, na.rm = TRUE)
      
      sliderInput(
        "subgroup_filter_num", 
        "Filter Range",
        min = min_val,
        max = max_val,
        value = c(min_val, max_val)
      )
    } else {
      # For categorical variables, create checkboxes
      unique_vals <- unique(var_data)
      unique_vals <- unique_vals[!is.na(unique_vals)]
      
      checkboxGroupInput(
        "subgroup_filter_cat",
        "Select Categories",
        choices = unique_vals,
        selected = unique_vals
      )
    }
  })
  
  # Filtered data based on subgroup selections
  filtered_data <- reactive({
    req(rv$data, input$subgroup_var, input$subgroup_by)
    
    data <- rv$data
    
    # Apply filter if enabled
    if (input$adv_filter) {
      req(input$subgroup_filter_var)
      
      var <- input$subgroup_filter_var
      var_data <- data[[var]]
      
      if (is.numeric(var_data)) {
        req(input$subgroup_filter_num)
        min_val <- input$subgroup_filter_num[1]
        max_val <- input$subgroup_filter_num[2]
        
        # Filter numeric data
        data <- data[var_data >= min_val & var_data <= max_val | is.na(var_data), ]
      } else {
        req(input$subgroup_filter_cat)
        # Filter categorical data
        data <- data[var_data %in% input$subgroup_filter_cat | is.na(var_data), ]
      }
    }
    
    return(data)
  })
  
  # Subgroup plot
  output$subgroup_plot <- renderPlotly({
    req(filtered_data(), input$subgroup_var, input$subgroup_by)
    
    data <- filtered_data()
    target_var <- input$subgroup_var
    group_var <- input$subgroup_by
    plot_type <- input$subgroup_plot_type
    
    # Check if variables exist
    if (!(target_var %in% names(data)) || !(group_var %in% names(data))) {
      return(NULL)
    }
    
    # Create a clean version of the data for plotting
    plot_data <- data %>%
      select(all_of(c(target_var, group_var))) %>%
      rename(target = !!target_var, group = !!group_var)
    
    # Handle missing values in grouping variable
    plot_data$group[is.na(plot_data$group)] <- "Missing"
    plot_data$group <- factor(plot_data$group)
    
    p <- NULL
    
    # Create different plot types based on variable types
    if (is.numeric(plot_data$target)) {
      
      # For numeric target variables
      if (plot_type == "hist") {
        # Histogram with facets
        if (input$subgroup_facet) {
          p <- ggplot(plot_data, aes(x = target, fill = group)) +
            geom_histogram(bins = input$subgroup_bins, alpha = 0.7, position = if(input$subgroup_normalize) "fill" else "stack") +
            facet_wrap(~ group, scales = "free_y") +
            scale_fill_viridis_d() +
            theme_minimal() +
            labs(title = paste("Distribution of", target_var, "by", group_var),
                 x = target_var, 
                 y = if(input$subgroup_normalize) "Proportion" else "Count") +
            theme(legend.position = "none")
        } else {
          # Overlaid histograms
          p <- ggplot(plot_data, aes(x = target, fill = group)) +
            geom_histogram(bins = input$subgroup_bins, alpha = 0.6, position = if(input$subgroup_normalize) "fill" else "identity") +
            scale_fill_viridis_d() +
            theme_minimal() +
            labs(title = paste("Distribution of", target_var, "by", group_var),
                 x = target_var, 
                 y = if(input$subgroup_normalize) "Proportion" else "Count")
        }
      } else if (plot_type == "density") {
        # Density plots
        if (input$subgroup_facet) {
          p <- ggplot(plot_data, aes(x = target, fill = group)) +
            geom_density(alpha = 0.7) +
            facet_wrap(~ group, scales = "free_y") +
            scale_fill_viridis_d() +
            theme_minimal() +
            labs(title = paste("Density of", target_var, "by", group_var),
                 x = target_var, y = "Density") +
            theme(legend.position = "none")
        } else {
          p <- ggplot(plot_data, aes(x = target, fill = group, color = group)) +
            geom_density(alpha = 0.2) +
            scale_fill_viridis_d() +
            scale_color_viridis_d() +
            theme_minimal() +
            labs(title = paste("Density of", target_var, "by", group_var),
                 x = target_var, y = "Density")
        }
      } else if (plot_type == "box") {
        # Boxplots
        if (input$subgroup_facet) {
          p <- ggplot(plot_data, aes(y = target, fill = group)) +
            geom_boxplot(alpha = 0.7) +
            facet_wrap(~ group, scales = "free_x") +
            scale_fill_viridis_d() +
            theme_minimal() +
            labs(title = paste("Boxplot of", target_var, "by", group_var),
                 y = target_var) +
            theme(legend.position = "none")
        } else {
          p <- ggplot(plot_data, aes(x = group, y = target, fill = group)) +
            geom_boxplot(alpha = 0.7) +
            scale_fill_viridis_d() +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(title = paste("Boxplot of", target_var, "by", group_var),
                 x = group_var, y = target_var)
        }
      }
    } else {
      # For categorical target variables
      if (plot_type == "bar") {
        # Calculate counts
        plot_data <- plot_data %>%
          group_by(group, target) %>%
          summarise(count = n(), .groups = "drop") %>%
          ungroup()
        
        # Add proportion calculation if normalized
        if (input$subgroup_normalize) {
          plot_data <- plot_data %>%
            group_by(group) %>%
            mutate(proportion = count / sum(count)) %>%
            ungroup()
        }
        
        # Bar chart with facets
        if (input$subgroup_facet) {
          p <- ggplot(plot_data, aes(
            x = target, 
            y = if(input$subgroup_normalize) proportion else count, 
            fill = target
          )) +
            geom_col(alpha = 0.7) +
            facet_wrap(~ group, scales = "free_y") +
            scale_fill_viridis_d() +
            theme_minimal() +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none"
            ) +
            labs(
              title = paste("Distribution of", target_var, "by", group_var),
              x = target_var, 
              y = if(input$subgroup_normalize) "Proportion" else "Count"
            )
        } else {
          # Grouped bar chart
          p <- ggplot(plot_data, aes(
            x = target, 
            y = if(input$subgroup_normalize) proportion else count, 
            fill = group
          )) +
            geom_col(position = "dodge", alpha = 0.7) +
            scale_fill_viridis_d() +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(
              title = paste("Distribution of", target_var, "by", group_var),
              x = target_var, 
              y = if(input$subgroup_normalize) "Proportion" else "Count"
            )
        }
      }
    }
    
    if (!is.null(p)) {
      ggplotly(p)
    }
  })
  
  # Subgroup statistics
  output$subgroup_stats <- renderDT({
    req(filtered_data(), input$subgroup_var, input$subgroup_by)
    
    data <- filtered_data()
    target_var <- input$subgroup_var
    group_var <- input$subgroup_by
    
    # Get the target and group variables
    target <- data[[target_var]]
    group <- data[[group_var]]
    
    # Replace NA in group with "Missing"
    group[is.na(group)] <- "Missing"
    
    # Create a data frame for statistics
    stats_data <- data.frame(target = target, group = group)
    
    if (is.numeric(target)) {
      # For numeric target, calculate statistics by group
      result <- stats_data %>%
        group_by(group) %>%
        summarise(
          count = n(),
          min = min(target, na.rm = TRUE),
          q1 = quantile(target, 0.25, na.rm = TRUE),
          median = median(target, na.rm = TRUE),
          mean = mean(target, na.rm = TRUE),
          q3 = quantile(target, 0.75, na.rm = TRUE),
          max = max(target, na.rm = TRUE),
          sd = sd(target, na.rm = TRUE),
          missing = sum(is.na(target)),
          .groups = "drop"
        )
    } else {
      # For categorical target, calculate frequencies by group
      result <- stats_data %>%
        group_by(group, target) %>%
        summarise(count = n(), .groups = "drop") %>%
        group_by(group) %>%
        mutate(
          proportion = count / sum(count),
          percentage = proportion * 100
        ) %>%
        ungroup()
    }
    
    datatable(result, options = list(pageLength = 25))
  })
  
  # OUTLIERS TAB
  
  # Update outlier detection when variable or methods change
  observe({
    req(rv$data, input$outlier_var, input$outlier_methods)
    
    # Filter to only keep the selected variable
    data_subset <- rv$data[, input$outlier_var, drop = FALSE]
    
    # Perform outlier detection
    rv$outlier_detection <- outlier_detection(
      data_subset,
      methods = input$outlier_methods,
      threshold = input$outlier_threshold,
      plot = TRUE
    )
  })
  
  # Outlier plot
  output$outlier_plot <- renderPlotly({
    req(rv$outlier_detection, input$outlier_var)
    
    var <- input$outlier_var
    
    if (!is.null(rv$outlier_detection$plots) && length(rv$outlier_detection$plots) > 0) {
      # Use the first plot
      p <- rv$outlier_detection$plots[[1]] +
        labs(title = paste("Outlier Detection for", var))
      
      ggplotly(p)
    }
  })
  
  # Outlier statistics
  output$outlier_stats <- renderDT({
    req(rv$outlier_detection, input$outlier_var, input$outlier_methods)
    
    var <- input$outlier_var
    
    # Prepare summary of outliers
    results <- data.frame(
      Method = character(),
      Outliers_Count = integer(),
      Percentage = numeric(),
      stringsAsFactors = FALSE
    )
    
    for (method in input$outlier_methods) {
      if (!is.null(rv$outlier_detection$outliers[[method]][[var]])) {
        outlier_indices <- rv$outlier_detection$outliers[[method]][[var]]$indices
        n_outliers <- length(outlier_indices)
        percent <- round(n_outliers / nrow(rv$data) * 100, 2)
        
        results <- rbind(results, data.frame(
          Method = method,
          Outliers_Count = n_outliers,
          Percentage = percent
        ))
      }
    }
    
    datatable(results, options = list(pageLength = 25))
  })
  
  # MODEL RECOMMENDATIONS TAB
  
  # Update target variable dropdown when data changes
  observe({
    req(rv$data)
    
    # Update target variable dropdown
    updateSelectInput(session, "target_variable", 
                     choices = c("None" = "", names(rv$data)), 
                     selected = "")
  })
  
  # Basic model recommendations
  observeEvent(input$get_basic_recommendations, {
    req(rv$data, input$model_question)
    
    withProgress(message = 'Generating model recommendations...', {
      tryCatch({
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
                         "Unknown (further clarification needed)")
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