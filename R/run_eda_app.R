#' Launch the EDAhelper Shiny App
#'
#' This function launches an interactive Shiny application that provides a graphical
#' user interface for the EDAhelper package. The app allows users to upload data,
#' perform exploratory data analysis, visualize distributions with subgroup analysis,
#' detect outliers, and get model recommendations.
#'
#' @param browser Logical, whether to launch the app in a browser (TRUE) or in the RStudio viewer (FALSE).
#' Default is FALSE.
#' @param ... Additional arguments to pass to \code{shiny::runApp()}.
#'
#' @return No return value, called for side effects.
#'
#' @examples
#' \dontrun{
#' # Launch the EDAhelper Shiny app
#' run_eda_app()
#'
#' # Launch in browser
#' run_eda_app(browser = TRUE)
#' }
#'
#' @importFrom shiny runApp
#' @export
run_eda_app <- function(browser = FALSE, ...) {
  # Find app directory
  app_dir <- system.file("shiny-apps/eda_app", package = "EDAhelper")
  
  # If not found, try to find it relative to the current script
  if (app_dir == "") {
    # For development use
    if (file.exists("inst/shiny-apps/eda_app")) {
      app_dir <- "inst/shiny-apps/eda_app"
    } else {
      stop("Could not find Shiny app directory. Try reinstalling the package.",
           call. = FALSE)
    }
  }
  
  # Check for required packages
  required_pkgs <- c("shiny", "shinydashboard", "shinyjs", "DT", "ggplot2", 
                    "plotly", "dplyr", "tidyr", "scales")
  missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
  
  if (length(missing_pkgs) > 0) {
    stop("The following required packages are missing: ", 
         paste(missing_pkgs, collapse = ", "), 
         ". Please install them using install.packages().")
  }
  
  # Launch the app
  shiny::runApp(app_dir, launch.browser = browser, ...)
}