% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outlier_detection.R
\name{outlier_detection}
\alias{outlier_detection}
\title{Detect outliers in a dataset using multiple methods}
\usage{
outlier_detection(
  data,
  methods = "all",
  variables = NULL,
  threshold = 3,
  plot = TRUE
)
}
\arguments{
\item{data}{A dataframe to analyze}

\item{methods}{Character vector of outlier detection methods to use:
"iqr" (interquartile range), "zscore", "modified_zscore", "dbscan", or "all"}

\item{variables}{Character vector of specific variables to analyze for outliers.
If NULL (default), all numeric variables will be analyzed.}

\item{threshold}{Numeric threshold for outlier detection. For z-score methods,
this is the number of standard deviations. For IQR, this is the multiplier of IQR.}

\item{plot}{Logical, whether to create visualizations (default: TRUE)}
}
\value{
A list containing outlier detection results
}
\description{
This function implements several outlier detection methods and provides
visualizations to help identify outliers in the data.
}
\examples{
outlier_detection(mtcars)
outlier_detection(iris, methods = c("iqr", "zscore"))
}
