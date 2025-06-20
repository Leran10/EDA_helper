% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_preprocessing.R
\name{impute_missing}
\alias{impute_missing}
\title{Impute missing values in a dataframe}
\usage{
impute_missing(
  data,
  numeric_method = "median",
  categorical_method = "mode",
  knn_k = 5,
  threshold = 50
)
}
\arguments{
\item{data}{A dataframe}

\item{numeric_method}{Character, method for numeric imputation: "mean", "median", "mode", "knn", or "none"}

\item{categorical_method}{Character, method for categorical imputation: "mode", "new_category", or "none"}

\item{knn_k}{Integer, number of neighbors for KNN imputation}

\item{threshold}{Numeric, maximum percentage of missing values to impute}
}
\value{
A dataframe with imputed values
}
\description{
Impute missing values in a dataframe
}
\examples{
# Create data with missing values
df <- data.frame(
  a = c(1, 2, NA, 4, 5),
  b = c("x", NA, "z", "x", NA)
)

# Impute missing values
impute_missing(df)
}
