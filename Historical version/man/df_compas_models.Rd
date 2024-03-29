% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{df_compas_models}
\alias{df_compas_models}
\title{350 nearly optimal logistic regression models for recidivism prediction}
\format{
An object of class \code{data.frame} with 350 rows and 8 columns.
}
\usage{
df_compas_models
}
\description{
A \code{data.frame} with 350 rows (each corresponding to a
  model) and 8 columns, where the first 7 columns correspond to the
  regression coefficients and the 8th column correspond to the ratio of
  logistic loss to the minimum loss (i.e., logistic loss of model trained
  from training set).
}
\keyword{datasets}
