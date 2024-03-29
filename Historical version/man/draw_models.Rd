% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draw_models.R
\name{draw_models}
\alias{draw_models}
\title{Generate nearly optimal logistic regression models from Rashomon set}
\usage{
draw_models(
  coef_optim,
  coef_optim_var,
  x,
  y,
  M = 800,
  u1 = 0.5,
  u2 = 20,
  epsilon = 0.05,
  n_final = NULL
)
}
\arguments{
\item{coef_optim}{Regression coefficients of the optimal logistic regression
(i.e., the model that minimized logistic loss).}

\item{coef_optim_var}{Variance-covariance matrix of \code{coef_optim}.}

\item{x}{A \code{data.frame} of predictors from the training set. Make
sure categorical variables are properly encoded as factors.}

\item{y}{A numeric vector of outcome, with events coded as 1.}

\item{M}{Total number of models to generate.}

\item{u1}{Lower bound of a uniform distribution.}

\item{u2}{Upper bound of a uniform distribution.}

\item{epsilon}{Nearly optimal models are defined as models with logistic loss
less than (1+epsilon) times the minimum loss. Default is 0.05.}

\item{n_final}{Final number of nearly optimal models to select (optional).}
}
\value{
Returns a \code{data.frame} with \code{M} rows.
}
\description{
Generate nearly optimal logistic regression models from Rashomon set
}
