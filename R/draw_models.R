#' Compute logistic loss
#' @param coef_vec Regression coefficients of the logistic regression. The first
#'   element should be the intercept term.
#' @param x A \code{data.frame} of predictors. Make sure categorical variables
#'   are properly encoded as factors.
#' @param y A numeric vector of outcome, with events coded as 1.
#' @return Returns the logistic loss.
#' @importFrom stats model.matrix
compute_loss_bin <- function(coef_vec, x, y){
  y <- as.numeric(as.character(y))
  y <- ifelse(y == 1, 1, -1)
  design_mat <- model.matrix(~ ., data = x)
  sum(log(1 + exp(-y * (design_mat %*% coef_vec))))
}
#' Generate M logistic regression models centered around the optimal model
#' @param coef_optim Regression coefficients of the optimal logistic regression
#'   (i.e., the model that minimized logistic loss).
#' @param coef_optim_var Variance-covariance matrix of \code{coef_optim}.
#' @param x A \code{data.frame} of predictors from the training set. Make
#'   sure categorical variables are properly encoded as factors.
#' @param y A numeric vector of outcome, with events coded as 1.
#' @param M Total number of models to generate.
#' @param u1 Lower bound of a uniform distribution.
#' @param u2 Upper bound of a uniform distribution.
#' @return Returns a \code{data.frame} with \code{M} rows.
#' @importFrom MASS mvrnorm
#' @importFrom stats runif
draw_models_initial <- function(coef_optim, coef_optim_var, x, y,
                                M = 800, u1 = 0.5, u2 = 20) {
  coef_names <- names(coef_optim)
  coef_optim <- as.numeric(coef_optim)
  k_vec <- runif(n = M, min = u1, max = u2)
  coef_mat <- do.call("rbind", lapply(1:M, function(i) {
    as.numeric(MASS::mvrnorm(n = 1, mu = coef_optim,
                             Sigma = coef_optim_var * k_vec[i]))
  }))
  colnames(coef_mat) <- coef_names
  as.data.frame(cbind(id = 1:M, coef_mat))
}
#' Mark whether newly generated models are eligible
#' @inheritParams draw_models_initial
#' @param coef_df A \code{data.frame} of models generated from
#'   \code{\link{draw_models_initial}}.
#' @param criterion Criterion for determining model eligibility. Currently
#'   supporting \code{"loss"}.
#' @param epsilon Nearly optimal models are defined as models with logistic loss
#'   less than (1+epsilon) times the minimum loss.
#' @return Returns \code{coef_df} with 2 additional columns, \code{perf_metric}
#'   and \code{eligible}.
mark_eligibility <- function(coef_df, coef_optim, x, y, criterion = "loss",
                             epsilon = 0.05) {
  criterion <- match.arg(arg = tolower(criterion), c("loss"))
  if (criterion == "loss") {
    loss_optim <- compute_loss_bin(coef_vec = coef_optim, x = x, y = y)
    loss_vec <- t(apply(coef_df, 1, function(coef_vec) {
      compute_loss_bin(coef_vec = coef_vec[-1], x = x, y = y)
    }))
    coef_df$perf_metric <- as.numeric(loss_vec) / loss_optim
    coef_df$eligible <- coef_df$perf_metric < (1 + epsilon)
  } else {
    warning(simpleWarning("Other criterion not yet supported. coef_df unchanged."))
  }
  coef_df
}
#' Plot performance metrics of sampled models
#' @param perf_metric Numeric vector of performance metrics for all sampled
#'   models.
#' @param eligible Numeric vector of the same length of \code{perf_metric},
#'   indicating whether each sample is eligible.
#' @param select (Optional) Numeric vector of the same length of
#'   \code{perf_metric}, indicating whether each sample is selected in the final
#'   sample.
#' @param x_range Numeric vector indicating the range of eligible values for
#'   performance metrics. Will be indicated by dotted vertical lines in plots.
#' @param plot_selected Whether performance metrics of selected models should be
#'   plotted in a second panel. Default is no.
#' @param x_breaks (Optional) If selected models are to be plotted, the breaks
#'   to use in the histogram.
#' @return Histogram(s) of model performance made using \code{ggplot}.
#' @import ggplot2
#' @importFrom gridExtra grid.arrange
plot_perf_metric <- function(perf_metric, eligible, select = NULL,
                             x_range, plot_selected = FALSE, x_breaks = NULL) {
  M <- length(perf_metric)
  p1 <- ggplot(data = data.frame(x = perf_metric), aes_string(x = "x")) +
    geom_histogram() +
    geom_vline(xintercept = x_range, lty = 2) +
    labs(x = "Ratio of loss to minimum loss",
         title = sprintf("Loss of %d sampled models", M),
         subtitle = sprintf("%d (%.1f%%) sampled models are eligible",
                            sum(eligible), sum(eligible) / M * 100)) +
    theme_bw()
  if (plot_selected) {
    p2 <- ggplot(data = data.frame(x = perf_metric[which(select == 1)]),
                 aes_string(x = "x")) +
      geom_histogram(breaks = x_breaks) +
      labs(x = "Ratio of loss to minimum loss",
           title = sprintf("%d final sampled models", sum(select))) +
      theme_bw()
    p <- gridExtra::grid.arrange(p1, p2, nrow = 1)
  } else {
    p <- p1
  }
  p
}
#' Generate nearly optimal logistic regression models from Rashomon set
#' @inheritParams draw_models_initial
#' @param epsilon Nearly optimal models are defined as models with logistic loss
#'   less than (1+epsilon) times the minimum loss. Default is 0.05.
#' @param n_final Final number of nearly optimal models to select (optional).
#' @return Returns a \code{data.frame} with \code{M} rows.
#' @export
draw_models <- function(coef_optim, coef_optim_var, x, y,
                        M = 800, u1 = 0.5, u2 = 20, epsilon = 0.05,
                        n_final = NULL) {
  df <- draw_models_initial(coef_optim = coef_optim,
                            coef_optim_var = coef_optim_var,
                            x = x, y = y, M = M, u1 = u1, u2 = u2)
  df <- mark_eligibility(coef_df = df, coef_optim = coef_optim, x = x, y = y,
                         criterion = "loss", epsilon = epsilon)
  if (!is.null(n_final)) {
    if (sum(df$eligible == 1) <= n_final) {
      message("Not enough sampled models are eligible. All are selected.")
      df$select <- ifelse(df$eligible == 1, 1, 0)
    } else {
      i_final <- sample(x = which(df$eligible == 1),
                        size = n_final, replace = FALSE)
      df$select <- 0
      df$select[i_final] <- 1
    }
    suppressMessages(
      plot_perf_metric(perf_metric = df$perf_metric, eligible = df$eligible,
                       select = df$select, x_range = c(1, 1 + epsilon),
                       plot_selected = TRUE,
                       x_breaks = seq(from = 1, to = 1 + epsilon, by = 0.005))
    )
    df[df$select == 1, setdiff(names(df), c("id", "eligible", "select"))]
  } else {
    suppressMessages(
      plot_perf_metric(perf_metric = df$perf_metric, eligible = df$eligible,
                       x_range = c(1, 1 + epsilon), plot_selected = FALSE)
    )
    df
  }
}
