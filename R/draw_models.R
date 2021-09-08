#' Generate nearly optimal logistic regression models from Rashomon set
#' @param coef_optim Regression coefficients of the optimal logistic regression
#'   (i.e., the model that minimized logistic loss).
#' @param coef_optim_var Variance-covariance matrix of \code{coef_optim}.
#' @param x A \code{data.frame} of predictors from the training set. Make
#'   sure categorical variables are properly encoded as factors.
#' @param y A numeric vector of outcome, with events coded as 1.
#' @param M Total number of models to generate.
#' @param u1 Lower bound of a uniform distribution.
#' @param u2 Upper bound of a uniform distribution.
#' @param epsilon Nearly optimal models are defined as models with logistic loss
#'   less than (1+epsilon) times the minimum loss.
#' @param n_final Final number of nearly optimal models to select (optional).
#' @return Returns a \code{data.frame} with \code{M} rows.
#' @import ggplot2
#' @import gridExtra
#' @export
draw_models <- function(coef_optim, coef_optim_var, x, y,
                        M = 800, u1 = 0.5, u2 = 20, epsilon = 0.05,
                        n_final = NULL) {
  loss_optim <- compute_loss_bin(coef_vec = coef_optim, x = x, y = y)
  k_vec <- runif(n = M, min = u1, max = u2)
  coef_mat <- do.call("rbind", lapply(1:M, function(i) {
    as.numeric(MASS::mvrnorm(n = 1, mu = coef_optim,
                             Sigma = coef_optim_var * k_vec[i]))
  }))
  colnames(coef_mat) <- names(coef_optim)
  loss_vec <- t(apply(coef_mat, 1, function(coef_vec) {
    compute_loss_bin(coef_vec = coef_vec, x = x, y = y)
  }))
  df <- as.data.frame(cbind(
    id = 1:M,
    coef_mat,
    loss_ratio = as.numeric(loss_vec) / loss_optim,
    loss_in_rashomon = (as.numeric(loss_vec) / loss_optim) < (1 + epsilon)
  ))
  p1 <- ggplot(data = df, aes_string(x = "loss_ratio")) +
    geom_histogram() +
    geom_vline(xintercept = 1, lty = 2) +
    geom_vline(xintercept = 1 + epsilon, lty = 2) +
    labs(x = "Ratio of loss to minimum loss",
         title = sprintf("Loss of %d sampled models", M),
         subtitle = sprintf("%d sampled models are in the Rashomon set",
                            sum(df$loss_in_rashomon))) +
    theme_bw()
  if (!is.null(n_final)) {
    if (sum(df$loss_in_rashomon == 1) <= n_final) {
      message("Not enough sampled models are in Rashomon set. All are selected.")
      df$select <- ifelse(df$loss_in_rashomon == 1, 1, 0)
    } else {
      i_final <- sample(x = which(df$loss_in_rashomon == 1),
                        size = n_final, replace = FALSE)
      df$select <- 0
      df$select[i_final] <- 1
    }
    n_selected <- sum(df$select)
    p2 <- ggplot(data = df[which(df$select == 1), ],
                 aes_string(x = "loss_ratio")) +
      geom_histogram(breaks = seq(from = 1, to = 1.05, by = 0.005)) +
      labs(x = "Ratio of loss to minimum loss",
           title = sprintf("%d final sampled models", n_selected)) +
      theme_bw()
    p <- gridExtra::grid.arrange(p1, p2, nrow = 1)
  } else {
    p <- p1
  }
  print(p)
  return(df)
}
