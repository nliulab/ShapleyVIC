#' Compute SAGE model reliance for a model
#' @param model_py A callable model object to pass to Python.
#' @param model_class Model class. Currently only supports \code{"logit"} for
#'   logistic regression model.
#' @param coef_vec Named numeric vector of model coefficients. The first element
#'   should be the intercept term.
#' @param X Numeric matrix of predictors.
#' @param y Integer vector of outcome, preferably coded as 1 (event) vs -1 (non-event).
#' @export
compute_mr_model <- function(model_py, model_class = "logit", coef_vec = NULL,
                             var_names = NULL,
                             X, y, check_convergence = FALSE) {
  if (!is.null(coef_vec)) {
    model_py$intercept_ <- coef_vec[1]
    model_py$coef_ <- matrix(coef_vec[-1], nrow = 1)
    if (is.null(var_names)) var_names <- names(coef_vec)
  } else {
    coef_vec <- c(model_py$intercept_, as.vector(model_py$coef_))
    if (is.null(var_names)) var_names <- ""
  }
  # reticulate::source_python("inst/compute_sage_value.py")
  sage_values <- compute_sage_value_logit(model = model_py, X_test = X, Y_test = y,
                                          verbose = check_convergence)
  data.frame(model_class = model_class,
             coef = coef_vec, var_name = var_names,
             sage_value = c(NA, sage_values$values),
             sage_sd = c(NA, sage_values$std))
}
#' Compute cross entropy loss for a logistic regression model
#' @param coef_vec Regression coefficients of the logistic regression. The first
#'   element should be the intercept term.
#' @param design_mat Design matrix corresponding to the coefficient vector.
#' @param y Binary outcome. Events must be indicated by value \code{1}.
compute_loss_bin <- function(coef_vec, design_mat, y){
  y <- as.numeric(as.character(y))
  y <- ifelse(y == 1, 1, -1)
  sum(log(1 + exp(-y * (design_mat %*% coef_vec))))
}
