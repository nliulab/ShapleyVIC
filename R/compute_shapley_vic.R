#' Create a design matrix without the intercept term
#' @param formula Formula.
#' @param data A \code{data.frame} of predictors. Make sure categorical
#'   variables are properly encoded as factors.
#' @return Returns a numeric matrix.
model_matrix_no_intercept <- function(formula, data) {
  mat <- model.matrix(object = formula, data = data)
  if (ncol(mat) == 2) {
    matrix(mat[, -1], ncol = 1)
  } else {
    mat[, -1]
  }
}
#' Replace estimated coefficients in a Python object with prespeficied values.
replace_coef <- function(model_py, coef_vec) {
  model_py$intercept_ <- coef_vec[1]
  model_py$coef_ <- matrix(coef_vec[-1], nrow = 1)
  model_py
}
#' Fit logistic regression model using sklearn python library
#' @param x_train A \code{data.frame} of predictors from the training set. Make
#'   sure categorical variables are properly encoded as factors.
#' @param y_train A numeric vector of outcome, with events coded as 1.
#' @return Returns a callable python model object to be used by
#'   \code{\link{compute_shap_value}} and \code{\link{compute_sage_value}},
#'   where the estimated coefficients will be identical to those from the
#'   \code{glm} function.
#' @importFrom reticulate py_module_available
#' @export
logit_model_python <- function(x_train, y_train) {
  have_sklearn <- reticulate::py_module_available("sklearn")
  if (!have_sklearn) {
    warning(simpleWarning("Need to install sklearn python library."))
    print("installing now")
    reticulate::py_install("sklearn")
    if (reticulate::py_module_available("sklearn"))
    print("python library sklearn is now installed")
    else
    print("sklearn installation failed")
    return(NULL)
  }
  model_r <- glm(y_train ~ ., data = cbind(y_train = y_train, x_train),
                 family = "binomial")
  x_train_dm <- model_matrix_no_intercept(~ ., data = x_train)
  y_train <- as.integer(as.vector(y_train == 1))
  model_py <- sklearn$linear_model$LogisticRegression(solver = "liblinear")$fit(
    x_train_dm, y_train
  )
  coef_vec <- coef(model_r)
  replace_coef(model_py = model_py, coef_vec = coef_vec)
}
#' Compute SAGE values for a model
#' @inheritParams compute_shap_value
#' @param coef_vec Named numeric vector of model coefficients (including the
#'   intercept term as the first element), to replace the coefficients in
#'   \code{model_py}. Default is \code{NULL}, where the coefficients in
#'   \code{model_py} will be used.
#' @param var_names String vector of name of model coefficients. Default is
#'   \code{NULL}, in which case \code{names(coef_vec)} will be used.
#' @param check_convergence Whether to check convergence in SAGE algorithm (may
#'   slightly slow done the algorithm). Default is \code{FALSE}.
#' @return Returns a \code{data.frame} containing model class, model
#'   coefficients and their names, SAGE values and their standard deviations.
#'   Note that SAGE value and standard deviation is not defined for the
#'   intercept and therefore will be \code{NA}.
#' @examples
#' data("df_compas", package = "ShapleyVIC")
#' head(df_compas)
#' # The following requires python libraries sage and sklearn, otherwise NULL is
#' # returned. Small training and test sets are used to reduce run time.
#' m_optim <- ShapleyVIC::logit_model_python(x_train = df_compas[1:1000, -1],
#'                                           y_train = df_compas$y[1:1000])
#' if (!is.null(m_optim)) {
#'   ShapleyVIC::compute_sage_value(model_py = m_optim,
#'                                  var_names = names(df_compas)[-1],
#'                                  x_test = df_compas[1001:1100, -1],
#'                                  y_test = df_compas$y[1001:1100])
#' }
#' @importFrom reticulate py_module_available
#' @export
compute_sage_value <- function(model_py, coef_vec = NULL, var_names = NULL,
                               x_test, y_test, check_convergence = FALSE) {
  have_sage <- reticulate::py_module_available("sage")
  if (!sage) {
    warning(simpleWarning("Need to install sage python library."))
    print("installing now")
    reticulate::py_install("sage")
    if (reticulate::py_module_available("sage"))
      print("python library sage is now installed")
    else
      print("sage installation failed")
    return(NULL)
  }
  if (!is.null(coef_vec)) {
    model_py <- replace_coef(model_py = model_py, coef_vec = coef_vec)
    if (is.null(var_names)) var_names <- names(coef_vec)
  } else {
    coef_vec <- c(model_py$intercept_, as.vector(model_py$coef_))
    if (is.null(var_names)) var_names <- rep("", length(coef_vec))
  }
  if (!is.null(var_names)) {
    if (length(var_names) == (length(coef_vec) - 1)) {
      var_names <- c("intercept", var_names)
    } else if (length(var_names) != length(coef_vec)) {
      warning(simpleWarning("var_names has wrong dimension. Replaced by ''."))
      var_names <- NULL
    }
  }
  x_test_dm <- model_matrix_no_intercept(~ ., data = x_test)
  imputer <- sage$MarginalImputer(model_py, x_test_dm)
  estimator <- sage$PermutationEstimator(imputer, 'cross entropy')
  sage_values <- estimator(x_test_dm, matrix(y_test, ncol = 1),
                           verbose = check_convergence)
  data.frame(coef = coef_vec, var_name = var_names,
             sage_value = c(NA, sage_values$values),
             sage_sd = c(NA, sage_values$std))
}
#' Compute logistic loss
#' @param coef_vec Regression coefficients of the logistic regression. The first
#'   element should be the intercept term.
#' @param x A \code{data.frame} of predictors. Make sure categorical variables
#'   are properly encoded as factors.
#' @param y A numeric vector of outcome, with events coded as 1.
#' @return Returns the logistic loss.
compute_loss_bin <- function(coef_vec, x, y){
  y <- as.numeric(as.character(y))
  y <- ifelse(y == 1, 1, -1)
  design_mat <- model.matrix(~ ., data = x)
  sum(log(1 + exp(-y * (design_mat %*% coef_vec))))
}
