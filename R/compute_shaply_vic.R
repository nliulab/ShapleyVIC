#' Fit logistic regression model using sklearn python library
#' @param x_train Numeric design matrix of predictors.
#' @param y_train Integer vector of outcome, preferably coded as 1 (event) vs -1
#'   (non-event).
#' @param solver Solver to use by the python function.
#' @return Returns a callable python model object to be used by
#'   \code{\link{compute_sage_value}}.
#' @importFrom reticulate py_module_available
#' @export
logit_model_python <- function(x_train, y_train, solver = "liblinear") {
  have_sklearn <- reticulate::py_module_available("sklearn")
  if (!have_sklearn) {
    warning(simpleWarning("Please install sklearn python library."))
    return(NULL)
  }
  sklearn$linear_model$LogisticRegression(solver = solver)$fit(
    as.matrix(x_train), as.vector(y_train)
  )
}
#' Compute SAGE values for a model
#' @param model_py A callable model object to pass to Python.
#' @param coef_vec Named numeric vector of model coefficients. The first element
#'   should be the intercept term.
#' @param var_names String vector of name of model coefficients. Default is
#'   \code{NULL}, in which case \code{names(coef_vec)} will be used.
#' @param x_test Numeric design matrix of predictors.
#' @param y_test Integer vector of outcome, preferably coded as 1 (event) vs -1
#'   (non-event).
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
compute_sage_value <- function(model_py, coef_vec = NULL,
                               var_names = NULL, x_test, y_test,
                               check_convergence = FALSE) {
  have_sage <- reticulate::py_module_available("sage")
  if (!have_sage) {
    warning(simpleWarning("Please install sage python library."))
    return(NULL)
  }
  if (!is.null(coef_vec)) {
    model_py$intercept_ <- coef_vec[1]
    model_py$coef_ <- matrix(coef_vec[-1], nrow = 1)
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
  imputer <- sage$MarginalImputer(model_py, as.matrix(x_test))
  estimator <- sage$PermutationEstimator(imputer, 'cross entropy')
  sage_values <- estimator(as.matrix(x_test), matrix(y_test, ncol = 1),
                           verbose = check_convergence)
  data.frame(coef = coef_vec, var_name = var_names,
             sage_value = c(NA, sage_values$values),
             sage_sd = c(NA, sage_values$std))
}
#' Compute logistic loss
#' @param coef_vec Regression coefficients of the logistic regression. The first
#'   element should be the intercept term.
#' @param design_mat Design matrix corresponding to the coefficient vector.
#' @param y Binary outcome. Events must be indicated by value \code{1}.
#'   Non-events may be \code{0} or \code{-1}.
#' @return Returns the logistic loss.
compute_loss_bin <- function(coef_vec, design_mat, y){
  y <- as.numeric(as.character(y))
  y <- ifelse(y == 1, 1, -1)
  sum(log(1 + exp(-y * (design_mat %*% coef_vec))))
}
