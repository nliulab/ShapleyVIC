#' Compute SHAP values for a model
#' @param model_py A Python callable model object that has a
#'   \code{predict_proba} function.
#' @param var_names String vector of name of model coefficients. If unspecified,
#'   'X1', 'X2', etc will be used.
#' @param x_test A \code{data.frame} of predictors from the test set. Make sure
#'   categorical variables are properly encoded as factors.
#' @return Returns a \code{data.frame} of SHAP values, where each column
#'   corresponds to a variable and each row corresponds to an observation.
#' @examples
#' data("df_compas", package = "ShapleyVIC")
#' head(df_compas)
#' # The following requires python libraries shap, sklearn, numpy and pandas,
#' # otherwise NULL is returned. Small training and test sets are used to reduce
#' # run time.
#' m_optim <- ShapleyVIC::logit_model_python(x_train = df_compas[1:1000, -1],
#'                                           y_train = df_compas$y[1:1000])
#' if (!is.null(m_optim)) {
#'   shap_vals <- ShapleyVIC::compute_shap_value(model_py = m_optim,
# '                                              var_names = names(df_compas)[-1],
#'                                               x_test = df_compas[1001:1100, -1])
#'   dim(shap_vals)
#'   head(shap_vals)
#' }
#' @importFrom reticulate py_module_available
#' @export
compute_shap_value <- function(model_py, var_names = NULL, x_test) {
  have_shap <- reticulate::py_module_available("shap")
  if (!have_shap) {
    warning(simpleWarning("Please install shap python library."))
    return(NULL)
  }
  have_numpy <- reticulate::py_module_available("numpy")
  if (!have_numpy) {
    warning(simpleWarning("Please install numpy python library."))
    return(NULL)
  }
  have_pandas <- reticulate::py_module_available("pandas")
  if (!have_pandas) {
    warning(simpleWarning("Please install pandas python library."))
    return(NULL)
  }
  have_matplotlib <- reticulate::py_module_available("matplotlib")
  if (!have_matplotlib) {
    warning(simpleWarning("Please install matplotlib python library."))
    return(NULL)
  }
  coef_vec <- c(model_py$intercept_, as.vector(model_py$coef_))
  if (is.null(var_names)) var_names <- paste("X", seq_along(coef_vec[-1]))
  # Create a "lambda" for predict_proba to take data.frame instead of matrix:
  f <- function(x) {
    model_py$predict_proba(pandas$get_dummies(data = x, drop_first = TRUE))
  }
  explainer = shap$explainers$Permutation(f, x_test)
  shap_values = explainer(x_test)
  shap_values2 = shap_values
  # shap_values are computed for both y=1 and y=0, need to remove y=0:
  shap_values2$values = numpy$array(shap_values$values[, , -1])
  shap_values2$base_values = numpy$array(shap_values2$base_values[, -1])
  shap_values2$feature_names = var_names
  message("Use 'Preious Plot' and 'Next Plot' buttons in 'Plots' panel to navigate between beeswarm plot and bar plot of SHAP values.\n")
  f = plt$figure()
  plt$subplots_adjust(left = 0.3, right = 0.7)
  # Using blue bar style:
  shap$summary_plot(shap_values2$values, x_test, plot_type = "bar",
                    feature_names = shap_values2$feature_names,
                    max_display = as.integer(length(var_names)))
  # shap$plots$bar(shap_values2, max_display = as.integer(length(coef_vec) - 1))
  f = plt$figure()
  plt$subplots_adjust(left = 0.3, right = 0.7)
  shap$plots$beeswarm(shap_values2, max_display = as.integer(length(var_names)))
  # Return SHAP values as a data.frame (each column is a variable, each row is
  # an observation):
  shap_values <- as.data.frame(shap_values2$values)
  names(shap_values) <- shap_values2$feature_names
  shap_values
}
