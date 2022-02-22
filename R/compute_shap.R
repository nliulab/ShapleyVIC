#' Find clusters of columns that correspond to the same variable in one-hot coding
#' @param data A \code{data.frame} of predictors.
#' @details When a \code{data.frame} of predictors is converted to one-hot
#'   coding (i.e., a \code{model.matrix} without the column corresponding to the
#'   intercept term), columns that correspond to different levels of the same
#'   category variable belong to the same cluster. This is used to compute SHAP
#'   and SAGE values for categorical variables.
#' @return Returns a list of vectors, where the j-th vector contains the columns
#'   in the one-hot coding that correspond to a variable in the original
#'   \code{data.frame}.
#' @examples
#' df <- data.frame(x1 = 1:10,
#'                  x2 = factor(sample(letters[1:3], size = 10, replace = TRUE)),
#'                  x3 = factor(sample(c("f", "m"), size = 10, replace = TRUE)))
#' ShapleyVIC:::find_clusters(df)
find_clusters <- function(data) {
  var_ncol <- unlist(lapply(data, function(x) {
    if (class(x) == "factor") {
      length(levels(x)) - 1
    } else {
      1
    }
  }))
  var_start_end <- matrix(NA, ncol = 2, nrow = ncol(data))
  i_start <- 1
  for (j in 1:ncol(data)) {
    var_start_end[j, 1] <- i_start
    if (var_ncol[j] > 1) {
      i_end <- i_start + var_ncol[j] - 1
    } else {
      i_end <- i_start
    }
    var_start_end[j, 2] <- i_end
    i_start <- i_end + 1
  }
  lapply(1:ncol(data), function(j) {
    len <- as.integer(var_ncol[j])
    v <- vector("integer", length = len)
    v[1:len] <- as.integer(var_start_end[j, 1]:var_start_end[j, 2])
    v
  })
}
#' Check predictor names for test set
#' @param var_names A vector of variable names
#' @param x_test A \code{data.frame} of predictors from the test set.
check_var_names <- function(var_names, x_test) {
  if (!is.null(var_names)) {
    var_names <- as.character(var_names)
    if (length(var_names) != ncol(x_test)) {
      warning(simpleWarning("var_names has wrong dimension. Replaced by 'names(x_test)'."))
      var_names <- names(x_test)
    }
  } else {
    var_names <- names(x_test)
  }
  var_names
}
#' Compute SHAP values for a model
#' @param model_py A Python callable model object that has a
#'   \code{predict_proba} function.
#' @param x_test A \code{data.frame} of predictors from the test set. Make sure
#'   categorical variables are properly encoded as factors.
#' @param var_names String vector of variable names (not the names of regression
#'   coefficients, if categorical variables are involved). If unspecified,
#'   column names of \code{x_test} will be used.
#' @param plot Whether to plot SHAP values (default is \code{TRUE}).
#' @param left Numeric values between 0 and 1 controlling the boundaries of bar
#'   and beeswarm plots. The complete plotting region has a width and height of
#'   1, and the bottom left corner is (0,0). Default parameters \code{(left =
#'   0.3, right = 0.7, top = 0.8, bottom = 0.1)} means margins of 0.3 at left
#'   and right sides, and margins of 0.1 on the top and bottom.
#' @param right See \code{left}.
#' @param top See \code{left}.
#' @param bottom See \code{left}.
#' @return Returns a \code{data.frame} of SHAP values, where each column
#'   corresponds to a variable and each row corresponds to an observation. SHAP
#'   value of a categorical variable is the sum of SHAP values for all
#'   categories. Plots a bar plot of mean absolute SHAP values and a beeswarm
#'   plot of SHAP values (note that these plots do not work with RMarkdown yet).
#'   Users can save the two plots to an external PDF file by using the
#'   \code{pdf()} function (see Example section). Categorical variables are
#'   converted to integer values (i.e., 1 for first category, 2 for second
#'   category, etc) when plotting in the beeswarm plot.
#' @examples
#' data("df_compas", package = "ShapleyVIC")
#' head(df_compas)
#' # The following requires python libraries shap, sklearn and numpy,
#' # otherwise NULL is returned. Small training and test sets are used to reduce
#' # run time.
#' m_optim <- ShapleyVIC::logit_model_python(x_train = df_compas[1:1000, 2:7],
#'                                           y_train = df_compas$y[1:1000])
#' if (!is.null(m_optim)) {
#'   # pdf("shap_figures.pdf") # Add this to save the two SHAP plots to a PDF file
#'   shap_vals <- ShapleyVIC::compute_shap_value(
#'     model_py = m_optim,
#'     var_names = c("Age", "Race", "Prior criminal history", "Gender",
#'                   "Juvenile criminal history", "Current charge"),
#'     x_test = df_compas[1001:1100, 2:7],
#'     plot = FALSE # Change to `plot = TRUE` to plot SHAP values
#'   )
#'   # dev.off()
#'   dim(shap_vals)
#'   head(shap_vals)
#' }
#' @importFrom reticulate py_module_available
#' @export
compute_shap_value <- function(model_py, x_test, var_names = NULL, plot = TRUE,
                               left = 0.3, right = 0.7, top = 0.8, bottom = 0.1) {
  sys_info <- Sys.info()
  is_arm <- length(grep(pattern = "ARM", x = sys_info["version"],
                        ignore.case = FALSE)) > 0
  if (sys_info["sysname"] == "Darwin" & is_arm) {
    message("Currently not available for M1 Mac.\n")
    return(NULL)
  }
  if (is.null(dim(x_test)) || ncol(x_test) <= 1) {
    stop(simpleError("x_test should be a data.frame with at least 2 columns."))
  }
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
  have_matplotlib <- reticulate::py_module_available("matplotlib")
  if (!have_matplotlib) {
    warning(simpleWarning("Please install matplotlib python library."))
    return(NULL)
  }
  # Variable names, not names of coefficients:
  var_names <- check_var_names(var_names = var_names, x_test = x_test)
  x_test_dm <- model_matrix_no_intercept(~ ., data = x_test)
  explainer = shap$explainers$Permutation(model_py$predict_proba, x_test_dm)
  shap_values = explainer(x_test_dm)
  # shap_values are computed for both y=1 and y=0, need to remove y=0. Also need
  # to sum up SHAP values of different categories of the same variable.
  values_mat = numpy$array(shap_values$values[, , -1])
  # Check if necessary to combine categories in values:
  x_clusters <- find_clusters(data = x_test)
  len_clusters <- unlist(lapply(x_clusters,length))
  if (any(len_clusters > 1)) {
    values_mat <- do.call("cbind", lapply(x_clusters, function(cols) {
      mat <- values_mat[, cols]
      if (length(cols) > 1) apply(mat, 1, sum) else mat
    }))
  }
  if (plot) {
    message("If plots are not saved to external PDF file, use 'Preious Plot' and 'Next Plot' buttons in RStudio 'Plots' panel to navigate between the beeswarm plot and bar plot of SHAP values.\n")
    # Convert categorical variables to integers to plot (otherwise cannot plot):
    x_test_num <- do.call("cbind", lapply(x_test, as.numeric))
    f = plt$figure()
    plt$subplots_adjust(left = left, right = right, top = top, bottom = bottom)
    shap$summary_plot(values_mat, x_test_num, plot_type = "bar",
                      feature_names = var_names,
                      max_display = as.integer(length(var_names)))
    f = plt$figure()
    plt$subplots_adjust(left = left + 0.2, right = right + 0.2, top = top, bottom = bottom)
    violin <- FALSE # Whether to use violin plot offered by SHAP
    if (!violin) {
      shap$summary_plot(values_mat, x_test_num,
                        feature_names = var_names,
                        max_display = as.integer(length(var_names)))
    } else {
      shap$summary_plot(values_mat, x_test_num,
                        feature_names = var_names, plot_type = "violin",
                        max_display = as.integer(length(var_names)))
    }
  }
  # Return SHAP values as a data.frame (each column is a variable, each row is
  # an observation):
  shap_values <- as.data.frame(values_mat)
  names(shap_values) <- var_names
  shap_values
}
