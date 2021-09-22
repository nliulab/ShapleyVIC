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
  coef_vec <- as.numeric(coef_vec)
  if (anyNA(coef_vec)) {
    stop(simpleError("coef_vec should not contain non-numeric values or NA."))
  }
  model_py$intercept_ <- coef_vec[1]
  model_py$coef_ <- matrix(coef_vec[-1], nrow = 1)
  model_py
}
#' Fit logistic regression model using sklearn python library
#' @param x_train A \code{data.frame} of predictors from the training set. Make
#'   sure categorical variables are properly encoded as factors.
#' @param y_train A numeric vector of outcome from the training set, with events
#'   coded as 1.
#' @param save_to_file Where to save the fitted python model. Use \code{.sav}
#'   file extension. Default is \code{NULL}, i.e., not saved to external file.
#' @return Returns a callable python model object to be used by
#'   \code{\link{compute_shap_value}} and \code{\link{compute_sage_value}},
#'   where the estimated coefficients will be identical to those from the
#'   \code{glm} function.
#' @importFrom reticulate py_module_available
#' @export
logit_model_python <- function(x_train, y_train, save_to_file = NULL) {
  have_sklearn <- reticulate::py_module_available("sklearn")
  if (!have_sklearn) {
    warning(simpleWarning("Need to install sklearn python library."))
    # print("installing now")
    # reticulate::py_install("sklearn")
    # if (reticulate::py_module_available("sklearn"))
    # print("python library sklearn is now installed")
    # else
    # print("sklearn installation failed")
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
  model_py <- replace_coef(model_py = model_py, coef_vec = coef_vec)
  if (!is.null(save_to_file)) {
    message(sprintf("Saving python object to file '%s' ... ", save_to_file))
    reticulate::py_save_object(model_py, filename = save_to_file)
    message("saved.\n")
  }
  model_py
}
#' Compute SAGE values for a model
#' @inheritParams compute_shap_value
#' @param y_test A numeric vector of outcome from the test set, with events
#'   coded as 1.
#' @param coef_vec Numeric vector of model coefficients (including the
#'   intercept term as the first element), to replace the coefficients in
#'   \code{model_py}. Default is \code{NULL}, where the coefficients in
#'   \code{model_py} will be used.
#' @param check_convergence Whether to check convergence in SAGE algorithm (may
#'   slightly slow done the algorithm). Default is \code{FALSE}.
#' @return Returns a \code{data.frame} containing variable names, SAGE values
#'   and their standard deviations.
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
#' @importFrom reticulate py_module_available py_eval
#' @export
compute_sage_value <- function(model_py, x_test, y_test,
                               coef_vec = NULL, var_names = NULL,
                               check_convergence = FALSE) {
  if (is.null(dim(x_test)) || ncol(x_test) <= 1) {
    stop(simpleError("x_test should be a data.frame with at least 2 columns."))
  }
  loss_type <- "cross entropy"
  # loss_type <- match.arg(loss, choices = c("cross entropy", "mse"))
  have_sage <- reticulate::py_module_available("sage")
  if (!have_sage) {
    warning(simpleWarning("Need to install sage python library."))
    # print("installing now")
    # reticulate::py_install("sage")
    # if (reticulate::py_module_available("sage"))
    #   print("python library sage is now installed")
    # else
    #   print("sage installation failed")
    return(NULL)
  }
  if (!is.null(coef_vec)) {
    model_py <- replace_coef(model_py = model_py, coef_vec = coef_vec)
  } else {
    coef_vec <- c(model_py$intercept_, as.vector(model_py$coef_))
  }
  if (!is.null(var_names)) {
    if (length(var_names) != length(coef_vec)) {
      warning(simpleWarning("var_names has wrong dimension. Replaced by 'names(x_test)'."))
      var_names <- names(x_test)
    }
  } else {
    var_names <- names(x_test)
  }
  x_test_dm <- model_matrix_no_intercept(~ ., data = x_test)
  # Create a python array to indicate which columns in the design matrix
  # correspond to the same categorical variable.
  x_clusters <- find_clusters(data = x_test)
  x_groups <- paste0(
    "[",
    paste(lapply(x_clusters, function(cols) paste0("[", toString(cols - 1), "]")),
          collapse = ","), # python counts from 0
    "]"
  )
  x_groups_py <- reticulate::py_eval(x_groups, convert = FALSE)
  # Columns corresponding to the same categorical variable are handled together:
  imputer <- sage$GroupedMarginalImputer(model_py, x_test_dm, groups = x_groups_py)
  estimator <- sage$PermutationEstimator(imputer, loss_type)
  sage_values <- estimator(x_test_dm, matrix(y_test, ncol = 1),
                           verbose = check_convergence)
  data.frame(var_name = var_names, sage_value = sage_values$values,
             sage_sd = sage_values$std)
}
#' Compute ShapleyVIC values for nearly optimal models
#' @inheritParams compute_sage_value
#' @param model_py A Python callable model object that has a
#'   \code{predict_proba} function, or the path to a file that stores the model.
#' @param coef_mat A numeric matrix or \code{data.frame} of coefficients of
#'   nearly optimal models, where each row corresponds to a model and each
#'   column corresponds to a variable (or a category of a categorical variable).
#' @param perf_metric Performance metric of each model in \code{coef_mat}.
#' @param output_folder Folder to save ShapleyVIC values from individual models
#'   (optional).
#' @param n_cores Number of cores to use for parallel computing.
#' @importFrom car vif
#' @import parallel
#' @import doParallel
#' @import foreach
#' @export
compute_shapley_vic <- function(model_py, coef_mat, perf_metric,
                                x_test, y_test, var_names = NULL,
                                output_folder = NULL, n_cores) {
  # The model_py python object cannot be passed to individual parallel clusters,
  # therefore it is loaded from external file in every %dopar%
  if (is.character(model_py)) {# model_py was saved to file
    if (!file.exists(model_py)) {
      stop(simpleError("model_py file does not exist."))
    }
    is_temp <- FALSE
    model_py_file <- model_py
  } else {# create a temporary file to save model_py and remove on exit
    is_temp <- TRUE
    model_py_file <- tempfile(pattern = "model_py", fileext = "sav")
    reticulate::py_save_object(model_py, filename = model_py_file)
  }
  if (!is.null(output_folder)) {
    if (!file.exists(output_folder)) dir.create(output_folder)
  }
  if (is.null(var_names)) var_names <- names(x_test)
  # Run ShapleyVIC using foreach
  n_cores_total <- parallel::detectCores()
  if (n_cores >= n_cores_total - 1) {
    warning(simpleWarning(sprintf(
      "In total %d cores detected and %d cores requested. n_cores reset to %d (maximum feasible number).",
      n_cores_total, n_cores, n_cores_total - 1
    )))
    n_cores <- n_cores_total - 1
  }
  if (Sys.info()["sysname"] == "Windows") {
    par_type <- "PSOCK" # available for all systems, but slower than fork
  } else {
    par_type <- "FORK"
  }
  my_cluster <- parallel::makeCluster(n_cores, type = par_type)
  doParallel::registerDoParallel(cl = my_cluster)
  # foreach::getDoParRegistered()
  # foreach::getDoParWorkers()
  rows <- 1:nrow(coef_mat)
  df_sage <- foreach(
    i = rows, .combine = 'rbind', .packages = c("reticulate")
  ) %dopar% {
    coef_vec <- as.numeric(coef_mat[i, ])
    model_py = reticulate::py_load_object(filename = model_py_file)
    # model_py <- logit_model_python(x_train = x_train, y_train = y_train)
    df_sage_i <- compute_sage_value(model_py = model_py, coef_vec = coef_vec,
                                    var_names = var_names,
                                    x_test = x_test, y_test = y_test)
    df_sage_i <- cbind(model_id = i, df_sage_i, perf_metric = perf_metric[i])
    if (!is.null(output_folder)) {
      write.csv(
        df_sage_i,
        file = file.path(output_folder, paste0("shapley_vic_", i, ".csv")),
        row.names = FALSE
      )
    }
    df_sage_i
  }
  rownames(df_sage) <- NULL
  return(df_sage)
  on.exit({
    try({
      # message("Attempting to stop cluster ...\n")
      # cluster_stopped <- FALSE
      # while (!cluster_stopped) {
      doParallel::stopImplicitCluster()
      parallel::stopCluster(cl = my_cluster)
      #   cluster_stopped <- !foreach::getDoParRegistered() &
      #     foreach::getDoParWorkers() == 1
      # }
      # message("Cluster stopped.\n")
      if (is_temp) {
        message("Removing temporary model_py save file ...\n")
        fr <- file.remove(model_py_file)
        if (fr) message("Temporary file removed.\n")
      }
    })
  })
}
