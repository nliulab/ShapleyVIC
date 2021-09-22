#' COMPAS data
#' @description A dataset containing the 2-year recidivism outcome and 6 binary
#'   variables of 7214 subjects, created from raw data
#'   (https://github.com/Jiayun-Dong/vic/blob/v1.0.0/compas.csv) and code
#'   (https://github.com/Jiayun-Dong/vic/blob/v1.0.0/compas-logistics.R) shared
#'   by Dong and Rudin (2020).
#' @format A \code{data.frame} with 7214 rows and 7 binary variables from COMPAS
#'   data, and the 8th column indicating whether each observation belongs to the
#'   training or testing set:
#' \describe{
#'   \item{y}{2-year recidivism (the binary outcome, 1=event and 0=non-event).}
#'   \item{age}{Age dichotomised at 20.}
#'   \item{race}{Race, African American or otherwise.}
#'   \item{prior}{Presence of prior criminal history.}
#'   \item{gender}{Gender.}
#'   \item{juvenilecrime}{Presence of juvenile criminal history.}
#'   \item{currentcharge}{Current charge.}
#'   \item{train_test}{Whether the observation belonged to the training or test set.}
#' }
#' @seealso \code{\link{df_compas_models}}, \code{\link{df_compas_shapley_vic}}
"df_compas"
#' 350 nearly optimal logistic regression models for predicting COMPAS data
#' @description A \code{data.frame} with 350 rows (each corresponding to a
#'   model) and 8 columns, where the first 7 columns correspond to the
#'   regression coefficients and the 8th column correspond to the ratio of
#'   logistic loss to the minimum loss (i.e., logistic loss of model trained
#'   from training set).Code used to generated these 350 models is included in
#'   Example section.
#' @examples
#' data("df_compas")
#' set.seed(1234)
#' i_test <- sample(1:nrow(df_compas), size = round(nrow(df_compas) * 0.1),
#'                  replace = FALSE)
#' # i_test was later recorded as the `train_test` column
#' df_train <- df_compas[-i_test, ]
#' df_test <- df_compas[i_test, ]
#' m_optim_r <- glm(y ~ ., data = df_train, family = "binomial")
#' df_compas_models <- ShapleyVIC::draw_models(
#'   coef_optim = coef(m_optim_r),
#'   coef_optim_var = vcov(m_optim_r),
#'   x = df_train[, -1], y = df_train$y,
#'   M = 800, u1 = 0.5, u2 = 80, epsilon = 0.05,
#'   n_final = 350
#' )
#' head(df_compas_models)
#' @seealso \code{\link{df_compas}}, \code{\link{df_compas_shapley_vic}}
"df_compas_models"
#' ShapleyVIC values from the 350 nearly optimal models for COMPAS data
#' @format A \code{data.frame} with 2100 rows and 5 variables:
#' \describe{
#'   \item{model_id}{Model ID, ranging from 1 to 350.}
#'   \item{var_name}{Variable name as in \code{\link{df_compas}}.}
#'   \item{sage_value}{SAGE-based variable importance of the variable to the model.}
#'   \item{sage_sd}{Standard error of \code{sage_value}.}
#'   \item{perf_metric}{Performance of each model, measured as the ratio of
#'   logistic loss to the minimum loss (i.e., logistic loss of model trained
#'   from training set).}
#' }
#' @seealso \code{\link{df_compas}}, \code{\link{df_compas_models}}
"df_compas_shapley_vic"
