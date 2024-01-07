#' COMPAS data
#' @description A dataset containing the 2-year recidivism outcome and 6 binary
#'   variables of 7214 subjects, created from raw data
#'   (https://github.com/Jiayun-Dong/vic/blob/v1.0.0/compas.csv) and code
#'   (https://github.com/Jiayun-Dong/vic/blob/v1.0.0/compas-logistics.R) shared
#'   by Dong and Rudin (2020).
#' @format A \code{data.frame} with 7214 rows and 7 binary variables, and the
#'   8th column indicating whether each observation belongs to the training or
#'   testing set:
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
"df_compas"
#' 350 nearly optimal logistic regression models for recidivism prediction
#' @description A \code{data.frame} with 350 rows (each corresponding to a
#'   model) and 8 columns, where the first 7 columns correspond to the
#'   regression coefficients and the 8th column correspond to the ratio of
#'   logistic loss to the minimum loss (i.e., logistic loss of model trained
#'   from training set).
"df_compas_models"
#' ShapleyVIC values from the 350 nearly optimal models for recidivism prediction
#' @format A \code{data.frame} with 2100 rows and 6 variables:
#' \describe{
#'   \item{model_id}{Model ID, ranging from 1 to 350.}
#'   \item{var_names}{Variable names}.
#'   \item{sage_value_unadjusted}{Unadjusted SAGE variable omportance of the variable to the model.}
#'   \item{sage_sd}{Standard error of \code{sage_value}.}
#'   \item{shapley_vic_val}{ShapleyVIC value of the variable to the model, in this example same as unadjusted SAGE values.}
#'   \item{perf_metric}{Performance of each model, measured as the ratio of
#'   logistic loss to the minimum loss (i.e., logistic loss of model trained
#'   from training set).}
#' }
"df_compas_shapley_vic"
