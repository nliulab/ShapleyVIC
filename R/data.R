#' COMPAS data
#' @description A dataset containing the 2-year recidivism outcome and 6 binary
#'   variables of 7214 subjects, created from raw data
#'   (https://github.com/Jiayun-Dong/vic/blob/v1.0.0/compas.csv) and code
#'   (https://github.com/Jiayun-Dong/vic/blob/v1.0.0/compas-logistics.R) shared
#'   by Dong and Rudin (2020).
#' @format A data frame with 7214 rows and 7 binary variables:
#' \describe{
#'   \item{y}{2-year recidivism (the binary outcome, 1=event and 0=non-event).}
#'   \item{age}{Age dichotomised at 20.}
#'   \item{race}{Race, African American or otherwise.}
#'   \item{prior}{Presence of prior criminal history.}
#'   \item{gender}{Gender.}
#'   \item{juvenilecrime}{Presence of juvenile criminal history.}
#'   \item{currentcharge}{Current charge.}
#' }
"df_compas"
