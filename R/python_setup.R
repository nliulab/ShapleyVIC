#' R wrapper for installing sage python library
#' @param method Installation method. By default, "auto" automatically finds a
#'   method that will work in the local environment. Change the default to force
#'   a specific installation method. Note that the "virtualenv" method is not
#'   available on Windows. See \code{\link[reticulate]{py_install}}.
#' @param conda The path to a conda executable. Use "auto" to allow reticulate
#'   to automatically find an appropriate conda binary. See
#'   \code{\link[reticulate]{py_install}}.
#' @export
install_sage <- function(method = "auto", conda = "auto") {
  reticulate::py_install("sage", method = method, conda = conda)
}
#' R wrapper for installing sklearn python library
#' @inheritParams install_sage
#' @export
install_sklearn <- function(method = "auto", conda = "auto") {
  reticulate::py_install("sklearn", method = method, conda = conda)
}

sklearn <- NULL
sage <- NULL
.onLoad <- function(libname, pkgname) {
  # delay load foo module (will only be loaded when accessed via $)
  sklearn <<- reticulate::import("sklearn", delay_load = TRUE)
  sage <<- reticulate::import("sage", delay_load = TRUE)
}
