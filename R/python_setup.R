sklearn <- NULL
sage <- NULL
shap <- NULL
plt <- NULL
numpy <- NULL
# pickle <- NULL
# pandas <- NULL
.onLoad <- function(libname, pkgname) {
  # delay load module (will only be loaded when accessed via $)
  sklearn <<- reticulate::import("sklearn", delay_load = TRUE)
  sage <<- reticulate::import("sage", delay_load = TRUE)
  shap <<- reticulate::import("shap", delay_load = TRUE)
  plt <<- reticulate::import("matplotlib.pyplot", delay_load = TRUE)
  numpy <<- reticulate::import("numpy", delay_load = TRUE)
  # pandas <<- reticulate::import("pandas", delay_load = TRUE)
  # pickle <<- reticulate::import("pickle", delay_load = TRUE)
}
