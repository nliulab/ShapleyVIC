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

#' Install required Python libraries for Windows users
#' @description Installs Python libraries sage-importance, shap, pandas and
#'   sklearn to current Python inspector specified through in RStudio menu.
#' @export
install_python_lib <- function() {
  sys_info <- Sys.info()
  if (!sys_info["sysname"] == "Windows") {
    warning(simpleWarning("Current installer function is only developed for Windows machines."))
    return(NULL)
  }
  py_info <- reticulate::py_config()
  message(cat("Using Python:", py_info$python))
  
  message("Upgrading pip to latest version -----")
  system("python.exe -m pip install --upgrade pip")
  message("Upgrade complete -----")
  
  pkgs <- c("sage", "sklearn", "pandas", "shap")
  message("Installing Python libraries: sage-importance, shap, pandas, sklearn -----")
  system("pip.exe install --upgrade setuptools")
  inst_status <- rep(1, length(pkgs))
  inst_status[1] <- system("pip.exe install sage-importance")
  inst_status[2] <- system("pip.exe install sklearn")
  inst_status[3] <- system("pip.exe install pandas")
  inst_status[4] <- system("pip.exe install shap")
  names(inst_status) <- pkgs
  if (all(inst_status == 0)) {
    message("Installation complete -----")
  } else {
    stop(simpleError(cat(
      "Installation failed for ", 
      toString(names(inst_status[which(inst_status != 0)])), 
      ".\nFor help on Python setup and library installation, please ",
      "email us at yilin.ning[AT]duke-nus.edu.sg, or ", 
      "report issue at https://github.com/nliulab/ShapleyVIC/issues.", 
      sep = ""
    )))
  }
  load_status <- rep(FALSE, length(pkgs))
  load_sage <- tryCatch(reticulate::import("sage"), error = function(e) FALSE)
  load_status[1] <- load_sage != FALSE
  load_sklearn <- tryCatch(reticulate::import("sklearn"), error = function(e) FALSE)
  load_status[2] <- load_sklearn != FALSE
  load_pandas <- tryCatch(reticulate::import("pandas"), error = function(e) FALSE)
  load_status[3] <- load_pandas != FALSE
  load_shap <- tryCatch(reticulate::import("shap"), error = function(e) FALSE)
  load_status[4] <- load_shap != FALSE
  if (all(load_status == TRUE)) {
    message("Libraries installed can be loaded. Python setup completed. -----")
  } else {
    stop(simpleError(cat(
      "Failed to load library ", 
      toString(names(load_status[which(load_status == FALSE)])), 
      ".\nFor help on Python setup and library installation, please ",
      "email us at yilin.ning[AT]duke-nus.edu.sg, or ", 
      "report issue at https://github.com/nliulab/ShapleyVIC/issues.", 
      sep = ""
    )))
  }
}
