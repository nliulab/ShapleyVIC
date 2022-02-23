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
}
#' Check if running on Mac Silicon
is_arm_mac <- function() {
  sys_info <- Sys.info()
  is_arm <- length(grep(pattern = "ARM", x = sys_info["version"],
                        ignore.case = FALSE)) > 0
  sys_info["sysname"] == "Darwin" & is_arm
}

#' Install required Python libraries
#' @param install_shap Whether the SHAP Python library should be installed.
#'   Default is \code{TRUE}. See Details.
#' @description Installs Python libraries sage-importance, sklearn, pandas and
#'   shap (if \code{install_shap = TRUE}) to current Python inspector specified
#'   through in RStudio menu. 
#' @details Currently SHAP cannot be installed for Mac Silicon machines due to
#'   compatibility issues. To install the SHAP Python library on Windows
#'   machines, users must first install the Microsoft Visual C++ (version 14.0
#'   or later), which is not available by default. If users prefer not to or are
#'   unable to install this tool, they can choose not to install the SHAP Python
#'   library and not to use the \code{compute_shap_value} function.
#' @export
install_python_lib <- function(install_shap = TRUE) {
  sys_info <- Sys.info()
  if (sys_info["sysname"] == "Windows") {
    python <- "python.exe"
    pip <- "pip.exe"
  } else {
    if (is_arm_mac()) install_shap <- FALSE
    python <- "python3"
    pip <- "pip3"
  }
  py_info <- reticulate::py_config()
  message(cat("Using Python:", py_info$python))
  
  message("Upgrading pip to latest version -----")
  system(paste(python, "-m pip install --upgrade pip --no-warn-script-location"))
  message("Upgrade complete -----")
  
  pkgs <- c("sage-importance", "sklearn", "pandas")
  if (install_shap) pkgs <- c(pkgs, "shap")
  pkgs_imp <- pkgs
  pkgs_imp[1] <- "sage"
  message(paste("Installing Python libraries:", toString(pkgs), "-----"))
  system(paste(pip, "install --upgrade setuptools --no-warn-script-location"))
  inst_status <- unlist(lapply(pkgs, function(pkg) {
    tryCatch(system(paste(pip, "install", pkg, "--no-warn-script-location")), 
             error = function(e) 1) 
  }))
  if (all(inst_status == 0)) {
    message("Installation complete -----")
  } else {
    stop(simpleError(cat(
      "Installation failed for ", 
      toString(pkgs[which(inst_status != 0)]), 
      ".\nFor help on Python setup and library installation, please ",
      "email us at yilin.ning[AT]duke-nus.edu.sg, or ", 
      "report issue at https://github.com/nliulab/ShapleyVIC/issues.\n", 
      sep = ""
    )))
  }
  load_status <- unlist(lapply(pkgs_imp, function(pkg) {
    reticulate::py_module_available(pkg)
  }))
  if (all(load_status == TRUE)) {
    message("Libraries installed can be loaded. Python setup completed. -----")
  } else {
    stop(simpleError(cat(
      "Failed to load library ", 
      toString(pkgs[which(load_status == FALSE)]), 
      ".\nFor help on Python setup and library installation, please ",
      "email us at yilin.ning[AT]duke-nus.edu.sg, or ", 
      "report issue at https://github.com/nliulab/ShapleyVIC/issues.\n", 
      sep = ""
    )))
  }
}
