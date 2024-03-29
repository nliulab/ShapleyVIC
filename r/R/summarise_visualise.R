#' Check if input data contains essential information from ShapleyVIC analysis
#' @param df_svic Input ShapleyVIC output.
#' @return If input passes check, returns it as \code{data.frame}. Otherwise
#'   returns \code{NULL} and prints data problems.
check_df_svic <- function(df_svic) {
  correct <- TRUE
  df_svic <- as.data.frame(df_svic)
  names_expected <- c("model_id", "var_names", "sage_value_unadjusted",
                      "sage_sd", "perf_metric")
  i_not_found <- which(!names_expected %in% names(df_svic))
  if (length(i_not_found) > 0) {
    correct <- FALSE
    warning(simpleWarning(paste(
      "The following required columns are not found in input ShapleyVIC output:",
      toString(names_expected[i_not_found])
    )))
  }
  for (x_name in names_expected[-(1:2)]) df_svic[, x_name] <- as.numeric(df_svic[, x_name])
  n_na <- unlist(lapply(df_svic, function(x) sum(is.na(x))))
  if (any(n_na > 0)) {
    correct <- FALSE
    warning(simpleWarning(paste(
      "NA/illegal values found in input ShapleyVIC output:",
      toString(names(n_na[which(n_na > 0)]))
    )))
  }
  # Number of entries per model, then summarise number of unique counts. Should
  # be same across all models (i.e., length 1)
  n_per_model <- table(table(df_svic$model_id))
  if (length(n_per_model) > 1) {
    correct <- FALSE
    warning(simpleWarning(paste(
      "Not all models have the same number of rows in input ShapleyVIC output."
    )))
    NULL
  }
  if (!correct) stop(simpleError("Please check input ShapleyVIC output."))
  df_svic
}
#' Compile ShapleyVIC values and compute overall variable importance
#' @param output_dir A string indicating path to the output folder created by
#'   the Python library.
#' @param outcome_type A string indicating type of outcome. Currently
#'   supports \code{"binary", "continuous", "ordinal"}.
#' @param criterion Criterion for defining nearly optimal used in the Python
#'   workflow, which can be "loss" (default, any outcome type), "auc" (binary
#'   outcome only) or "prauc" (binary outcome only).
#' @param x A \code{data.frame} of variables in the training set. Required if
#'   \code{x.csv} is not in \code{output_dir}. Ensure \code{x} is identical to 
#'   that used in the Python workflow.
#' @param y A vector of the outcome variable in the training set. Required if
#'   \code{y.csv} is not in \code{output_dir}. Ensure \code{y} is identical to 
#'   that used in the Python workflow.
#' @param x_names_cat A string vector indicating names of variables in raw data
#'   (i.e., \code{x}) to be treated as categorical. Must be the same as that
#'   used to compute ShapleyVIC values using the Python library.
#' @param x_names A string vector indicating alternative variable names to use
#'   in output and plots. If unspecified, variable names in raw data will be
#'   used.
#' @return Returns a \code{ShapleyVIC} object with two elements: (i) a
#'   \code{data.frame} of ShapleyVIC values from nearly optimal models, adjusted
#'   based on colinearity among predictors, and (ii) a \code{data.frame} of
#'   overall variable importance computed from ShapleyVIC values.
#' @importFrom stats glm lm
#' @importFrom utils read.csv
#' @export
compile_shapley_vic <- function(output_dir, outcome_type, criterion = "loss", 
                                x = NULL, y = NULL, 
                                x_names_cat = NULL, x_names = NULL) {
  outcome_type <- match.arg(
    arg = tolower(outcome_type), 
    choices = c("binary", "continuous", "ordinal")
  )
  criterion <- match.arg(
    arg = tolower(criterion), 
    choices = c("loss", "auc", "prauc")
  )
  if (criterion != "loss" & outcome_type != "binary") {
    stop(simpleError("For non-binary outcomes, only criterion='loss' is supported.\n Please enter the correct outcome_type and criterion used in the Python workflow."))
  }
  message(sprintf("Compiling results for %s outcome using %s criterion to define neaerly optimal models.", outcome_type, criterion))
  # x = NULL
  # y = NULL
  # Read ShapleyVIC output from Python library
  err_msg <- "Please provide valid output folder generated by ShapleyVIC Python library."
  if (!dir.exists(output_dir)) {
    stop(simpleError(paste(output_dir, "not found.", err_msg)))
  }
  f_svic <- file.path(output_dir, "df_svic.csv")
  if (!file.exists(f_svic)) {
    stop(simpleError(paste(f_svic, "not found.", err_msg)))
  }
  df_svic <- check_df_svic(df_svic = read.csv(f_svic)[, -1])
  f_x <- file.path(output_dir, "x.csv")
  f_y <- file.path(output_dir, "y.csv")
  if (!file.exists(f_x)) {
    if (is.null(x)) stop(paste(f_x, "not found.", simpleError(err_msg)))
  } else {
    x <- read.csv(f_x, check.names = FALSE)[, -1]
  }
  if (!file.exists(f_y)) {
    if (is.null(y)) stop(paste(f_y, "not found.", simpleError(err_msg)))
  } else {
    y <- read.csv(f_y, check.names = FALSE)[, -1]
  }
  
  if (!is.null(x_names_cat)) {
    x_names_cat <- unique(x_names_cat)
    x_names_raw <- names(x)
    n_in <- sum(x_names_cat %in% x_names_raw)
    if (n_in == 0) {
      stop(simpleError("None of 'x_names_cat' are in data. Please check."))
    }
    if (n_in < length(x_names_cat)) {
      warning(simpleWarning(sprintf(
        "The following 'x_names_cat' are not in data and are ignored: %s",
        toString(setdiff(x_names_cat, x_names_raw))
      )))
      x_names_cat <- intersect(x_names_cat, x_names_raw)
    }
    
    for (x_name in x_names_cat) x[, x_name] <- factor(x[, x_name])
  }
  
  if (outcome_type == "binary") {
    dat <- as.data.frame(cbind(.y = y, x))
    m <- glm(.y ~ ., data = dat, family = "binomial")
  } else if (outcome_type == "continuous") {
    dat <- as.data.frame(cbind(.y = y, x))
    m <- lm(.y ~ ., data = dat)
  } else if (outcome_type == "survival") {
    dat <- as.data.frame(cbind(.y = y$rank, x))
    m <- lm(.y ~ ., data = dat)
  } else if (outcome_type == "ordinal") {
    # Treat outcome as continuous to compute VIF
    dat <- as.data.frame(cbind(.y = as.numeric(y), x))
    m <- lm(.y ~ ., data = dat)
  } 
  var_vif <- car::vif(m)
  if (!is.null(dim(var_vif))) var_vif <- var_vif[, "GVIF"]
  
  df_svic$shapley_vic_val <- adjust_val(
    val = df_svic$sage_value_unadjusted,
    var_vif = var_vif, var_vif_threshold = 2
  )
  df_svic$var_names_raw <- df_svic$var_names
  if (!is.null(x_names)) {
    df_svic$var_names <- x_names[match(x = df_svic$var_names_raw, 
                                       table = names(x))]
  }
  df_bar <- summ_shapley_vic(
    val = df_svic$shapley_vic_val,
    val_sd = df_svic$sage_sd, var_names = df_svic$var_names
  )
  df_bar$significant <- df_bar$val_lower > 0
  obj <- list(models = df_svic, overall_importance = df_bar, 
              criterion = criterion,
              x = x, y = y)
  class(obj) <- "ShapleyVIC"
  obj
}
#' Identify variables with zero importance in all models, to exclude from plots
#' @param df_bar Data frame of overall variable importance.
#' @return Returns a vectors of variables to exclude (vector of zero length if
#'   none to exclude).
#' @importFrom stats complete.cases
vars_to_exclude <- function(df_bar) {
  # Variables with all zero from all models will have NA for overall importance
  i_zero <- which(!complete.cases(df_bar))
  if (length(i_zero) > 0) {
    vars <- df_bar$Variable[i_zero]
    message(simpleMessage("The following variables are excluded due to zero importance in all models analysed:\n"))
    message(simpleMessage(paste("\t", toString(vars), "\n")))
  } else {
    vars <- NULL
  }
  vars
}
#' Make bar plots for ShapleyVIC findings
#' @inheritParams draw_bars
#' @param x ShapleyVIC object generated using \code{\link{compile_shapley_vic}}.
#' @return Returns a bar plot that can be further edited using ggplot2.
#' @export
plot_bars <- function(x, title = NULL, subtitle = NULL) {
  df_bar <- x$overall_importance
  var_ordering <- levels(df_bar$Variable)
  vars <- vars_to_exclude(df_bar = df_bar)
  if (length(vars) > 0) {
    df_bar <- df_bar[-which(df_bar$Variable %in% vars), ]
    # Also need to adjust levels in variables for correct plotting
    var_ordering <- setdiff(var_ordering, vars)
    df_bar$Variable <- factor(df_bar$Variable, levels = var_ordering)
  }
  if (nrow(df_bar) > 0) {
    draw_bars(
      val = df_bar$val, 
      val_lower = df_bar$val_lower, val_upper = df_bar$val_upper,
      var_names = df_bar$Variable, title = title, subtitle = subtitle
    )
  } else {
    stop(simpleError("All variables have zero importance. Please investigate nearly optimal models generated."))
  }
}
#' Make violin plot for ShapleyVIC findings
#' @inheritParams draw_violins
#' @inheritParams plot_bars
#' @return Returns a violin plot. Use \code{plot_theme} to specify
#'   \code{theme()} settings.
#' @export
plot_violin <- function(x, title = NULL, plot_theme = NULL) {
  smaller_is_better <- x$criterion == "loss"
  df_svic <- x$models
  df_bar <- x$overall_importance
  var_ordering <- levels(df_bar$Variable)
  vars <- vars_to_exclude(df_bar = df_bar)
  if (length(vars) > 0) {
    df_bar <- df_bar[-which(df_bar$Variable %in% vars), ]
    var_ordering <- setdiff(var_ordering, vars)
    df_svic <- df_svic[-which(df_svic$var_names %in% vars), ]
  }
  if (nrow(df_bar) > 0) {
    draw_violins(
      var_names = df_svic$var_names, var_ordering = var_ordering,
      val = df_svic$shapley_vic_val, perf_metric = df_svic$perf_metric,
      smaller_is_better = smaller_is_better, title = title,
      boarder_size = 0.5, plot_theme = plot_theme
    )
  } else {
    stop(simpleError("All variables have zero importance. Please investigate nearly optimal models generated."))
  }
}
#' Plot ShapleyVIC findings
#' @param x ShapleyVIC object generated using \code{\link{compile_shapley_vic}}.
#' @param ... Not implemented
#' @return Displays bar and violin plots and returns the two plot objects as a
#'   list.
#' @export
plot.ShapleyVIC <- function(x, ...) {
  p_bar <- plot_bars(x = x)
  plot(p_bar)
  p_vio <- suppressMessages(plot_violin(x = x))
  plot(p_vio)
}
#' Print ShapleyVIC object
#' @inheritParams plot.ShapleyVIC
#' @param digits Number of digits to print for numeric values. Default is 5.
#' @importFrom knitr kable
#' @export
print.ShapleyVIC <- function(x, digits = 5, ...) {
  df_bar <- x$overall_importance
  print(knitr::kable(df_bar, digits = digits, row.names = FALSE,
                     col.names = c("Variable", "Overall importance",
                                   "95% PI, lower bound", "95% PI, upper bound",
                                   "Significant")))
}
#' Rank variables based on pairwise comparison of model reliance
#' @inheritParams plot.ShapleyVIC
#' @param summarise If \code{TRUE}, ranks will be summarised across models by
#'   taking the numeric average. Default is \code{FALSE}, where ranks for
#'   individual models will be returned.
#' @param filter Whether to filter out variables with non-significant overall
#'   importance. Default is \code{TRUE}.
#' @param as_vector If \code{summarise = TRUE}, whether to return variable ranks
#'   as a named vector (if \code{TRUE}, and using raw variable names), or as a
#'   \code{data.frame} (if \code{FALSE}, default).
#' @return Returns an integer vector of ranks of variables for each model (in
#'   descending order). Model reliance (possibly with 95\% confidence interval)
#'   is compared between all possible pairs of variables. Variables with model
#'   reliance significantly than the other variable in more pairwise comparisons
#'   are ranked higher.
#' @importFrom dplyr group_by summarise arrange `%>%`
#' @importFrom rlang .data
#' @export
rank_variables <- function(x, summarise = FALSE, filter = TRUE, as_vector = FALSE) {
  df_svic <- x$models
  df_bar <- x$overall_importance
  vars <- vars_to_exclude(df_bar = df_bar)
  if (length(vars) == nrow(df_bar)) {
    stop(simpleError("All variables have zero importance. Please investigate nearly optimal models generated."))
  }
  if (length(vars) > 0) {
    df_bar <- df_bar[-which(df_bar$Variable %in% vars), ]
    df_svic <- df_svic[-which(df_svic$var_names %in% vars), ]
  }
  model_ids <- unique(df_svic$model_id)
  val_ranks <- do.call("rbind", lapply(sort(model_ids), function(id) {
    m_i <- df_svic[df_svic$model_id == id, ]
    data.frame(
      model_id = id,
      Variable = m_i$var_names,
      rank = rank_vars(val = m_i$shapley_vic_val, val_sd = m_i$sage_sd,
                       ties.method = "min")
    )
  }))
  if (length(vars) > 0) {
    val_ranks_excl <- data.frame(
      model_id = rep(model_ids, each = length(vars)),
      Variable = rep(vars, length(model_ids)),
      rank = nrow(df_bar)
    )
    val_ranks <- rbind(val_ranks, val_ranks_excl)
    val_ranks <- val_ranks[sort.list(val_ranks$model_id), ]
  }
  rownames(val_ranks) <- NULL
  if (summarise) {
    val_ranks_summ <- val_ranks %>% dplyr::group_by(.data$Variable) %>%
      dplyr::summarise(mean_rank = mean(.data$rank)) %>%
      dplyr::arrange(.data$mean_rank) %>%
      as.data.frame()
    if (filter) {
      vars_include <- df_bar$Variable[which(df_bar$val_lower > 0)]
      val_ranks_summ <- val_ranks_summ[which(
        val_ranks_summ$Variable %in% vars_include), ]
    }
    if (as_vector) {
      # Here use x$models, not df_svic, to keep all variable names
      # x_names_map <- unique(x$models[, c("var_names_raw", "var_names")])
      val_ranks_vec <- val_ranks_summ$mean_rank
      # names(val_ranks_vec) <- x_names_map$var_names_raw[match(
      #   x = val_ranks_summ$Variable, table = x_names_map$var_names)]
      names(val_ranks_vec) <- val_ranks_summ$Variable
      val_ranks_vec
    } else {
      val_ranks_summ
    }
  } else {
    val_ranks
  }
}
#' Perform random effects meta-analysis to summarize ShapleyVIC results
#' @param val A vector of ShapleyVIC values.
#' @param val_sd A vector of standard deviation of ShapleyVIC values. Must be
#'   the same length as \code{val}.
#' @return Returns a named vector of summary statistics from the random effects
#'   model analysis of ShapleyVIC values, including the estimated \code{mean},
#'   the upper and lower of prediction interval (\code{pred_lower} and
#'   \code{pred_upper}).
#' @importFrom meta metagen
compute_meta_interval <- function(val, val_sd) {
  val <- as.numeric(as.vector(val))
  val_sd <- as.numeric(as.vector(val_sd))
  if (anyNA(val)) stop(simpleError("NA not allowed in val."))
  if (anyNA(val_sd)) stop(simpleError("NA not allowed in val_sd."))
  if (length(val) != length(val_sd)) {
    stop(simpleError("val and val_sd must have the same length."))
  }
  mt <- suppressWarnings(meta::metagen(TE = val, seTE = val_sd, studlab = 1:length(val),
                                       fixed = FALSE, random = TRUE,
                                       method.tau = "DL", method.ci = "t"))
  summ <- summary(mt)
  c(mean = summ$random$TE,
    pred_lower = summ$predict$lower, pred_upper = summ$predict$upper)
}
#' Adjust raw SAGE values based on VIC
#' @param val A long vector of unadjusted SAGE values
#' @param var_vif A vector of VIF values for each variable. Variables must be in
#'   the same order as in \code{val}.
#' @param var_vif_threshold Threshold for \code{var_vif} to consider as "high
#'   colinearity".
#' @return Returns adjusted values, same length as \code{val}.
adjust_val <- function(val, var_vif, var_vif_threshold) {
  var_vif <- as.numeric(var_vif)
  var_vif_threshold <- as.numeric(var_vif_threshold)[1]
  if (var_vif_threshold <= 0) {
    warning(simpleWarning("var_vif_threshold must be a positive number."))
  }
  if (length(var_vif) != length(val)) {
    if (length(var_vif) > length(val)) {
      stop(simpleError("length(var_vif) must be either length(val) or the number of predictors."))
    } else {
      d <- as.integer(length(val) / length(var_vif))
      var_vif <- rep(var_vif, d)
      if (length(var_vif) != length(val)) {
        stop(simpleError("length(val) must be an integer multiple of length(var_vif)."))
      }
    }
  }
  # Now var_vif is converted to same length as val
  ifelse(var_vif > var_vif_threshold, abs(val), val)
}
#' Summarize ShapleyVIC values
#' @param val A vector of ShapleyVIC values from all models evaluated.
#' @param val_sd A vector of standard deviation of ShapleyVIC values from all
#'   models evaluated. Must be the same length as \code{val}. If not available,
#'   assign to \code{NULL}.
#' @param var_names A vector of variable names.
#' @return Returns a \code{data.frame} of variable names, their average
#'   ShapleyVIC values, and the lower and upper bounds of 95\% prediction
#'   intervals (\code{NA} if \code{val_sd = NULL}).
summ_shapley_vic <- function(val, val_sd, var_names) {
  val <- as.numeric(as.vector(val))
  if (anyNA(val)) stop(simpleError("NA not allowed in val."))
  if (!is.null(val_sd)) {
    val_sd <- as.numeric(as.vector(val_sd))
    if (anyNA(val_sd)) stop(simpleError("NA not allowed in val_sd."))
    if (length(val) != length(val_sd)) {
      stop(simpleError("val and val_sd must have the same length."))
    }
  }
  var_names <- as.character(as.vector(var_names))
  if (!is.null(val_sd)) {
    df_summ <- do.call("rbind", lapply(unique(var_names), function(var) {
      val_i <- val[var_names == var]
      val_sd_i <- val_sd[var_names == var]
      summ_vec <- suppressWarnings(compute_meta_interval(val = val_i, val_sd = val_sd_i))
      data.frame(Variable = var, val = summ_vec["mean"],
                 val_lower = summ_vec["pred_lower"],
                 val_upper = summ_vec["pred_upper"],
                 stringsAsFactors = FALSE)
    }))
    df_summ$sign <- 0
    df_summ$sign[df_summ$val_lower > 0] <- 1
    df_summ$sign[df_summ$val_upper < 0] <- -1
  } else {
    df_summ <- do.call("rbind", lapply(unique(var_names), function(var) {
      val_i <- val[var_names == var]
      data.frame(Variable = var, val = mean(val_i),
                 val_lower = NA,  val_upper = NA,
                 stringsAsFactors = FALSE)
    }))
    df_summ$sign <- 0
    df_summ$sign[df_summ$val > 0] <- 1
    df_summ$sign[df_summ$val < 0] <- -1
  }
  df_summ$Variable <- factor(
    df_summ$Variable,
    levels = df_summ$Variable[order(df_summ$val, df_summ$sign)]
  )
  df_summ <- df_summ[sort.list(df_summ$val, decreasing = TRUE), ]
  rownames(df_summ) <- NULL
  df_summ[, setdiff(names(df_summ), "sign")]
}
#' Draw bar plot for model reliance.
#' @param val A numeric vector of model reliance.
#' @param val_sd A numeric vector of standard deviations of model reliance (with
#'   the same length as \code{val}), if available. Alternatively, provide
#'   \code{val_lower} and \code{val_upper} instead of \code{val_sd} to reflect
#'   uncertainty in \code{val}.
#' @param val_lower A numeric vector of the lower bound of 95\% confidence
#'   interval of model reliance (with the same length as \code{val}), if
#'   available.
#' @param val_upper A numeric vector of the upper bound of 95\% confidence
#'   interval of model reliance (with the same length as \code{val}), if
#'   available.
#' @param var_names A factor or string vector of variable names (with the same
#'   length as \code{val}). If a factor is provided, variables will be plotted
#'   as ordered in \code{levels(var_names)}, otherwise variables will be ordered
#'   by \code{val}. If unspecified, variables will be named 'X1', 'X2', etc.
#' @param title Title of the bar plot (optional).
#' @param subtitle Subtitle of the bar plot (optional).
#' @return Returns a ggplot object for the bar plot.
#' @import ggplot2
#' @importFrom rlang .data
draw_bars <- function(val, val_sd = NULL, val_lower = NULL, val_upper = NULL,
                      var_names = NULL, title = NULL, subtitle = NULL) {
  if (is.null(var_names)) {
    var_names <- paste0("X", seq_along(val))
  }
  if ("factor" %in% class(var_names)) {
    var_names_ordered <- var_names
  } else {
    var_names_ordered <- factor(var_names, levels = var_names[order(val)])
  }
  var_levels <- levels(var_names_ordered)
  var_names2 <- paste0(ifelse(val_lower > 0, "*", ""), as.character(var_names))
  var_labels <- var_names2[match(x = var_levels, table = as.character(var_names))]
  var_names_ordered2 <- factor(var_names_ordered, levels = var_levels, 
                               labels = var_labels)
  dt <- data.frame(val = val, variables = var_names_ordered2)
  if (is.null(val_lower) | is.null(val_upper)) {
    if (is.null(val_sd)) {
      draw_errors <- FALSE
      dt$val_lower <- val
      dt$val_upper <- val
    } else {
      draw_errors <- TRUE
      dt$val_lower <- val - 1.96 * val_sd
      dt$val_upper <- val + 1.96 * val_sd
    }
  } else {
    draw_errors <- TRUE
    dt$val_lower <- val_lower
    dt$val_upper <- val_upper
  }
  if (draw_errors) {
    # dt$val_lower should have been supplied or computed
    # Putting 1 (i.e., +ve sign) as reference level, so that steelblue will be
    # used when all are +ve
    dt$sign_vec <- factor(as.numeric(dt$val_lower > 0), levels = c(1, 0))
  } else {
    # dt$sign_vec <- factor(rep(1, nrow(dt)), levels = c(1, 0))
    dt$sign_vec <- factor(as.numeric(dt$val_lower > 0), levels = c(1, 0))
  }
  common_theme <- theme(panel.grid.major.y = element_line(colour = "grey95"),
                        panel.grid.minor = element_blank(),
                        panel.background = element_blank(),
                        axis.line.x = element_line(colour = "black"),
                        axis.ticks.y = element_blank())
  # x_lab <- "Average model reliance (>0 suggests importance)\nwith 95% predicion interval"
  x_lab <- "Overall variable importance (>0 suggests importance)\nwith 95% predicion interval"
  p <- ggplot(data = dt,
              mapping = aes(x = .data$variables, y = .data$val, 
                            fill = .data$sign_vec)) +
    geom_hline(yintercept = 0, color = "grey") +
    geom_bar(stat = "identity") +
    common_theme +
    coord_flip() +
    labs(x = "", y = x_lab, title = title, subtitle = subtitle) +
    theme(legend.position = "none") +
    scale_fill_manual(values = c("steelblue", "grey"))
  # if (!is.null(labels)) {
  #   dt$labels = labels
  #   p <- p +
  #     geom_text(data = dt,
  #               aes_string(x = "variables", y = 0, label = "labels"),
  #               nudge_x = 0.2, hjust = 0)
  # }
  if (draw_errors) {
    p <- p +
      geom_errorbar(aes(ymin = .data$val_lower, ymax = .data$val_upper),
                    width = 0.2)
  }
  p
}
#' Rank variables based on pairwise comparison of model reliance for one model
#' @param val A numeric vector of model reliance.
#' @param val_sd A numeric vector of standard deviations of model reliance (with
#'   the same length as \code{val}), if available.
#' @param ties.method How to handle tied ranks. Default is \code{"min"}. See
#'   \code{\link{rank}}.
#' @return Returns an integer vector of ranks of variables in descending order.
#' @importFrom stats pnorm
#' @importFrom utils combn
rank_vars <- function(val, val_sd = NULL, ties.method = "min") {
  if (is.null(val_sd)) {
    rank(-val, ties.method = ties.method)
  } else {
    ind_mat <- t(combn(seq_along(val), 2))
    ind_mat_all <- rbind(ind_mat, cbind(ind_mat[, 2], ind_mat[, 1]))
    val_df <- data.frame(
      i1 = ind_mat_all[, 1], i2 = ind_mat_all[, 2],
      val1 = val[ind_mat_all[, 1]], val2 = val[ind_mat_all[, 2]],
      val_sd1 = val_sd[ind_mat_all[, 1]], val_sd2 = val_sd[ind_mat_all[, 2]]
    )
    val_df$val_diff <- val_df$val1 - val_df$val2
    val_df$v1_gt_v2 <- val_df$val_diff > 0
    val_df$sd_diff <- sqrt(val_df$val_sd1 ^ 2 + val_df$val_sd2 ^ 2)
    val_df$pval_v1_gt_v2 <- pnorm(val_df$val_diff / val_df$sd_diff,
                                  lower.tail = FALSE)
    val_df$pval_v1_gt_v2_sig <- val_df$v1_gt_v2 & (val_df$pval_v1_gt_v2 < 0.05)
    val_df$pval_v1_gt_v2_sig <- ifelse(is.na(val_df$pval_v1_gt_v2_sig),
                                       FALSE, val_df$pval_v1_gt_v2_sig)
    count_v1_gt_v2 <- tapply(val_df$pval_v1_gt_v2_sig, val_df$i1, sum)
    rank(-count_v1_gt_v2, ties.method = ties.method)
  }
}
#' Prepare data for making coloured violin plot
#' @param var_names Factor of variable names. Must be a factor.
#' @param val Model reliance.
#' @param perf_metric Model performance metrics, to determine colour.
#' @param perf_metric_breaks Cut-off values to categorise model performance
#'   metrics.
#' @param impute_color When a model reliance interval is not observed with any
#'   models, whether to impute color of this region based on neighboring
#'   regions.
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @importFrom rlang .data
#' @importFrom purrr map_df
prep_data_zebra <- function(var_names, val, perf_metric, perf_metric_breaks,
                            impute_color) {
  my_width <- 0.35
  df <- data.frame(x = var_names, y = val, perf_metric = perf_metric)
  p <- df %>% ggplot(aes_string(x = "x", y = "y")) + geom_violin(trim = TRUE)
  # This is all you need for the fill:
  vl_fill <- data.frame(ggplot_build(p)$data) %>%
    group_by(.data$x) %>%
    mutate(width = .data$violinwidth / max(.data$violinwidth)) %>%
    ungroup() %>%
    mutate(xnew = .data$x - my_width * .data$width,
           xend = .data$x + my_width * .data$width)
  # color by population
  x_lvls <- levels(df$x)
  vl_fill2 <- do.call("rbind", lapply(unique(vl_fill$x), function(xi) {
    dat_i <- df[df$x == x_lvls[xi], ]
    vl_fill_i <- vl_fill[vl_fill$x == xi, ]
    n_i <- nrow(vl_fill_i)
    y_color <- rep(NA, n_i)
    for (i in 1:n_i) {
      rows <- which(dat_i$y < vl_fill_i$ymax[i])
      if (length(rows) > 0) {
        y_color[i] <- mean(dat_i$perf_metric[rows])
        dat_i <- dat_i[-rows, ]
      }
    }
    vl_fill_i$y_color <- y_color
    if (impute_color) {
      vl_fill_i <- fill(data = vl_fill_i, .data$y_color, .direction = "updown")
    }
    vl_fill_i
  })) %>%
    mutate(y_color_cat = cut(.data$y_color, right = FALSE,
                             breaks = unique(c(-Inf, perf_metric_breaks, Inf))))
  # Bit convoluted for the outline, need to be rearranged: the order matters
  vl_poly <- vl_fill2 %>%
    select(.data$xnew, .data$xend, .data$y, .data$group) %>%
    pivot_longer(-c(.data$y, .data$group), names_to = "oldx", values_to = "x") %>%
    arrange(.data$y)
  vl_poly_list <- split(vl_poly, vl_poly$oldx)
  vl_poly_final <- purrr::map_df(vl_poly_list, function(x_df) {
    if (all(x_df$oldx == "xnew")) x_df <- arrange(x_df, desc(x_df$y))
    x_df
  })
  list(df_zebra_color = vl_fill2, df_violin_shape = vl_poly_final)
}
#' Create figure legend for violin plot
#' @param perf_metric_breaks Thresholds for performance metrics.
#' @param col_vec Vector of color to use for each performance interval.
#' @import ggplot2
#' @import ggpubr
#' @importFrom rlang .data
make_violin_legend <- function(perf_metric_breaks, col_vec) {
  # col_vec is for discrete version. it is in reverse direction of continuous version
  col_vec <- rev(col_vec)
  n_brk <- length(perf_metric_breaks)
  brk_diff <- diff(perf_metric_breaks)
  y_color <- c(perf_metric_breaks[1] - brk_diff[1], perf_metric_breaks,
               perf_metric_breaks[n_brk] + brk_diff[n_brk - 1])
  n_mid <- round(median(1:(n_brk + 1)))
  p_legend <- ggplot(data = data.frame(x = seq_along(y_color),
                                       y = seq_along(y_color),
                                       y_color = y_color),
                     aes(x = .data$x, y = .data$y, fill = .data$y_color)) +
    geom_tile() +
    scale_fill_gradient2(low = col_vec[1], high = col_vec[n_brk + 1],
                         mid = col_vec[n_mid], midpoint = median(y_color)) +
    # labels = c("", "Lower", rep("", n_brk - 2), "Higher", "")) +
    theme(legend.position = "bottom") +
    guides(fill = guide_colorbar(title = "Model performance (Lower to higher)",
                                 label.hjust = 0, title.position = "top",
                                 ticks = FALSE, label = FALSE,
                                 barwidth = grid::unit(0.5, "npc"),
                                 barheight = grid::unit(0.5, "lines")))
  ggpubr::get_legend(p_legend)
}
#' Draw coloured violin plot for model reliance.
#' @param var_names Factor or string vector of variable names. Variables will be
#'   plotted in the order specified by \code{levels(var_names)} if a factor is
#'   provided and \code{var_ordering = NULL}. If \code{var_ordering} is
#'   available, ordering of variables will be determined by \code{var_ordering}.
#'   If \code{var_names} is a string vector and \code{var_ordering} is not
#'   available, variables will be plotted in the order of appearance in
#'   \code{var_names} (top-down).
#' @param var_ordering String vector of variable names to indicate their
#'   ordering (ton-down) in the plot (optional).
#' @param val Model reliance for each model.
#' @param perf_metric Model performance metrics (e.g., ratio of model loss to
#'   optimal loss), to determine color.
#' @param smaller_is_better Whether smaller value of model performance metrics
#'   indicates better performance. Default is \code{TRUE} (e.g., when
#'   \code{perf_metric} is loss ratio).
#' @param title Title of violin plot (optional).
#' @param boarder_size Width of the boarder of violins (optional).
#' @param plot_theme \code{ggplot theme} to apply to the violin plot, if any.
#' @param impute_color When a model reliance interval is not observed with any
#'   models, whether to impute colour of this region based on neighbouring
#'   regions. Default is to impute, otherwise the corresponding regions will be
#'   coloured white.
#' @import ggplot2
#' @import RColorBrewer
#' @import ggpubr
#' @importFrom stats median
#' @importFrom rlang .data
draw_violins <- function(var_names, var_ordering = NULL, val, perf_metric,
                         smaller_is_better = TRUE, title = NULL,
                         boarder_size = 0.5, plot_theme = NULL,
                         impute_color = TRUE) {
  if (!is.null(var_ordering)) {
    var_ordering <- as.character(unique(var_ordering))
    if (length(var_ordering) != length(unique(var_names))) {
      stop(simpleError("'var_ordering' should be all unique variable names in 'var_names'."))
    }
    var_names_ordered <- factor(var_names, levels = var_ordering)
  } else {
    if ("factor" %in% class(var_names)) {
      var_names_ordered <- var_names
    } else {
      var_names_ordered <- factor(var_names, levels = rev(unique(var_names)))
    }
  }
  if (!smaller_is_better) perf_metric <- -perf_metric
  # Cut perf_metric into 5 categories to determine color
  perf_metric_breaks <- seq(from = min(perf_metric), to = max(perf_metric),
                            length.out = 6)[2:5]
  col_vec <- RColorBrewer::brewer.pal(n = 6, name = "Blues")[-1]
  col_vec <- rev(col_vec)
  # Create a legend for fill, which looks nicer than default legend for color:
  color_legend <- make_violin_legend(perf_metric_breaks = perf_metric_breaks,
                                     col_vec = col_vec)
  # First prepare the list of two data.frames for the shape and color strips
  # of violins:
  df_violin_list <- prep_data_zebra(var_names = var_names_ordered,
                                    val = val, perf_metric = perf_metric,
                                    perf_metric_breaks = perf_metric_breaks,
                                    impute_color = impute_color)
  x_lvls <- levels(var_names_ordered)
  # The shape of violins are always plotted:
  p <- ggplot() +
    geom_hline(yintercept = 0, color = "grey") +
    geom_polygon(data = df_violin_list$df_violin_shape,
                 aes(x = .data$x, y = .data$y, group = .data$group),
                 color = "grey", size = boarder_size, fill = "white") +
    coord_flip() +
    scale_x_continuous(breaks = seq_along(x_lvls), labels = x_lvls) +
    theme(panel.grid.major.y = element_line(colour = "grey95"),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line.x = element_line(colour = "black"),
          axis.ticks.y = element_blank()) +
    labs(x = "", y = "Variable importance (>0 suggests importance)", title = title)
  # labs(x = "", y = "Model reliance (>0 suggests importance)", title = title)
  # Next, add color to violin plots:
  p <- p +
    geom_segment(data = df_violin_list$df_zebra_color,
                 aes(x = .data$xnew, xend = .data$xend, 
                     y = .data$y, yend = .data$y, color = .data$y_color_cat)) +
    scale_color_manual(values = col_vec, na.value = "white") +
    theme(legend.position = "none") +
    plot_theme
  # Finally, combine plot with legend:
  ggpubr::ggarrange(p, legend.grob = color_legend, legend = "bottom")
}
