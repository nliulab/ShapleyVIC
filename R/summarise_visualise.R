#' Perform meta-analysis to summarise ShapleyVIC results
#' @param val A vector of ShapleyVIC values.
#' @param val_sd A vector of standard deviation of ShapleyVIC values. Must be
#'   the same length as \code{val}.
#' @return Returns a named vector of summary statistics from the random effects
#'   model analysis of ShapleyVIC values, including the estimated \code{mean},
#'   the upper and lower of prediction interval (\code{pred_lower} and
#'   \code{pred_upper}), the I2 statistic for heterogeneity (\code{hetero_i2})
#'   and its confidence interval (\code{hetero_i2_lower} and
#'   \code{hetero_i2_upper}).
#' @details Fixed effects model is used when all "studies" are from same sample,
#'   therefore should be estimating exactly the same thing. Random effects model
#'   thinks different "studies" can have different true value. In traditional
#'   meta analysis this difference is due to different sample. Here it is due to
#'   the genuinely different true model reliance in different models.
#' @export
#' @importFrom meta metagen
compute_meta_interval <- function(val, val_sd) {
  val <- as.numeric(as.vector(val))
  val_sd <- as.numeric(as.vector(val_sd))
  if (anyNA(val)) stop(simpleError("NA not allowed in val."))
  if (anyNA(val_sd)) stop(simpleError("NA not allowed in val_sd."))
  if (length(val) != length(val_sd)) {
    stop(simpleError("val and val_sd must have the same length."))
  }
  mt <- meta::metagen(TE = val, seTE = val_sd, studlab = 1:length(val),
                      comb.fixed = FALSE, comb.random = TRUE)
  summ <- summary(mt)
  c(mean = summ$random$TE, pred_lower = summ$predict$lower,
    pred_upper = summ$predict$upper,
    hetero_i2 = summ$I2$TE, hetero_i2_lower = summ$I2$lower,
    hetero_i2_upper = summ$I2$upper)
}
#' Summarise ShapleyVIC values
#' @param val A vector of ShapleyVIC values from all models evaluated.
#' @param val_sd A vector of standard deviation of ShapleyVIC values from all
#'   models evaluated. Must be the same length as \code{val}. If not available,
#'   assign to \code{NULL}.
#' @param var_names A vector of variable names corresponding to each ShapleyVIC
#'   value. Should be the same length as \code{val}. Will be recycled if shorter
#'   than \code{val}.
#' @return Returns a \code{data.frame}.
#' @export
summarise_shapley_vic <- function(val, val_sd, var_names) {
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
      summ_vec <- compute_meta_interval(val = val_i, val_sd = val_sd_i)
      data.frame(Variable = var, val = summ_vec["mean"],
                 val_lower = summ_vec["pred_lower"],
                 val_upper = summ_vec["pred_upper"],
                 hetero_i2 = sprintf("%.1f%% (%.1f%%, %.1f%%)",
                                     summ_vec["hetero_i2"] * 100,
                                     summ_vec["hetero_i2_lower"] * 100,
                                     summ_vec["hetero_i2_upper"] * 100),
                 stringsAsFactors = FALSE)
    }))
    df_summ$sign <- 0
    df_summ$sign[df_summ$val_lower > 0] <- 1
    df_summ$sign[df_summ$val_upper < 0] <- -1
  } else {
    df_summ <- do.call("rbind", lapply(unique(var_names), function(var) {
      val_i <- val[var_names == var]
      data.frame(Variable = var, val = mean(val_i),
                 val_lower = NA, # mean(val_i) - 1.96 * sd(val_i),
                 val_upper = NA, # mean(val_i) + 1.96 * sd(val_i),
                 hetero_i2 = "",
                 stringsAsFactors = FALSE)
    }))
    df_summ$sign <- 0
    df_summ$sign[df_summ$val > 0] <- 1
    df_summ$sign[df_summ$val < 0] <- -1
  }
  df_summ$Variable_ordered <- factor(
    df_summ$Variable,
    levels = df_summ$Variable[order(df_summ$val, df_summ$sign)]
  )
  rownames(df_summ) <- NULL
  df_summ
}
#' Set color template for bar plots
#' @param sign_vec A vector of -1, 0, 1.
#' @param color_template A vector of 3 colours corresponding to -1, 0 and 1.
set_color_template <- function(sign_vec, color_template = NULL) {
  if (is.null(color_template)) {
    color_template <- c("darkorange", "grey", "steelblue")
  }
  color_template[match(sort(unique(sign_vec)), c(-1, 0, 1))]
}
#' Draw bar plot for model reliance.
#' @export
#' @import ggplot2
draw_bars <- function(val, val_sd = NULL, val_lower = NULL, val_upper = NULL,
                      var_names = NULL, labels = NULL, title = "", subtitle = "",
                      sign_vec = NULL, color_template = NULL) {
  if (is.null(var_names)) {
    var_names <- rep("", length(val))
  }
  dt <- data.frame(val = val, variables = var_names)
  if (is.null(val_lower) | is.null(val_upper)) {
    if (is.null(val_sd)) {
      draw_errors <- FALSE
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
  if (!is.null(sign_vec)) {
    dt$sign_vec_factor <- factor(sign_vec)
    color_vec <- set_color_template(sign_vec = sign_vec,
                                    color_template = color_template)
    p <- ggplot(data = dt,
                mapping = aes_string(x = "variables", y = "val",
                                     fill = "sign_vec_factor")) +
      geom_bar(stat = "identity") +
      theme_bw() +
      coord_flip() +
      labs(x = "", y = "", title = title, subtitle = subtitle) +
      theme(legend.position = "none") +
      scale_fill_manual(values = color_vec)
  } else {
    p <- ggplot(data = dt,
                mapping = aes_string(x = "variables", y = "val")) +
      geom_bar(stat = "identity") +
      theme_bw() +
      coord_flip() +
      labs(x = "", y = "", title = title, subtitle = subtitle) +
      theme(legend.position = "none")
  }
  if (!is.null(labels)) {
    dt$labels = labels
    p <- p +
      geom_text(data = dt,
                aes_string(x = "variables", y = 0, label = "labels"),
                nudge_x = 0.2, hjust = 0)
  }
  if (draw_errors) {
    p <- p +
      geom_errorbar(aes_string(ymin = "val_lower", ymax = "val_upper"),
                    width = 0.2)
  }
  print(p)
}
#' Rank variables based on pairwise comparison of SAGE-based model reliance
#' @param val A numeric vector of SAGE-based model reliance.
#' @param val_sd A numeric vector of standard deviations of SAGE-based model
#'   reliance of the same length as \code{val}. Supply \code{NULL} if
#'   unavailable.
#' @param ties.method How to handle tied ranks. Default is \code{"min"}. See
#'   \code{\link{rank}}.
#' @return Returns an integer vector of ranks of variables in descending order.
#'   SAGE-based model reliance (with 95\% CI) was compared between all possible
#'   pairs of variables. Variables with model reliance significantly than the
#'   other variable in more pairwise comparisons are ranked higher.
#' @export
rank_variables <- function(val, val_sd = NULL, ties.method = "min") {
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
#' Rank variables by the 95\% CI of SAGE-based model reliance
#' @param val A numeric vector of SAGE-based model reliance.
#' @param val_sd A numeric vector of standard deviations of SAGE-based model
#'   reliance of the same length as \code{val}. Supply \code{NULL} if
#'   unavailable.
#' @param ties.method How to handle tied ranks. Default is \code{"min"}. See
#'   \code{\link{rank}}.
#' @return Returns an integer vector of ranks of variables in descending order,
#'   based on pairwise t-test for equal model reliance for neighbouring variables.
#' @export
rank_variables_old <- function(val, val_sd = NULL, ties.method = "min") {
  if (is.null(val_sd)) {
    rank(-val, ties.method = ties.method)
  } else {
    val_df <- data.frame(var_id = 1:length(val), val = val, val_sd = val_sd)
    val_df <- val_df[order(val_df$val, decreasing = TRUE), ]
    val_df$order <- 1:nrow(val_df)
    val_df$pairwise_pval <- c(1, unlist(lapply(2:nrow(val_df), function(i) {
      val_diff <- val_df$val[i - 1] - val_df$val[i]
      sd_diff <- sqrt(val_df$val_sd[i - 1] ^ 2 + val_df$val_sd[i] ^ 2)
      2 * pnorm(abs(val_diff / sd_diff), lower.tail = FALSE)
    })))
    val_df$pairwise_sig <- as.numeric(val_df$pairwise_pval < 0.05)
    val_df$order_sig <- rank(1 + cumsum(val_df$pairwise_sig),
                             ties.method = ties.method)
    val_df <- val_df[order(val_df$var_id), ]
    val_df$order_sig
  }
}
