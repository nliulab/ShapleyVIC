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
#' Summarize ShapleyVIC values
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
  common_theme <- theme(panel.grid.major.y = element_line(colour = "grey95"),
                        panel.grid.minor = element_blank(),
                        panel.background = element_blank(),
                        axis.line.x = element_line(colour = "black"),
                        axis.ticks.y = element_blank())
  x_lab <- "Average model reliance (>0 suggests importance)\nwith 95% predicion interval"
  if (!is.null(sign_vec)) {
    dt$sign_vec_factor <- factor(sign_vec)
    color_vec <- set_color_template(sign_vec = sign_vec,
                                    color_template = color_template)
    p <- ggplot(data = dt,
                mapping = aes_string(x = "variables", y = "val",
                                     fill = "sign_vec_factor")) +
      geom_hline(yintercept = 0, color = "grey") +
      geom_bar(stat = "identity") +
      common_theme +
      coord_flip() +
      labs(x = "", y = x_lab, title = title, subtitle = subtitle) +
      theme(legend.position = "none") +
      scale_fill_manual(values = color_vec)
  } else {
    p <- ggplot(data = dt,
                mapping = aes_string(x = "variables", y = "val")) +
      geom_hline(yintercept = 0, color = "grey") +
      geom_bar(stat = "identity") +
      common_theme +
      coord_flip() +
      labs(x = "", y = x_lab, title = title, subtitle = subtitle) +
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
  p
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
#' Prepare data for making colored violin plot
#' @param var_names Factor of variable names. Must be a factor.
#' @param val Model reliance.
#' @param loss_ratio Ratio of loss to optimal loss, to determine color.
#' @import tidyverse
#' @importFrom rlang .data
#' @importFrom purrr map_df
prep_data_zebra <- function(var_names, val, perf_metric,
                            perf_metric_breaks = c(1.01, 1.02, 1.03, 1.04),
                            my_width = 0.35) {
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
    vl_fill_i <- fill(data = vl_fill_i, .data$y_color, .direction = "updown")
    vl_fill_i
  })) %>%
    mutate(y_color_cat = cut(.data$y_color, right = FALSE,
                             breaks = unique(c(-Inf, perf_metric_breaks, Inf))))
  # Bit convoluted for the outline, need to be rearranged: the order matters
  vl_poly <- vl_fill2 %>%
    select(.data$xnew, .data$xend, .data$y, .data$group) %>%
    pivot_longer(-c(.data$y, .data$group), names_to = "oldx", values_to = "x") %>%
    arrange(.data$y) %>%
    split(., .$oldx) %>%
    purrr::map_df(., function(x) {
      if (all(x$oldx == "xnew")) x <- arrange(x, desc(y))
      x
    })
  list(df_zebra_color = vl_fill2, df_violin_shape = vl_poly)
}
#' @import ggplot2
#' @import ggpubr
make_violin_legend <- function(perf_metric_breaks, col_vec) {
  n_brk <- length(perf_metric_breaks)
  brk_diff <- diff(perf_metric_breaks)
  y_color <- c(perf_metric_breaks[1] - brk_diff[1], perf_metric_breaks,
               perf_metric_breaks[n_brk] + brk_diff[n_brk - 1])
  n_mid <- round(median(1:(n_brk + 1)))
  p_legend <- ggplot(data = data.frame(x = seq_along(y_color),
                                       y = seq_along(y_color),
                                       y_color = y_color),
                     aes_string(x = "x", y = "y", fill = "y_color")) +
    geom_tile() +
    # col_vec is for discrete version. it is in reverse direction of continuous version
    scale_fill_gradient2(low = col_vec[n_brk + 1], high = col_vec[1],
                         mid = col_vec[n_mid], midpoint = median(y_color),
                         labels = c("Lower", rep("", n_brk), "Higher")) +
    theme(legend.position = "bottom") +
    guides(fill = guide_colorbar(title = "Model performance", title.vjust = 1,
                                 ticks = FALSE, barwidth = grid::unit(0.5, "npc"),
                                 barheight = grid::unit(0.5, "lines")))
  ggpubr::get_legend(p_legend)
}
#' Draw colored violin plot for model reliance.
#' @param var_names Factor of variable names. Must be a factor.
#' @param val Model reliance.
#' @param loss_ratio Ratio of loss to optimal loss, to determine color.
#' @param title Title of violin plot. Default is an empty string.
#' @param colored Whether the violins should be colored by model loss.
#' @import ggplot2
#' @import RColorBrewer
#' @import ggpubr
#' @export
draw_violins <- function(var_names, val, perf_metric, smaller_is_better = TRUE,
                         title = "", colored = TRUE,
                         my_width = 0.35,
                         perf_metric_breaks = c(1.01, 1.02, 1.03, 1.04)) {
  # First prepare the list of two data.frames for the shape and color strips
  # of violins:
  df_violin_list <- prep_data_zebra(var_names = var_names,
                                    val = val, perf_metric = perf_metric)
  x_lvls <- levels(var_names)
  # The shape of violins are always plotted:
  p <- ggplot() +
    geom_hline(yintercept = 0, color = "grey") +
    geom_polygon(data = df_violin_list$df_violin_shape,
                 aes_string(x = "x", y = "y", group = "group"),
                 color = "grey", size = 1, fill = "white") +
    coord_flip() +
    scale_x_continuous(breaks = seq_along(x_lvls), labels = x_lvls) +
    theme(panel.grid.major.y = element_line(colour = "grey95"),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line.x = element_line(colour = "black"),
          axis.ticks.y = element_blank()) +
    labs(x = "", y = "Model reliance (>0 suggests importance)", title = title)
  # If colored, also add color strips:
  if (colored) {
    n_cats <- length(perf_metric_breaks) + 1
    col_vec <- RColorBrewer::brewer.pal(n = n_cats + 1, name = "Blues")[-1]
    if (all(perf_metric_breaks >= 1)) col_vec <- rev(col_vec)
    # First create a legend for fill, which looks nicer than default legend for
    # color:
    color_legend <- make_violin_legend(perf_metric_breaks = perf_metric_breaks,
                                       col_vec = col_vec)
    # Then make the colored violin plots:
    p <- p +
      geom_segment(data = df_violin_list$df_zebra_color,
                   aes_string(x = "xnew", xend = "xend", y = "y", yend = "y",
                              color = "y_color_cat")) +
      scale_color_manual(values = col_vec) +
      theme(legend.position = "none")
    # Finally, combine plot with legend:
    p <- ggpubr::ggarrange(p, legend.grob = color_legend, legend = "bottom")
  }
  p
}
