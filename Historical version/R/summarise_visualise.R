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
#' @param var_vif If \code{val} is the unadjusted SAGE values, specify a vector
#'   of variance inflation factors (VIF) for each variable to obtain adjusted
#'   ShapleyVIC values. Otherwise leave as \code{NULL}.
#' @param var_vif_threshold Threshold for adjusting SAGE values based on VIF.
#'   Default is 2.
#' @return Returns a \code{data.frame} of variable names, their average
#'   ShapleyVIC values, and the lower and upper bounds of 95\% prediction
#'   intervals (\code{NA} if \code{val_sd = NULL}).
#' @export
summarise_shapley_vic <- function(val, val_sd, var_names,
                                  var_vif = NULL, var_vif_threshold = 2) {
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
  if (!is.null(var_vif)) {
    val <- adjust_val(val = val, var_vif = var_vif,
                      var_vif_threshold = var_vif_threshold)
  }
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
#' @examples
#' data("df_compas", package = "ShapleyVIC")
#' head(df_compas)
#' # The following requires python libraries sage and sklearn, otherwise NULL is
#' # returned. Small training and test sets are used to reduce run time.
#' m_optim <- ShapleyVIC::logit_model_python(x_train = df_compas[1:1000, 2:7],
#'                                           y_train = df_compas$y[1:1000])
#' if (!is.null(m_optim)) {
#'   vals <- ShapleyVIC::compute_sage_value(
#'     model_py = m_optim,
#'     var_names = c("Age", "Race", "Prior criminal history", "Gender",
#'                   "Juvenile criminal history", "Current charge"),
#'     x_test = df_compas[1001:1100, 2:7],
#'     y_test = df_compas$y[1001:1100]
#'   )
#'   ShapleyVIC::draw_bars(val = vals$sage_value, val_sd = vals$sage_sd,
#'                         var_names = vals$var_name)
#' }
#' @export
#' @import ggplot2
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
  dt <- data.frame(val = val, variables = var_names_ordered)
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
  x_lab <- "Average model reliance (>0 suggests importance)\nwith 95% predicion interval"
  p <- ggplot(data = dt,
              mapping = aes_string(x = "variables", y = "val", fill = "sign_vec")) +
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
      geom_errorbar(aes_string(ymin = "val_lower", ymax = "val_upper"),
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
#' Rank variables based on pairwise comparison of model reliance
#' @inheritParams summarise_shapley_vic
#' @param model_id A vector of model ID.
#' @param val A numeric vector of model reliance from each model.
#' @param val_sd A numeric vector of standard deviations of model reliance (with
#'   the same length as \code{val}), if available.
#' @param var_names A factor or string vector of variable names (with the same
#'   length as \code{val}). If unspecified, variables will be named 'X1', 'X2', etc.
#' @param summarise If \code{TRUE}, ranks will be summarised across models by
#'   taking the numeric average. Default is \code{FALSE}, where ranks for
#'   individual models will be returned.
#' @param ties.method How to handle tied ranks. Default is \code{"min"}. See
#'   \code{\link{rank}}.
#' @return Returns an integer vector of ranks of variables for each model (in
#'   descending order). Model reliance (possibly with 95\% confidence interval)
#'   is compared between all possible pairs of variables. Variables with model
#'   reliance significantly than the other variable in more pairwise comparisons
#'   are ranked higher.
#' @import dplyr
#' @import rlang
#' @export
rank_variables <- function(model_id, val, val_sd = NULL, var_names = NULL,
                           var_vif = NULL, var_vif_threshold = 2,
                           summarise = FALSE, ties.method = "min") {
  if (!is.null(var_vif)) {
    val <- adjust_val(val = val, var_vif = var_vif,
                      var_vif_threshold = var_vif_threshold)
  }
  if (is.null(var_names)) {
    var_names <- paste0("X", seq_along(val))
  }
  val_ranks <- do.call("rbind", lapply(sort(unique(model_id)), function(id) {
    data.frame(model_id = id,
               Variable = var_names[model_id == id],
               rank = rank_vars(val = val[model_id == id],
                                val_sd = val_sd[model_id == id],
                                ties.method = ties.method))
  }))
  rownames(val_ranks) <- NULL
  if (summarise) {
    val_ranks %>% group_by(.data$Variable) %>%
      summarise(mean_rank = mean(rank)) %>% as.data.frame()
  } else {
    val_ranks
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
#' @import ggplot2
#' @import ggpubr
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
                     aes_string(x = "x", y = "y", fill = "y_color")) +
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
#' @inheritParams summarise_shapley_vic
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
#'   models, whether to impute color of this region based on neighboring
#'   regions. Default is to impute, otherwise the corresponding regions will be
#'   colored white.
#' @import ggplot2
#' @import RColorBrewer
#' @import ggpubr
#' @importFrom stats median
#' @export
draw_violins <- function(var_names, var_ordering = NULL, val, perf_metric,
                         var_vif = NULL, var_vif_threshold = 2,
                         smaller_is_better = TRUE, title = NULL,
                         boarder_size = 0.5, plot_theme = NULL,
                         impute_color = TRUE) {
  if (!is.null(var_vif)) {
    val <- adjust_val(val = val, var_vif = var_vif,
                      var_vif_threshold = var_vif_threshold)
  }
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
                 aes_string(x = "x", y = "y", group = "group"),
                 color = "grey", size = boarder_size, fill = "white") +
    coord_flip() +
    scale_x_continuous(breaks = seq_along(x_lvls), labels = x_lvls) +
    theme(panel.grid.major.y = element_line(colour = "grey95"),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line.x = element_line(colour = "black"),
          axis.ticks.y = element_blank()) +
    labs(x = "", y = "Model reliance (>0 suggests importance)", title = title)
  # Next, add color to violin plots:
  p <- p +
    geom_segment(data = df_violin_list$df_zebra_color,
                 aes_string(x = "xnew", xend = "xend", y = "y", yend = "y",
                            color = "y_color_cat")) +
    scale_color_manual(values = col_vec, na.value = "white") +
    theme(legend.position = "none") +
    plot_theme
  # Finally, combine plot with legend:
  ggpubr::ggarrange(p, legend.grob = color_legend, legend = "bottom")
}
