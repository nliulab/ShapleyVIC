% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarise_visualise.R
\name{draw_violins}
\alias{draw_violins}
\title{Draw coloured violin plot for model reliance.}
\usage{
draw_violins(
  var_names,
  var_ordering = NULL,
  val,
  perf_metric,
  smaller_is_better = TRUE,
  title = NULL,
  boarder_size = 0.5,
  plot_theme = NULL,
  impute_color = TRUE
)
}
\arguments{
\item{var_names}{Factor or string vector of variable names. Variables will be
plotted in the order specified by \code{levels(var_names)} if a factor is
provided and \code{var_ordering = NULL}. If \code{var_ordering} is
available, ordering of variables will be determined by \code{var_ordering}.
If \code{var_names} is a string vector and \code{var_ordering} is not
available, variables will be plotted in the order of appearance in
\code{var_names} (top-down).}

\item{var_ordering}{String vector of variable names to indicate their
ordering (ton-down) in the plot (optional).}

\item{val}{Model reliance for each model.}

\item{perf_metric}{Model performance metrics (e.g., ratio of model loss to
optimal loss), to determine color.}

\item{smaller_is_better}{Whether smaller value of model performance metrics
indicates better performance. Default is \code{TRUE} (e.g., when
\code{perf_metric} is loss ratio).}

\item{title}{Title of violin plot (optional).}

\item{boarder_size}{Width of the boarder of violins (optional).}

\item{plot_theme}{\code{ggplot theme} to apply to the violin plot, if any.}

\item{impute_color}{When a model reliance interval is not observed with any
models, whether to impute colour of this region based on neighbouring
regions. Default is to impute, otherwise the corresponding regions will be
coloured white.}
}
\description{
Draw coloured violin plot for model reliance.
}
