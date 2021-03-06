% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/random-forest-model.R
\name{single_partial_dependence_plot}
\alias{single_partial_dependence_plot}
\title{Plot a single partial dependence plot}
\usage{
single_partial_dependence_plot(
  rf_model,
  data,
  variable,
  return_plot = FALSE,
  ylabel = "y"
)
}
\arguments{
\item{rf_model}{random forest model obtained from the function fit_random_forest.}

\item{data}{data used to fit the random forest model}

\item{variable}{variable to use. A character.}

\item{return_plot}{Boolean. If TRUE an object of class ggplot is returned, if FALSE
the data about partial dependence is calculated and returned. FALSE by default.}

\item{ylabel}{label of the y axis. Character. Defaults to "y".}
}
\value{
Data on partial dependence for the selected variable or a ggplot object depending
on the argument return_plot.
}
\description{
Plot a single partial dependence plot
}
