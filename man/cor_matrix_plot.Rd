% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cor_matrix_plot.R
\name{cor_matrix_plot}
\alias{cor_matrix_plot}
\title{cor.matrix.plot}
\usage{
cor_matrix_plot(data, conf.level = 0.95)
}
\arguments{
\item{data}{Data frame to plot. Use dplyer::select to filter variables to use.}

\item{conf.level}{Level of significance (default .95)}
}
\value{
Returns nothing. Is called for sideeffects (i.e. the plot)
}
\description{
Plots a correlation matrix plot. The to
}
\examples{
library(magrittr)
library(dplyr)
mtcars \%>\% select(disp, qsec) \%>\% cor_matrix_plot()
}
