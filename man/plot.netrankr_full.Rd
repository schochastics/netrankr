% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{plot.netrankr_full}
\alias{plot.netrankr_full}
\title{Plot netrankr_full object}
\usage{
\method{plot}{netrankr_full}(x, icols = NULL, bcol = "grey66", ecol = "black", ...)
}
\arguments{
\item{x}{A netrankr_full object}

\item{icols}{a list of colors (an internal palette is used if missing)}

\item{bcol}{color used for the barcharts}

\item{ecol}{color used for errorbars}

\item{...}{additional plot parameters}
}
\description{
Plots the result of an object obtained from \link{exact_rank_prob}
}
\author{
David Schoch
}
