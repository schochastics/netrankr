% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/majorization.gap.R
\name{majorization_gap}
\alias{majorization_gap}
\title{Majorization gap}
\usage{
majorization_gap(g, norm = TRUE)
}
\arguments{
\item{g}{An igraph object}

\item{norm}{\code{True} (Default) if the normalized majorization gap should be returned.}
}
\value{
Majorization gap of an undirected graph.
}
\description{
Calculates the (normalized) majorization gap of an undirected graph.
The majorization gap indicates how far the degree sequence of a graph is
from a degree sequence of a \link{threshold_graph}.
}
\details{
The distance is measured by the number of \emph{reverse unit
transformations} necessary to turn the degree sequence into a threshold sequence.
First, the \emph{corrected conjugated degree sequence} d' is calculated from the degree sequence d as follows:
\deqn{d'_k= |\{ i : i<k \land d_i\geq k-1 \} | +
| \{ i : i>k \land d_i\geq k \} |.}
the majorization gap is then defined as
\deqn{1/2 \sum_{k=1}^n \max\{d'_k - d_k,0\}}
The higher the value, the further away is a graph to be a threshold graph.
}
\examples{
library(igraph)
g <- graph.star(5, "undirected")
majorization_gap(g) # 0 since star graphs are threshold graphs

g <- sample_gnp(100, 0.15)
majorization_gap(g, norm = TRUE) # fraction of reverse unit transformation
majorization_gap(g, norm = FALSE) # number of reverse unit transformation
}
\references{
Schoch, D., Valente, T. W. and Brandes, U., 2017. Correlations among centrality
indices and a class of uniquely ranked graphs. \emph{Social Networks} \strong{50}, 46–54.

Arikati, S.R. and Peled, U.N., 1994. Degree sequences and majorization.
\emph{Linear Algebra and its Applications}, \strong{199}, 179-211.
}
\author{
David Schoch
}
