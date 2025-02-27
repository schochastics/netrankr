% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/netswan.R
\name{swan_efficiency}
\alias{swan_efficiency}
\title{Impact on farness when a node is removed}
\usage{
swan_efficiency(g)
}
\arguments{
\item{g}{An \code{igraph} object representing the graph to analyze.
\code{swan_efficiency} is based on geographic accessibility, similar to indices used for
assessing transportation network performance, such as closeness accessibility.
It quantifies the impact of node removal by calculating the change in the sum of
distances between all node pairs.

The code is an adaptation from the NetSwan package that was archived on CRAN.}
}
\value{
A numeric vector where each entry represents the \code{swan_efficiency} value for the
corresponding node.
}
\description{
\code{swan_efficiency} measures the change in the sum of distances between all node pairs
when excluding a node from the network.
}
\examples{
library(igraph)
# Example graph (electrical network structure)
elec <- matrix(ncol = 2, byrow = TRUE, c(
  11,1, 11,10, 1,2, 2,3, 2,9,
  3,4, 3,8, 4,5, 5,6, 5,7,
  6,7, 7,8, 8,9, 9,10
))
gra <- graph_from_edgelist(elec, directed = FALSE)

# Compute efficiency impact of node removal
f2 <- swan_efficiency(gra)
bet <- betweenness(gra)
reg <- lm(bet ~ f2)
summary(reg)
}
\references{
Lhomme S. (2015). \emph{Analyse spatiale de la structure des réseaux techniques dans un
contexte de risques}. Cybergeo: European Journal of Geography.
}
