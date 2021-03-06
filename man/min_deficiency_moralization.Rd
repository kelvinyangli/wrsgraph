% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/min_deficiency_moralization.R
\name{min_deficiency_moralization}
\alias{min_deficiency_moralization}
\title{A function to moralize a graph according to the minimum deficiency}
\usage{
min_deficiency_moralization(graph)
}
\arguments{
\item{graph}{A graph that is stored in an adjacency matrix.}

\item{node}{A node that is expressed in text, e.g. "V1" or "X1".}
}
\description{
The function moralizes a graph according to the minimum deficiency at each
step. Once deficiency of a node is filled in, all edges between the neighbours
of the node are removed together with the node itself. This is similar to
minimum degree moralization.
}
\keyword{deficiency}
