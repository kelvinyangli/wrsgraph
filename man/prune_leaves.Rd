% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prune_leaves.R
\name{prune_leaves}
\alias{prune_leaves}
\title{A function that prunes leaves}
\usage{
prune_leaves(graph)
}
\arguments{
\item{graph}{A graph that is stored in an adjacency matrix.}
}
\description{
This function recursively removes leaves from a graph. A leave is a node with degree 1. The process
stops if there is no leaves or the graph is a single node or empty graph.
}
