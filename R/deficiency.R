#' A function to calculates the deficiency of a node
#'
#' The deficiency of a node in a graph is the number of edges, adding which make
#' the node's neighbours a complete subgraph (or a clique).
#' @param graph A graph that is stored in an adjacency matrix.
#' @param node A node that is expressed in text, e.g. "V1" or "X1".
#' @keywords deficiency
#' @export

deficiency = function(graph, x) {

  def = 0
  nbrs = names(which(graph[x, ] == 1))
  emptyDeficiency = wrsgraph::is_clique(graph, nbrs)
  if (emptyDeficiency == 0) { # if the deficiency is not empty

    nNbrs = length(nbrs)
    maxNEdges = nNbrs * (nNbrs - 1) / 2
    def = maxNEdges - sum(graph[nbrs, nbrs]) / 2

  }

  return(def)

}
