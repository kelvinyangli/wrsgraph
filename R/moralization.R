#' Graph moralization
#'
#' This function turns an immoral graph to a moral graph by following the node
#' ordering given. It either removes no or all edges between the neighbours of
#' the simplicial node at each step of moraliziing the graph.
#' @param graph A graph that is stored in an adjacency matrix.
#' @param ordering A node ordering.
#' @param excess The edges (between the neighbours of the simplicial node) that
#' are removed with the simplicial node. At the moment, this is a binary input
#' that takes 0 or 1. The value 0 corresponds to no edge removal. The resulting
#' graph is triangulated. The value 1 corresponds to remove all edges between
#' the neighbours of each simplicial node.
#' @keywords moralization
#' @export

moralization = function(graph, ordering, excess = c(0, 1)) {

  # check argument
  if (!excess %in% c(0, 1)) stop("excess must be 0 or 1")
  H = graph
  nodes = colnames(graph)
  while (length(graph) > 1) {
    if (length(nodes) > 0) {
      x = ordering[1]
      xNbrs = find_nbr(graph, x)
      for (y in xNbrs) {
        H[y, xNbrs[xNbrs != y]] = 1 # add deficiency
        if (excess == 1) {
          graph[y, xNbrs[xNbrs != y]] = 0 # remove nbr edges
        }
      }
      graph = wrsgraph::subgraph(graph, nodes = x, type = "nodes")
      ordering = ordering[-1]
      nodes = nodes[nodes != x]
    }
  }

  return(H)

}
