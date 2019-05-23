#' Fixed ordering moralization
#'
#' This function turns a immoral graph to a moral graph by fill in the deficiency
#' of each node for a given node ordering. The elimination process removes a
#' nodes and the edges between its neighbours.
#' The output of the function is a moral graph.
#' @param graph A graph that is stored in an adjacency matrix.
#' @keywords moralization
#' @export

fixed_ordering_moralization = function(graph, ordering) {

  H = graph
  nodes = colnames(graph)
  while (length(graph) > 1) {
    if (length(nodes) > 0) {
      x = ordering[1]
      xNbrs = find_nbr(graph, x)
      for (y in xNbrs) {
        H[y, xNbrs[xNbrs != y]] = 1 # add deficiency
        graph[y, xNbrs[xNbrs != y]] = 0 # remove nbr edges
      }
      graph = wrsgraph::subgraph(graph, nodes = x, type = "nodes")
      ordering = ordering[-1]
      nodes = nodes[nodes != x]
    }
  }

  return(H)

}
