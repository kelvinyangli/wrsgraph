#' Minimum degree triangulation
#'
#' This function turns a graph to a triangulated graph by fill in the deficiency
#' of each node. A node with the minimum degree is selected at each step.
#' The elimination process removes a simplicial node at a time.
#' The output of the function is a triangulated graph.
#' @param graph A graph that is stored in an adjacency matrix.
#' @keywords triangulation
#' @export

min_deg_triangulation = function(graph) {

  H = graph
  while (length(graph) > 1) {
    graph = prune_leaves(graph)
    nodes = colnames(graph)
    if (length(nodes) > 0) {
      # find the node with the smallest degree
      degrees = sapply(nodes, wrsgraph::degree, graph = graph)
      x = nodes[which.min(degrees)]
      xNbrs = find_nbr(graph, x)
      for (y in xNbrs) {
        H[y, xNbrs[xNbrs != y]] = 1 # add deficiency
        graph[y, xNbrs[xNbrs != y]] = 1 # add deficiency
      }
      graph = wrsgraph::subgraph(graph, nodes = x, type = "nodes")
    }
  }
  return(H)

}
