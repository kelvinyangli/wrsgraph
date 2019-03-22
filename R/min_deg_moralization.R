#' Minimum degree moralization
#'
#' This function turns a immoral graph to a moral graph by fill in the deficiency
#' of each node. A node with the minimum degree is selected at each step.
#' The elimination process removes a nodes and the edges between its neighbours.
#' The output of the function is a moral graph.
#' @param graph A graph that is stored in an adjacency matrix.
#' @keywords moralization
#' @export

min_deg_moralization = function(graph) {

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
        graph[y, xNbrs[xNbrs != y]] = 0 # remove nbr edges
      }
      graph = wrsgraph::subgraph(graph, nodes = x, type = "nodes")
    }
  }
  return(H)

}
