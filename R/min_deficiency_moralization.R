#' A function to moralize a graph according to the minimum deficiency
#'
#' The function moralizes a graph according to the minimum deficiency at each
#' step. Once deficiency of a node is filled in, all edges between the neighbours
#' of the node are removed together with the node itself. This is similar to
#' minimum degree moralization.
#' @param graph A graph that is stored in an adjacency matrix.
#' @param node A node that is expressed in text, e.g. "V1" or "X1".
#' @keywords deficiency
#' @export

min_deficiency_moralization = function(graph) {

  H = graph
  while (length(graph) > 1) {
    graph = prune_leaves(graph)
    nodes = colnames(graph)
    if (length(nodes) > 0) {
      # find the node with the smallest deficiency
      deficiencies = sapply(nodes, wrsgraph::deficiency, graph = graph)
      x = names(which.min(sample(deficiencies))) # randomize nodes
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
