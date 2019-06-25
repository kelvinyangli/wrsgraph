#' Convert a list of MBs to an undirected graph
#' The list of MBs must be symmetric in order for the converted graph to be
#' symmetric.
#' @param mbList A list of MBs.
#' @param vars A vector of all the variables. Must be in the same order as the
#' list name.
#' @export
mblist2moral = function(mbList, vars) {

  nvars = length(vars)
  m = matrix(0, nvars, nvars)
  colnames(m) = rownames(m) = vars
  for (x in vars) {
    m[x, mbList[[x]]] = 1
  }

  return(m)

}
