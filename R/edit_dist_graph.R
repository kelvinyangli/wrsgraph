#' The edit distance from the learned to the true undirected graph
#' Both graphs are stored in adjacency matrix. The outcome is a list consists
#' of tp, fp, tn, fn, precision, recall, F1 score and edit distance.
#' @param learned The learned graph stored in an adjacency matrix format.
#' @param true The true graph stored in an adjacency matrix format.
#' @param debug A boolean argument to show steps of the backtracking algorithm.
#' @export

edit_dist_graph = function(learned, true, debug = FALSE) {
  tp = tn = fp = fn = 0
  for (i in 1:(nrow(true) - 1)) {
    for (j in (i + 1):ncol(true)) {
      if ((true[i, j] == 1) && (learned[i, j] == 1)) {
        tp = tp + 1
      } else if ((true[i, j] == 1) && (learned[i, j] == 0)) {
        fn = fn + 1
      } else if ((true[i, j] == 0) && (learned[i, j] == 1)) {
        fp = fp + 1
      } else {
        tn = tn + 1
      }
    }
  }
  pre = round(tp / (tp + fp), 2)
  rec = round(tp / (tp + fn), 2)
  f1 = round(2 * pre * rec / (pre + rec), 2)
  if (debug) {
    cat("tp:", tp, "fp:", fp, "fn:", fn, "tn:", tn, "\n")
    cat("pre:", pre, "rec:", rec, "f1:", f1, "ed:", fp + fn, "\n")
  }
  lst = list(tp = tp, fp = fp, tn = tn, fn = fn, pre = pre, rec = rec,
             f1 = f1, ed = fp + fn)
  return(lst)
}
