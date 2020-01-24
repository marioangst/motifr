
#' Translate multi-level statnet network object to networkx (python) object
#'
#' @param g statnet network object
#' @param typeAttr character vector specifying the attribute name where level information is stored in statnet object
#' @param relabel should nodes be relabeled with statnet vertex.names? (defaults to TRUE)
#'
#' @return python graph object
#' @export
#'
#' @examples
toPyGraph <- function(g, typeAttr, relabel = TRUE) {
  # function for translating a statnet network object into a Python compatible
  # networkx object
  adjacencyMatrix = network::as.matrix.network(g)
  attributeNames  = network::list.vertex.attributes(g)
  attributeValues = lapply(network::list.vertex.attributes(g),
                           function(x) network::get.vertex.attribute(g,x))
  py_g <- sma$translateGraph(adjacencyMatrix, attributeNames, attributeValues, typeAttr)

  if (relabel == TRUE){
    # JS: renaming in here right now, but will suggest update to rbridge.py to do in Python
    node_names <-
      reticulate::py_dict(keys = as.integer(0:(py_g$number_of_nodes()-1)),
              values = network::get.vertex.attribute(g, "vertex.names"))
    py_g <- nx$relabel_nodes(py_g,mapping = node_names)
  }
  py_g
}


# load("data/reussebene_mlnet.RData")
# # get python equivalent to statnet network type
# pyGraph = toPyGraph(ml_net, 'sesType')
#
# nx$nodes(pyGraph)
#
# sma$ThreeEMotifs(G = pyGraph)
#
# # count motifs
#
# # first create set specifying types of motifs (3 or 4 (or any?)), then type eg. II.B
# # then count motifs
# # achtung:
# # procedure empties "set" object after iteration, thus always needs to be rewritten (not very R-like)
#
# set_3_IIC <- sma$motifSet(sma$ThreeEMotifs(pyGraph), sma$is3Type("II.C"))
# sma$countAnyMotifs(set_3_IIC)
