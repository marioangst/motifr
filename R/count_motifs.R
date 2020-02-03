
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

  #github.com/rstudio/reticulate/issues/233 (delay loading till use)
  if(!(exists("sma") & typeof(sma) == "environment")){
    load_python_sma()
  }

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

#' Display all two-level motifs consisting of three nodes
#'
#' @return Opens image
#' @export
#'
#' @examples
show_3_motifs <- function(){
  magick::image_read(path = system.file("motif_reference",
                                        "motif_reference_3motifs.png",
                                        package = utils::packageName()))
}

#' Display all two-level motifs consisting of four nodes
#'
#' @return Opens figure from Ö. Bodin, M. Tengö: Disentangling intangible social–ecological systems in Global Environmental Change 22 (2012) 430–439 http://dx.doi.org/10.1016/j.gloenvcha.2012.01.005

#' @export
#'
#' @examples
show_4_motifs <- function(){
  magick::image_read(path = system.file("motif_reference",
                                        "motif_reference_4motifs.jpg",
                                        package = utils::packageName()))
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

#' Count multi-level motifs
#'
#' @param net A statnet network object with a node attribute specifying the level of each node
#' @param type_attr character vector specifying the attribute name where level information is stored in statnet object
#' @param number_nodes The number of nodes in the motif to be counted (3 or 4 are possible)
#' @param motif The motif name. Use show_3_motifs() or show_4_motifs() to see possible motifs
#'
#' @return Integer giving the motif count
#' @export
#'
#' @examples
count_motifs <- function(net, type_attr, number_nodes, motif){

  if(!(number_nodes %in% c(3,4))){
    stop(paste("At the moment, only motifs with 3 or 4 nodes can
         be classified. You entered",number_nodes,".
               Use show_3_motifs() or show_4_motifs() to see possible motifs"))
  }

  all_possibilities_4 <- expand.grid(
    c("I","II","III","IV","V","VI","VII"),
    c("A","B","C","D"))
  all_possibilities_4 <- paste(all_possibilities_4$Var1,all_possibilities_4$Var2,sep = ".")

  all_possibilities_3 <- expand.grid(
    c("I","II"),
    c("A","B","C","D"))
  all_possibilities_3 <- paste(all_possibilities_3$Var1,all_possibilities_3$Var2,sep = ".")

  if(number_nodes == 3 & !(motif %in% all_possibilities_3)){
    stop(paste("This motif (",
               motif,
                ") cannot be counted. For three nodes, only these are possible: ",
               paste(all_possibilities_3,collapse = ", ")))
  }

  if(number_nodes == 4 & !(motif %in% all_possibilities_4)){
    stop(paste("This motif (",
               motif,
                ") cannot be counted. For four nodes, only these are possible: ",
               paste(all_possibilities_4,collapse = ", ")))
  }

  py_g <- integrateR::toPyGraph(net,typeAttr = type_attr)
  if(number_nodes == 3){
    motif_set <- sma$motifSet(sma$ThreeEMotifs(py_g), sma$is3Type(motif))
    sma$countAnyMotifs(motif_set)
  }
  if(number_nodes == 4){
    motif_set <- sma$motifSet(sma$FourMotifs(py_g), sma$is4Type(motif))
    sma$countAnyMotifs(motif_set)
  }
}

