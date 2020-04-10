
#' Translate multi-level statnet network object to networkx (python) object
#'
#' @param g statnet network object
#' @param typeAttr character vector specifying the attribute name where level information
#' is stored in statnet object. The attribute should be a binary vector. 1 indicates a "social" node
#' and 0 indicates a "non-social" node.
#' @param relabel should nodes be relabeled with statnet vertex.names? (defaults to TRUE)
#'
#' @return python graph object
#' @export
#'
#' @examples
toPyGraph <- function(g, typeAttr, relabel = TRUE) {

  if(!(identical(unique(network::get.vertex.attribute(g, typeAttr)),c(1,0)))){
    stop("Please specify the typeAttr attribute of the network object as a binary vector.
         1s indicate social nodes, 0s non-social nodes")
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
#' @param motifs a list of motif identifiers which shall be counted, e.g. list("1,2[I.C]")
#' @param assume_sparse whether the network shall be assumed to be sparse (for optimization)
#'
#' @return list indexed by motif identifers giving the motif counts
#' @export
#'
#' @examples count_motifs(ml_net, type_attr = c("sesType"), motifs=list("1,2[I.C]", "1,2[II.C]", "2,1[I.C]", "2,1[II.C]"))
count_motifs <- function(net,
                         motifs,
                         type_attr = c("sesType"),
                         assume_sparse = TRUE,
                         omit_total_result = FALSE){
  # convert net to python object
  py_g <- integrateR::toPyGraph(net,typeAttr = type_attr)

  # call counter
  counted <- sma$countMotifsAutoR(py_g,
                                  motifs,
                                  assume_sparse = assume_sparse,
                                  omit_total_result = omit_total_result)
  if(!omit_total_result){
    # process result
    partial_count <- counted[[1]]
    total_count <- counted[[2]]

    partial_count$`total` <- total_count

    return(partial_count)
  }else{
    return(counted)
  }
}

#' Compute statistical properties of the distribution of motifs in a random
#' baseline
#'
#' @param g statnet network object
#' @param typeAttr character vector specifying the attribute name where level
#'   information is stored in statnet object. The attribute should be a binary
#'   vector. 1 indicates a "social" node and 0 indicates a "non-social" node.
#' @param motifs list of motif identifiers describing the motifs whose
#'   distribution shall be analysed
#' @param model model to be used, either erdos_renyi or actors_choice, see sma documentation
#' @param level additional parameter for actors_choice
#'
#' @return list indexed by motif identifers giving the tuple of expectation and
#'   variance of this motif in the random baseline
#' @export
#'
#' @examples
motifs_distribution <- function(net,
                                motifs,
                                type_attr = c("sesType"),
                                model = c("erdos_renyi"),
                                level = -1) {
  # convert net to python object
  py_g <- integrateR::toPyGraph(net,typeAttr = type_attr)

  # call counter
  result <- sma$distributionMotifsAutoR(py_g, motifs, model = model, level = level)

  # process result
  partial <- result[[1]]
  total <- result[[2]]

  partial$`total` <- total

  return(partial)
}

#' Summary for motif counts and distribution (Erdos-Renyi)
#'
#' @param g statnet network object
#' @param typeAttr character vector specifying the attribute name where level
#'   information is stored in statnet object. The attribute should be a binary
#'   vector. 1 indicates a "social" node and 0 indicates a "non-social" node.
#'
#' @return dataframe with motif counts, expectations and variances for set of
#'   selected motifs
#' @export
#'
#' @examples motif_summary(ml_net)
motif_summary <- function(net,
                          type_attr = c("sesType")) {

  # exquisite selection of motifs
  motifs <- c("1,2[I.C]", "1,2[II.C]", "2,1[I.C]", "2,1[II.C]", "2,2[III.C]", "2,2[III.D]")

  # count and compute distribution parameters
  counts <- integrateR::count_motifs(net, type_attr, motifs = motifs)
  distribution <- integrateR::motifs_distribution(net, type_attr, motifs = motifs)

  # reformat data
  result <- data.frame("count" = sapply(counts[motifs], function(x){x[1]}),
                       "expectation" =sapply(distribution[motifs], function(x){x[[1]][1]}),
                       "variance" = sapply(distribution[motifs], function(x){
                         val = x[[2]][[1]]
                         if(is.null(val)){
                           return(NA)
                         }else{
                           return(val)
                         }
                       }),
                       row.names = motifs)
  return(result)
}

#' Returns an example for a motif specified by a motif identifier string
#'
#' @param g statnet network object
#' @param motif motif identifier string for the motif
#' @param typeAttr character vector specifying the attribute name where level
#'   information is stored in statnet object. The attribute should be a binary
#'   vector. 1 indicates a "social" node and 0 indicates a "non-social" node.
#'
#' @return list of nodes in the motif
#' @export
#'
#' @examples exemplify_motif(ml_net, motif = '1,2[I.C]')
exemplify_motif <- function(net,
                            motif,
                            type_attr = c("sesType")) {
  # convert net to python object
  py_g <- integrateR::toPyGraph(net,typeAttr = type_attr)

  return(sma$exemplifyMotif(py_g, motif))
}

#' Plots an example for a motif with given motif identifier string taken from
#' the given graph.
#'
#' @param g statnet network object
#' @param motif motif identifier string for the motif
#' @param typeAttr character vector specifying the attribute name where level
#'   information is stored in statnet object. The attribute should be a binary
#'   vector. 1 indicates a "social" node and 0 indicates a "non-social" node.
#'
#' @return plot
#' @export
#'
#' @examples show_motif(ml_net, motif = '1,2[I.C]')
show_motif <- function(net,
                       motif,
                       type_attr = c("sesType")) {
  motif <- integrateR::exemplify_motif(net, motif, type_attr = type_attr)
  vertices <- get.vertex.attribute(net, "vertex.names")
  indices <- sapply(motif, function(x){match(x, vertices)})
  subgraph <- get.inducedSubgraph(net, indices)
  return(plot.network(subgraph))
}

#' Simulates base line.
#'
#' @param g statnet network object
#' @param motifs list of motif identifier strings
#' @param n number of random graphs
#' @param typeAttr character vector specifying the attribute name where level
#'   information is stored in statnet object. The attribute should be a binary
#'   vector. 1 indicates a "social" node and 0 indicates a "non-social" node.
#' @param assume_sparse whether the random graphs shall be assumed to be sparse.
#'   used to find ideal counting function
#'
#' @return data frame with one column for each motif identifier string and one
#'   row for every computed random graph
#' @export
#'
#' @examples simulate_baseline(ml_net, list('1,2[I.C]'), n = 10)
simulate_baseline <- function(net,
                              motifs,
                              n = 100,
                              type_attr = c("sesType"),
                              assume_sparse = TRUE) {
  py_g <- integrateR::toPyGraph(net, typeAttr = type_attr)

  result <- sma$simulateBaselineAutoR(py_g, motifs, n = n, assume_sparse = assume_sparse)
  df <- data.frame(result, check.names = FALSE)
  return(df)
}

#' Simulates a number of random graphs and compares the numbers of motifs in the
#' given graph with this random baseline.
#'
#' @param g statnet network object
#' @param motifs list of motif identifier strings
#' @param n number of random graphs
#' @param typeAttr character vector specifying the attribute name where level
#'   information is stored in statnet object. The attribute should be a binary
#'   vector. 1 indicates a "social" node and 0 indicates a "non-social" node.
#' @param assume_sparse whether the random graphs shall be assumed to be sparse.
#'   used to find ideal counting function
#'
#' @return data frame with one column for each motif identifier string and one
#'   row for every computed random graph
#' @export
#'
#' @examples
compare_to_baseline <- function(net,
                                motifs,
                                n = 100,
                                type_attr = c("sesType"),
                                assume_sparse = TRUE) {
  simulation <- integrateR::simulate_baseline(net,
                                              motifs,
                                              n = n,
                                              type_attr = type_attr,
                                              assume_sparse = assume_sparse)
  count <- integrateR::count_motifs(net,
                                    motifs,
                                    type_attr = type_attr,
                                    assume_sparse = assume_sparse,
                                    omit_total_result = TRUE)

  # put nice plots together
}
