
#' Translate multi-level statnet network object to networkx (python) object
#'
#' @param g statnet network object
#' @param type_attr character vector specifying the attribute name where level information
#' is stored in statnet object. The attribute should be a binary vector. 1 indicates a "social" node
#' and 0 indicates a "non-social" node.
#' @param relabel should nodes be relabeled with statnet vertex.names? (defaults to TRUE)
#'
#' @return python graph object
#' @export
#'
#' @examples
toPyGraph <- function(g, type_attr, relabel = TRUE) {

  # Support multi-level graphs:
  # if(!(identical(unique(network::get.vertex.attribute(g, type_attr)),c(1,0)))){
  #   stop("Please specify the type_attr attribute of the network object as a binary vector.
  #        1s indicate social nodes, 0s non-social nodes")
  # }

  # function for translating a statnet network object into a Python compatible
  # networkx object
  adjacencyMatrix = network::as.matrix.network(g)
  attributeNames  = network::list.vertex.attributes(g)
  attributeValues = lapply(network::list.vertex.attributes(g),
                           function(x) network::get.vertex.attribute(g,x))
  py_g <- sma$translateGraph(adjacencyMatrix, attributeNames, attributeValues, type_attr)

  if (relabel == TRUE){
    # JS: renaming in here right now, but will suggest update to rbridge.py to do in Python
    node_names <-
      reticulate::py_dict(keys = as.integer(0:(py_g$number_of_nodes()-1)),
              values = network::get.vertex.attribute(g, "vertex.names"))
    py_g <- nx$relabel_nodes(py_g,mapping = node_names)
  }
  py_g
}

#' Display all supported motifs with signature ``1,2``.
#'
#' @return Opens image
#' @export
#'
#' @examples show_3_motifs()
show_3_motifs <- function(){
  magick::image_read(path = system.file("motif_reference",
                                        "motif_reference_3motifs.png",
                                        package = utils::packageName()))
}

#' Display all supported motifs with signature ``2,2``.
#'
#' @return Opens figure from Ö. Bodin, M. Tengö: Disentangling intangible social–ecological systems in Global Environmental Change 22 (2012) 430–439 http://dx.doi.org/10.1016/j.gloenvcha.2012.01.005
#' @export
#'
#' @examples show_4_motifs()
show_4_motifs <- function(){
  magick::image_read(path = system.file("motif_reference",
                                        "motif_reference_4motifs.jpg",
                                        package = utils::packageName()))
}

#' Count multi-level motifs
#'
#' @param net A statnet network object with a node attribute specifying the
#'   level of each node
#' @param motifs a list of motif identifiers which shall be counted, e.g.
#'   list("1,2[I.C]")
#' @param type_attr character vector specifying the attribute name where level
#'   information is stored in statnet object
#' @param assume_sparse whether the network shall be assumed to be sparse (for
#'   optimization), default TRUE
#' @param omit_total_result whether total results shall be omitted, default
#'   FALSE
#'
#' @return data frame with counts indexed by motif identifiers
#' @export
#'
#' @examples count_motifs(ml_net, type_attr = c("sesType"), motifs=list("1,2[I.C]", "1,2[II.C]", "2,1[I.C]", "2,1[II.C]"))
count_motifs <- function(net,
                         motifs,
                         type_attr = c("sesType"),
                         assume_sparse = TRUE,
                         omit_total_result = TRUE){
  # convert net to python object
  py_g <- motifr::toPyGraph(net,type_attr = type_attr)

  # call counter
  counted <- sma$countMotifsAutoR(py_g,
                                  motifs,
                                  assume_sparse = assume_sparse,
                                  omit_total_result = omit_total_result)
  df <- data.frame(counted, check.names = FALSE)
  return(df)
}

#' Compute statistical properties of the distribution of motifs in a random
#' baseline
#'
#' @param g statnet network object
#' @param type_attr character vector specifying the attribute name where level
#'   information is stored in statnet object. The attribute should be a binary
#'   vector. 1 indicates a "social" node and 0 indicates a "non-social" node.
#' @param motifs list of motif identifiers describing the motifs whose
#'   distribution shall be analysed
#' @param model model to be used, either erdos_renyi or actors_choice, see sma
#'   documentation
#' @param level additional parameter for actors_choice
#' @param omit_total_result whether total results shall be omitted
#'
#' @return data frame indexed by motif identifers and with rows expectation and
#'   variances
#' @export
#'
#' @examples motifs_distribution(ml_net, motif = list('1,2[I.C]'))
motifs_distribution <- function(net,
                                motifs,
                                type_attr = c("sesType"),
                                model = c("erdos_renyi"),
                                level = -1,
                                omit_total_result = TRUE) {
  # convert net to python object
  py_g <- motifr::toPyGraph(net,type_attr = type_attr)

  # call counter
  result <- sma$distributionMotifsAutoR(py_g,
                                        motifs,
                                        model = model,
                                        level = level,
                                        omit_total_result = omit_total_result)
  df <- data.frame(result, check.names = FALSE)
  return(df)

}

#' Summary for motif counts and distribution (Erdos-Renyi)
#'
#' @param g statnet network object
#' @param type_attr character vector specifying the attribute name where level
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
  counts <- motifr::count_motifs(net, type_attr, motifs = motifs, omit_total_result = TRUE)
  distribution <- motifr::motifs_distribution(net, type_attr, motifs = motifs, omit_total_result = TRUE)

  # reformat data
  result <- rbind(counts, distribution)
  return(result)
}

#' Returns an example for a motif specified by a motif identifier string
#'
#' @param g statnet network object
#' @param motif motif identifier string for the motif
#' @param type_attr character vector specifying the attribute name where level
#'   information is stored in statnet object. The attribute should be a binary
#'   vector. 1 indicates a "social" node and 0 indicates a "non-social" node.
#'
#' @return vector of nodes in the motif
#' @seealso show_motif
#' @export
#'
#' @examples exemplify_motif(ml_net, motif = '1,2[I.C]')
exemplify_motif <- function(net,
                            motif,
                            type_attr = c("sesType")) {
  # convert net to python object
  py_g <- motifr::toPyGraph(net,type_attr = type_attr)
  motif <-sma$exemplifyMotif(py_g, motif)
  return(purrr::simplify(motif))
}

#' Plots an example for a motif with given motif identifier string taken from
#' the given graph.
#'
#' If no network is provided, a motif in a dummy network will be shown.
#'
#' @param motif motif identifier string for the motif
#' @param net statnet network object
#' @param type_attr character vector specifying the attribute name where level
#'   information is stored in statnet object. The attribute should be a binary
#'   vector. 1 indicates a "social" node and 0 indicates a "non-social" node.
#' @param ... additional arguments to be passed to plotting function
#' @return plot
#' @seealso exemplify_motif
#' @export
#'
#' @examples show_motif('1,2[I.C]', net = ml_net)
show_motif <- function(motif,
                       net = dummy_net,
                       type_attr = c("sesType"),
                       ...) {
  motif_names <- motifr::exemplify_motif(net = net, motif = motif,
                                         type_attr = type_attr)
  vertices <- network::get.vertex.attribute(net, "vertex.names")
  indices  <- sapply(motif_names, function(x){match(x, vertices)})
  subgraph <- network::get.inducedSubgraph(net, indices)
  p       <- motifr::plot_mnet(subgraph, type_attr = type_attr, ...)
  return(p)
}

#' Simulate a random baseline
#'
#' A specified number of random networks usind a modified Erdős-Rényi model is
#' computed. In each of the random networks motifs are counted. A dataframe with
#' this counts is returned.
#'
#' @param g statnet network object
#' @param motifs list of motif identifier strings
#' @param n number of random graphs
#' @param type_attr character vector specifying the attribute name where level
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
                              n = 10,
                              type_attr = c("sesType"),
                              assume_sparse = TRUE) {
  py_g <- motifr::toPyGraph(net, type_attr = type_attr)

  result <- sma$simulateBaselineAutoR(py_g, motifs, n = n, assume_sparse = assume_sparse)
  df <- data.frame(result, check.names = FALSE)
  return(df)
}

#' Compare empirical network to random baseline
#'
#' This function compares the motif counts in a given network with the motif
#' counts in a random baseline of motifs.
#'
#' @param g statnet network object
#' @param motifs list of motif identifier strings
#' @param n number of random graphs
#' @param type_attr character vector specifying the attribute name where level
#'   information is stored in statnet object. The attribute should be a binary
#'   vector. 1 indicates a "social" node and 0 indicates a "non-social" node.
#' @param assume_sparse whether the random graphs shall be assumed to be sparse.
#'   used to find ideal counting function
#'
#' @return data frame with one column for each motif identifier string and one
#'   row for every computed random graph
#' @export
#'
#' @examples compare_to_baseline(ml_net, list('1,2[I.C]', '1,2[II.C]'))
compare_to_baseline <- function(net,
                                motifs,
                                n = 10,
                                type_attr = c("sesType"),
                                assume_sparse = TRUE) {
  simulation <- motifr::simulate_baseline(net,
                                              motifs,
                                              n = n,
                                              type_attr = type_attr,
                                              assume_sparse = assume_sparse)
  count <- motifr::count_motifs(net,
                                    motifs,
                                    type_attr = type_attr,
                                    assume_sparse = assume_sparse,
                                    omit_total_result = TRUE)

  plot_df <- reshape2::melt(simulation)
  plot_df_count <- reshape2::melt(count)

  p <-
  ggplot2::ggplot(plot_df, ggplot2::aes(value)) +
    ggplot2::facet_wrap(~ variable, scales = "free") +
    ggplot2::geom_histogram(fill = "gray") +
    ggplot2::geom_vline(data = plot_df_count, ggplot2::aes(xintercept = value)) +
    ggplot2::theme_minimal() +
    ggplot2::xlab(sprintf("Simulated (gray histogram) versus actual (solid line) motif counts, n = %d iterations", n))

  return(p)
}
