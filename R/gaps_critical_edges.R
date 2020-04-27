#' List gaps
#'
#' List gaps ordered by contribution to a motif. This is a list of ties together
#' with the number of motifs of a given class the dyad would generate by being
#' added to the netwok.
#'
#' @param g statnet network object
#' @param motif motif identifier
#' @param type_attr character vector specifying the attribute name where level
#'   information is stored in statnet object. The attribute should be a binary
#'   vector. 1 indicates a "social" node and 0 indicates a "non-social" node.
#' @param level level of the dyads which shall be considered, or -1 if the level
#'   shall be determined automatically.
#'
#' @return data frame with three columns, listing edges and their contribution
#'   to motifs described by the motif identifier in descending order
#' @export
#'
#' @examples identify_gaps(ml_net, list('1,2[II.C]'))
identify_gaps <- function(net,
                          motif,
                          type_attr = c("sesType"),
                          level = -1) {
  # safety check temporarily disabled
  # if(sma$isClosedMotifR(motif, level) == FALSE){
  #   stop("The specified motif is not closed. Look for critical_dyads instead.")
  # }
  return(edge_contribution(net = net, motif = motif, type_attr = type_attr, level = level))
}

#' List critical dyads
#'
#' Critical dyads are edges on a specified level which break motifs by being
#' removed from the network.
#'
#' @param g statnet network object
#' @param motif motif identifier
#' @param type_attr character vector specifying the attribute name where level
#'   information is stored in statnet object. The attribute should be a binary
#'   vector. 1 indicates a "social" node and 0 indicates a "non-social" node.
#' @param level level of the dyads which shall be considered, or -1 if the level
#'   shall be determined automatically.
#'
#' @return data frame with three columns, listing edges and their contribution
#'   to motifs described by the motif identifier in descending order
#' @export
#'
#' @examples identify_gaps(ml_net, list('1,2[I.C]'))
critical_dyads <- function(net,
                           motif,
                           type_attr = c("sesType"),
                           level = -1) {
  if(sma$isClosedMotifR(motif, level) == TRUE){
    stop("The specified motif is not open. Look for identify_gaps instead.")
  }
  return(edge_contribution(net = net, motif = motif, type_attr = type_attr, level = level))
}

#' List edge contribution
#'
#' List gaps ordered by contribution to a motif. This is a list of ties together
#' with the number of motifs of a given class the dyad would generate by being
#' flipped. This is a gneralisation of ``identify_gaps`` and ``critical_dyads``.
#'
#' @param g statnet network object
#' @param motif motif identifier
#' @param type_attr character vector specifying the attribute name where level
#'   information is stored in statnet object.
#' @param level level of the dyads which shall be considered, or -1 if the level
#'   shall be determined automatically.
#'
#' @return data frame with three columns, listing edges and their contribution
#'   to motifs described by the motif identifier in descending order
#' @export
#'
#' @examples edge_contribtion(ml_net, list('1,2[I.C]'))
edge_contribution <- function(net,
                              motif,
                              type_attr = c("sesType"),
                              level = -1) {
  py_g <- motifr::toPyGraph(g = net, type_attr = type_attr)
  result <- sma$identifyGapsR(py_g, motif, level = level)
  df <- data.frame(result)
  return(df)
}

#' Plot gaps in network vizualization
#'
#' @param net Statnet network object
#' @param motif Motif to explore gaps in for
#' @param type_attr Node attribute specifiying level information
#' @param level Focal level for gap analysis
#' @param cutoff Cutoff point in contributions of an edge to the number of motifs above which to analyse gaps
#' @param ...
#' @param subset_graph Whether to subset the graph to only show nodes involved in gaps.
#' One of "none" (no subset, default), "partial" (only focal level is subset) or "focal" (only focal
#' level shown)
#'
#' @return
#' @export
#'
#' @examples plot_gaps(ml_net, "1,2[II.C]", level = -1)
#' plot_gaps(ml_net, "1,2[II.C]", level = -1, subset_graph = "focal", cutoff = 4, label = TRUE)
#' plot_gaps(ml_net, "1,2[II.C]", level = -1, subset_graph = "partial", cutoff = 4, label = TRUE)
plot_gaps <- function(net,
                      motif,
                      type_attr = c("sesType"),
                      level = -1,
                      cutoff = 2,
                      subset_graph = "none",
                      ...){

  gaps <- motifr::identify_gaps(net = net,
                                motif = motif,
                                type_attr = type_attr,
                                level = level)
  gaps <- gaps[gaps$contribution >= cutoff,]

  gap_nodes <- unique(c(gaps$vertex0,gaps$vertex1))

  if(class(net) == "network"){
    net <- intergraph::asIgraph(net)
    net <-
      igraph::set.vertex.attribute(net, name = "name", value =
                                     igraph::get.vertex.attribute(net,"vertex.names"))
  }

  t_g <- tidygraph::as_tbl_graph(net)
  nodes <- tibble::as_tibble(tidygraph::activate(t_g,nodes))
  edges <- tibble::as_tibble(tidygraph::activate(t_g,edges))

  edges$to_name <- nodes$name[edges$to]
  edges$from_name <- nodes$name[edges$from]

  colnames(nodes)[colnames(nodes) == type_attr] <- "sesType"

  gap_level <- unique(nodes$sesType[nodes$name %in% gap_nodes])

  if (subset_graph == "partial"){
    non_focal <- unique(edges$to_name[edges$from_name %in% gap_nodes],
                   edges$from_name[edges$to_name %in% gap_nodes])
    non_focal <- non_focal[non_focal %in% nodes$name[!(nodes$sesType %in% gap_level)]]
    nodes <- nodes[nodes$name %in% gap_nodes | nodes$name %in% non_focal,]
    t_g <- tidygraph::to_subgraph(t_g, name %in% nodes$name, subset_by = "nodes")$subgraph
  }

  if (subset_graph == "focal"){
    nodes <- nodes[nodes$name %in% gap_nodes,]
    t_g <- tidygraph::to_subgraph(t_g, name %in% nodes$name, subset_by = "nodes")$subgraph
  }

  netviz <- plot_mnet(net = t_g, type_attr = type_attr, ...)

  # get coordinates for all lines
  coord_gaps <- data.frame(
    x1 =
      unlist(lapply(gaps$vertex0, function(vertex){netviz$data$x[netviz$data$name == vertex]})),
    y1 =
      unlist(lapply(gaps$vertex0, function(vertex){netviz$data$y[netviz$data$name == vertex]})),
    x2 =
      unlist(lapply(gaps$vertex1, function(vertex){netviz$data$x[netviz$data$name == vertex]})),
    y2 =
      unlist(lapply(gaps$vertex1, function(vertex){netviz$data$y[netviz$data$name == vertex]})),
    weight = gaps$contribution
  )

  netviz +
    ggplot2::geom_segment(data = coord_gaps,
                          ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2, size = weight),
                          colour="#f2790070", alpha = 0.7) +
    scale_size_continuous("Gap weight", range = c(1,2))

}
