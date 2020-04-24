
#' Visualize a multi-level network (using ggraph)
#'
#' @param net A statnet network object (igraph or tidygraph should also work)
#' @param type_attr The name of the categorical node attribute specifying at which level a node is situated
#' @param layouts A list of layouts (see ?ggraph::layout_ggraph) for every level eg. for two levels list("auto","circle")
#' @param label logical - should nodes be labelled? (defaults to false)
#'
#' @return A ggraph object
#' @export
#'
#' @examples
plot_mnet <- function(net,
                      type_attr = c("sesType"),
                      layouts = rep("kk",n_levels),
                      label = FALSE){

  t_g <- tidygraph::as_tbl_graph(net)
  nodes <- tibble::as_tibble(tidygraph::activate(t_g,nodes))
  edges <- tibble::as_tibble(tidygraph::activate(t_g,edges))

  colnames(nodes)[colnames(nodes) == type_attr] <- "sesType"
  # ensure numeric and starting at 1
  nodes$sesType_n <- as.numeric(factor(nodes$sesType))

  n_levels <- length(unique(nodes$sesType_n))

  edges$to_level <- nodes$sesType_n[edges$to]

  edges$from_level <- nodes$sesType_n[edges$from]

  edges$between <- ifelse(edges$from_level != edges$to_level, "within","between")

  t_g <- tidygraph::tbl_graph(nodes = nodes, edges = edges)

  # separate subgraphs
  sub_g_list <- vector(mode = "list", length = length(unique(nodes$sesType_n)))

  for(level in 1:n_levels){
    t_g_sub <- tidygraph::to_subgraph(t_g, sesType_n == level, subset_by = "nodes")$subgraph
    # if graph has no edges (bug in igraph), create self_loop to have one (hacky)
    if(nrow(tibble::as_tibble(tidygraph::activate(t_g_sub,edges))) == 0){
      disc_edges <- tibble::as_tibble(tidygraph::activate(t_g_sub,edges))
      disc_nodes <- tibble::as_tibble(tidygraph::activate(t_g_sub,nodes))
      disc_edges <- dplyr::bind_rows(disc_edges,tibble::tibble(to = 1, from = 1, between = NA))
      t_g_sub <- tidygraph::tbl_graph(nodes = disc_nodes, edges = disc_edges)
    }
    sub_g_list[[level]] <- ggraph::ggraph(graph = t_g_sub, layout = layouts[[level]]) +
      ggraph::geom_edge_loop()
  }

  #compute x and y offsets (very basic at the moment just stacking to the right up)
  coord_offset <- lapply(c(1:n_levels), function(lvl) {
    x <- lvl - 1.5
    y <- lvl - 1
    return(list(x = x, y = y))
  })

  for(level in 1:n_levels){
    sub_g_list[[level]][["data"]][["x"]] <- scales::rescale(sub_g_list[[level]][["data"]][["x"]],
                                                            to = c(0,1)) +
      coord_offset[[level]][["x"]]
    sub_g_list[[level]][["data"]][["y"]] <- scales::rescale(sub_g_list[[level]][["data"]][["y"]],
                                                            to = c(0,1)) +
      coord_offset[[level]][["y"]]
  }

  p_comb <- ggraph::ggraph(t_g, layout = "kk") +
    ggraph::geom_edge_link(ggplot2::aes(color =
                                          ifelse(edges$between == "between",
                                                 "#646973","#b1b4ba")))

  for(level in 1:n_levels){
    p_comb[["data"]][["x"]][
      p_comb[["data"]][["sesType_n"]] == level] <-
      sub_g_list[[level]][["data"]][["x"]]

    p_comb[["data"]][["y"]][
      p_comb[["data"]][["sesType_n"]] == level] <-
      sub_g_list[[level]][["data"]][["y"]]
  }

  p_comb <-
    p_comb +
    ggraph::geom_node_point(ggplot2::aes(color = factor(sesType))) +
    ggplot2::theme_void() +
    ggplot2::scale_color_brewer("Level", breaks = levels(factor(nodes$sesType)),
                               palette = "Accent") +
    ggplot2::theme(legend.position="bottom")

  if(label == TRUE){
    p_comb <-
      p_comb + ggraph::geom_node_label(ggplot2::aes(label = name, fill = factor(sesType)),
                                       alpha = 0.5) +
      ggplot2::scale_fill_brewer("Level", breaks = levels(factor(nodes$sesType)),
                                 palette = "Accent")
  }

  p_comb + ggplot2::theme(plot.margin=ggplot2::unit(c(0.5,0.5,0.5,0.5),"cm"))

  }
