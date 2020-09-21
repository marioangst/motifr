
#' Visualize a multi-level network (using ggraph)
#'
#' Visualize a multi-level network, with the possibility of specifying separate
#' layouts for each level. This is a somewhat hacky wrapper for arranging
#' separate ggraph calls for each network level in a circle.
#'
#' @param net A tidygraph, igraph or statnet network object
#' @param lvl_attr The name of the categorical node attribute specifying at
#'   which level a node is situated
#' @param layouts A list of layouts (see ?ggraph::layout_ggraph) for every level
#'   eg. for two levels list("auto","circle")
#' @param label logical - should nodes be labelled? (defaults to false)
#'
#' @return A ggraph object
#' @export
#'
#' @examples
#' plot_mnet(net = ml_net, lvl_attr = "sesType", layouts = list("kk", "circle"))
plot_mnet <- function(net,
                      lvl_attr = c("sesType"),
                      layouts = rep("kk", n_levels),
                      label = FALSE) {
  if (network::is.network(net)) {
    net <- intergraph::asIgraph(net)
    net <-
      igraph::set.vertex.attribute(net,
        name = "name", value =
          igraph::get.vertex.attribute(net, "vertex.names")
      )
  }

  t_g <- tidygraph::as_tbl_graph(net)
  nodes <- tibble::as_tibble(tidygraph::activate(t_g, nodes))
  edges <- tibble::as_tibble(tidygraph::activate(t_g, edges))

  colnames(nodes)[colnames(nodes) == lvl_attr] <- "lvl"
  # ensure numeric and starting at 1
  nodes$lvl_n <- as.numeric(factor(nodes$lvl))

  n_levels <- length(unique(nodes$lvl_n))

  edges$to_level <- nodes$lvl_n[edges$to]

  edges$from_level <- nodes$lvl_n[edges$from]

  edges$between <- ifelse(edges$from_level != edges$to_level, "within", "between")

  t_g <- tidygraph::tbl_graph(nodes = nodes, edges = edges)

  # separate subgraphs
  sub_g_list <- vector(mode = "list", length = length(unique(nodes$lvl_n)))

  for (level in 1:n_levels) {
    #' @importFrom rlang .data
    t_g_sub <- tidygraph::to_subgraph(t_g, .data$lvl_n == level, subset_by = "nodes")$subgraph
    # if graph has no edges (bug in igraph), create self_loop to have one (hacky)
    if (nrow(tibble::as_tibble(tidygraph::activate(t_g_sub, edges))) == 0) {
      disc_edges <- tibble::as_tibble(tidygraph::activate(t_g_sub, edges))
      disc_nodes <- tibble::as_tibble(tidygraph::activate(t_g_sub, nodes))
      disc_edges <- dplyr::bind_rows(disc_edges, tibble::tibble(to = 1, from = 1, between = NA))
      t_g_sub <- tidygraph::tbl_graph(nodes = disc_nodes, edges = disc_edges)
    }
    sub_g_list[[level]] <- ggraph::ggraph(graph = t_g_sub, layout = layouts[[level]]) +
      ggraph::geom_edge_loop()
  }

  # compute x and y offsets
  # When arranging n points on a circle, the k-th point, k = 0, …, n-1, has coordinates
  # x = cos(2 * pi * k /n) and y = sin(2 * pi * k/n). This gives coordinates ranging from -1 to 1.
  coord_offset <- lapply(c(1:n_levels), function(level) {
    x <- cos(2 * pi * level / n_levels)
    y <- sin(2 * pi * level / n_levels)
    return(list(x = x, y = y))
  })

  for (level in 1:n_levels) {
    sub_g_list[[level]][["data"]][["x"]] <- scales::rescale(sub_g_list[[level]][["data"]][["x"]],
      to = c(0, 1)
    ) +
      coord_offset[[level]][["x"]]
    sub_g_list[[level]][["data"]][["y"]] <- scales::rescale(sub_g_list[[level]][["data"]][["y"]],
      to = c(0, 1)
    ) +
      coord_offset[[level]][["y"]]
  }

  if ("ggraph" %in% (.packages())) {
    detach("package:ggraph", unload = TRUE)
  }

  arrow <- NULL
  if(motifr::is.directed(net)) {
    arrow <- grid::arrow(angle = 10, length = ggplot2::unit(.3, "cm"))
  }

  p_comb <- ggraph::ggraph(t_g, layout = "kk") +
    ggraph::geom_edge_link(ggplot2::aes(
      color =
        ifelse(edges$between == "between",
          "#64697380", "#b1b4ba50"
        )
    ),
    arrow = arrow)

  for (level in 1:n_levels) {
    p_comb[["data"]][["x"]][
      p_comb[["data"]][["lvl_n"]] == level
    ] <-
      sub_g_list[[level]][["data"]][["x"]]

    p_comb[["data"]][["y"]][
      p_comb[["data"]][["lvl_n"]] == level
    ] <-
      sub_g_list[[level]][["data"]][["y"]]
  }

  p_comb <-
    p_comb +
    ggraph::geom_node_point(ggplot2::aes_(color = ~ factor(lvl))) +
    ggplot2::theme_void() +
    ggplot2::scale_color_brewer("Level",
      breaks = levels(factor(nodes$lvl)),
      palette = "Dark2"
    ) +
    ggplot2::theme(legend.position = "bottom")

  if (label == TRUE) {
    p_comb <-
      p_comb + ggraph::geom_node_label(ggplot2::aes_(label = ~name, fill = ~ factor(lvl)),
        alpha = 0.5
      ) +
      ggplot2::scale_fill_brewer("Level",
        breaks = levels(factor(nodes$lvl)),
        palette = "Dark2"
      )
  }

  p_comb + ggplot2::theme(plot.margin = ggplot2::unit(c(1, 1, 1, 1), "cm"))
}
