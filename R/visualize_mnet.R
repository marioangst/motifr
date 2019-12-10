
#' Visualize a two-level network of actors and issues
#'
#' @param actor_df Data frame containing actor information. Must contain a column named "actor" providing actor id.
#' @param issue_df Data frame containing issue information. Must contain a column named "issue" providing issue id.
#' @param actor_links Links between actors, must contain columns named "sender" and "receiver"
#' @param actor_issue_links Links between actors and issues, must contain columns named "sender" and "receiver"
#' @param issue_links Links between issues, must contain columns named "sender" and "receiver"
#' @param viz Choose which R package to use for visualization. At present options are "graphviz" (using DiagrammeR, fdp layout)
#' and "multinets" using the multinets package
#'
#' @return Returns a plot.
#' @export
#'
#' @examples
visualize_mnet <- function(actor_df,
                            issue_df,
                            actor_links,
                            actor_issue_links,
                            issue_links,
                           viz = c("multinets", "graphviz")){

  unique_actors <- unique(actor_df$actor)
  unique_issues <- unique(issue_df$issue)
  actor_issue_incidence <- matrix(0,nrow = length(unique_actors),
                                    ncol = length(unique_issues),
                                  dimnames = list(unique_actors, unique_issues))
  actor_issue_incidence[cbind(actor_issue_links$sender, actor_issue_links$receiver)] <- 1

  bip_graph <- igraph::graph_from_incidence_matrix(actor_issue_incidence)

  actor_actor_graph <- igraph::graph_from_data_frame(d = actor_links,vertices = actor_df, directed = FALSE)

  issue_issue_graph <- igraph::graph_from_data_frame(d = issue_links, vertices = issue_df, directed = FALSE)

  multilevel_graph <- igraph::graph.union(bip_graph,actor_actor_graph,issue_issue_graph)

  names_att <- igraph::V(multilevel_graph)$name
  type_multi <- ifelse(names_att %in% actor_df$actor, TRUE, FALSE)
  igraph::V(multilevel_graph)$type <- type_multi

  if(viz == "multinets"){
    # l <- multinets::layout_multilevel(multilevel_graph, layout = igraph::layout_with_fr)
    l2 <- multinets::layout_multilevel(multilevel_graph, layout = igraph::layout.drl)

    multilevel_graph <- multinets::set_color_multilevel(multilevel_graph)
    multilevel_graph <- multinets::set_shape_multilevel(multilevel_graph)

    # Plot with simple colors
    graphics::plot(multilevel_graph,
         layout = l2,
         vertex.size = 5,
         vertex.label = " ")
  }

  if (viz == "graphviz"){
    nodes_df<- data.frame(label = igraph::V(multilevel_graph)$name,
                          level = igraph::V(multilevel_graph)$type, stringsAsFactors = FALSE)
    nodes_df$level <- ifelse(nodes_df$level,"actor","issue")

    edges_df <- as.data.frame(igraph::get.edgelist(multilevel_graph,names = TRUE), stringsAsFactors = FALSE)
    colnames(edges_df) <- c("sender","receiver")

    ml_graphv <- DiagrammeR::create_graph()
    ml_graphv <-
      DiagrammeR::add_nodes_from_table(graph = ml_graphv,
                           table = nodes_df,
                           label_col = "label",
                           type_col = "level")


    ml_graphv <- DiagrammeR::add_edges_from_table(graph = ml_graphv,
                                                  table = edges_df,
                                                  from_col = "sender",
                                                  to_col = "receiver",
                                                  from_to_map = "label")

    ml_graphv <-
      DiagrammeR::add_global_graph_attrs(graph = ml_graphv, "layout", "fdp", "graph")
    ml_graphv <-
      DiagrammeR::add_global_graph_attrs(graph = ml_graphv,"K", "3", "graph")
    #additional params for fdp are especially K (to consider)

    ml_graphv$nodes_df$fillcolor <- ifelse(nodes_df$level == "actor", "red", "blue")
    ml_graphv$nodes_df$shape <- ifelse(nodes_df$level == "actor", "circle", "square")

    #make undirected - can later change per edge (cool stuff)
    ml_graphv$edges_df$dir <- "none"

    node_df <- DiagrammeR::get_node_df(ml_graphv)
    actor_ids <- node_df$id[node_df$type == "actor"]
    issue_ids <- node_df$id[node_df$type == "issue"]
    actor_ids_pasted <- paste(node_df$id[node_df$type == "actor"],collapse = ";")
    issue_ids_pasted <- paste(node_df$id[node_df$type == "issue"],collapse = ";")

    within_actor_edges <- (ml_graphv$edges_df$from %in% actor_ids) & (ml_graphv$edges_df$to %in% actor_ids)
    within_issue_edges <- (ml_graphv$edges_df$from %in% issue_ids) & (ml_graphv$edges_df$to %in% issue_ids)

    ml_graphv$edges_df$color <- ifelse(within_actor_edges,"red",ifelse(within_issue_edges,"blue","gray"))

    ml_graphv$global_attrs <- rbind(ml_graphv$global_attrs,
                                    c("fontsize","10","node"),
                                    c("arrowsize",1,"edge"))

    dot_out <- DiagrammeR::generate_dot(ml_graphv)

    clust_dot <-
      glue::glue(
        "subgraph cluster_0 {{
    label=\"Actors\";
    {actor_ids_pasted}
    }}
  subgraph cluster_1 {{
    label=\"Issues\";
  {issue_ids_pasted}
  }}")

    dot_out <- gsub(pattern = "}$",
                    replacement = paste(clust_dot,"}"),
                    x = dot_out)
    DiagrammeR::grViz(dot_out)
  }

}


# # some tests
# load(file = "data/reussebene_mlnet.RData")
# agg <- aggregate_cld(cld_concepts = cld_concepts, cld_el = cld_el,
#                      agg_method = "w_sum", type_col = "type",
#                      id_col = "concept",
#                      agg_target_type = "Activity",
#                      types_to_use_in_agg = "Direct Threat",
#                      max_path_length = 2,order_to_consider = 2)
#
# issue_links <- as.data.frame(igraph::as_edgelist(igraph::graph_from_adjacency_matrix(agg)), stringsAsFactors = FALSE)
# colnames(issue_links) <- c("sender","receiver")
# issue_links$weight <- agg[cbind(issue_links$sender,issue_links$receiver)]
#
# issue_df <- data.frame(issue = rownames(agg), stringsAsFactors = FALSE)
# visualize_mnet(actor_df = actors,
#                issue_df = issue_df,
#                actor_links = actor_el,
#                actor_issue_links = actor_concept_el,
#                issue_links = issue_links,viz = "graphviz"
#                )
