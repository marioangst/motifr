

load(file = "data/reussebene_mlnet.RData")
agg <- aggregate_cld(cld_concepts = cld_concepts, cld_el = cld_el,
              agg_method = "w_sum", type_col = "type",
              id_col = "concept",
              agg_target_type = "Activity",
              types_to_use_in_agg = "Direct Threat",
              max_path_length = 2,order_to_consider = 2)

issue_links <- igraph::as_edgelist(igraph::graph_from_adjacency_matrix(agg))
colnames(issue_links) <- c("sender","receiver")
issue_df <- data.frame(issue = rownames(agg), stringsAsFactors = FALSE)

visualize_mnet <- function(actor_df,
                            issue_df,
                            actor_links,
                            actor_issue_links,
                            issue_links){

  unique_actors <- unique(actor_df$actor)
  unique_issues <- unique(issue_df$issue)
  actor_issue_incidence <- matrix(0,nrow = length(unique_actors),
                                    ncol = length(unique_issues),
                                  dimnames = list(unique_actors, unique_issues))
  actor_issue_incidence[cbind(actor_issue_links$sender, actor_issue_links$receiver)] <- 1

  bip_graph <- igraph::graph_from_incidence_matrix()
  plot(bip_graph)

  actor_actor_graph <- graph_from_adjacency_matrix(general_collab_mat,mode = "undirected",
                                                   diag = FALSE)
  plot(actor_actor_graph)
  actor_clusters <- cluster_louvain(actor_actor_graph)

  # based on whole os
  # os_graph <- make_undirected_graph(cbind(links_named$sender, links_named$receiver))
  # based on aggregation (here distance)
  issue_issue_graph <- distance_graph
  # igraph::V(issue_issue_graph)$name <- rename_based_on_codebook(igraph::V(issue_issue_graph)$name,
  #                                                               codebook = concepts,
  #                                                               rawvar = "Concept",
  #                                                               codevar = "english")
  plot(issue_issue_graph)
  issue_clusters <- cluster_louvain(issue_issue_graph)

  # not needed
  multi_clusters <- data.frame(name = c(issue_clusters$names, actor_clusters$names),
                               cluster = c(issue_clusters$membership,
                                           actor_clusters$membership + max(issue_clusters$membership)),
                               stringsAsFactors = FALSE)
  rownames(multi_clusters) <- multi_clusters$name

  multilevel_graph <- graph.union(bip_graph,actor_actor_graph,issue_issue_graph)

  names_att <- igraph::V(multilevel_graph)$name
  type_multi <- ifelse(names_att %in% rownames(general_collab_mat), TRUE, FALSE)
  igraph::V(multilevel_graph)$type <- type_multi
  is_multilevel(multilevel_graph)

  #plot
  l <- layout_multilevel(multilevel_graph, layout = layout_with_fr)
  l2 <- layout_multilevel(multilevel_graph, layout = layout.drl)

  multilevel_graph <- set_color_multilevel(multilevel_graph)
  multilevel_graph <- set_shape_multilevel(multilevel_graph)

  # Plot with simple colors
  plot(multilevel_graph,
       layout = l,
       vertex.size = 5,
       vertex.label = " ")

  # set colors based on clusters
  V(multilevel_graph)[multi_clusters$name]$community <- multi_clusters$cluster
  rain <- rainbow(length(unique(multi_clusters$cluster)), alpha=.5)
  V(multilevel_graph)$color <- rain[V(multilevel_graph)$community]

  E(multilevel_graph)$color <- apply(get.edgelist(multilevel_graph),
                                     1, function(x) ifelse(
                                       V(multilevel_graph)[x[1]]$community ==
                                         V(multilevel_graph)[x[2]]$community,
                                       rain[V(multilevel_graph)[x[1]]$community],
                                       '#E5DCDA'))

  # Plot
  plot(multilevel_graph,
       layout = l,
       vertex.size = 5,
       vertex.label = " ")
}
