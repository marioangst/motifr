#' Aggregate a causal loop diagram
#'
#' @param cld_concepts A data frame with the concepts occuring in the causal loop diagram,
#' including at least a column naming the concepts and one column specifiying type of concept
#' (even if all concepts are of the same type)
#' @param cld_el A data frame with a column named "sender" and a column named "receiver" storing information of
#' links between concepts
#' @param agg_method Choose an aggregation method. At the moment the inverse shortest distance ("dist") between concepts and
#' the weighted sum of paths ("w_sum") are available. The weighted sum of paths adds all inverse path lengthts between paths.
#' @param type_col Character string specifying name of column where concept type is stored in cld_concepts
#' @param id_col Character string specifying name of column where concept names are stored in cld_concepts.
#' These must match with how concepts are called in cld_el
#' @param agg_target_type Character vector specifying the type(s) of concept(s) that are the target of the aggregation procedure
#' @param types_to_use_in_agg Character vector, specifying wether the aggregation should only be based on a subset of all
#' types of concepts. aggregate_cld then subsets the diagram before computation
#' (a method preserving the diagram and not counting some paths might be implemented later)
#' @param order_to_consider Integer setting the order of the neighborhood to consider in aggregation
#' @param max_path_length Integer setting the maximum path length to consider in aggregation
#'
#' @return An valued adjacency matrix specifying relations between all pairs of concepts
#' @export
#'
#' @examples
aggregate_cld <- function(cld_concepts, cld_el, agg_method = c("dist","w_sum"),
                          type_col, id_col,
                          agg_target_type, types_to_use_in_agg = NULL,
                          order_to_consider, max_path_length){

  #pre-treatment of data
  if(is.null(types_to_use_in_agg)){
    unique_use_targets <- unique(cld_concepts[[as.character(id_col)]])
  }
  else{
    unique_use_targets <- unique(cld_concepts[[as.character(id_col)]])
    unique_use_targets <- unique_use_targets[cld_concepts[[as.character(type_col)]] %in% types_to_use_in_agg]
  }
  unique_agg_targets <- unique(cld_concepts[[as.character(id_col)]])
  unique_agg_targets <- unique_agg_targets[cld_concepts[[as.character(type_col)]] %in% agg_target_type]

  unique_concepts_subset <- unique(unique_use_targets, unique_agg_targets)
  #create adjacency mat of subset to work with
  adj_mat <- matrix(0,nrow = length(unique_concepts_subset),
                    ncol = length(unique_concepts_subset),
                    dimnames = list(unique_concepts_subset,unique_concepts_subset))

  cld_el_subset <- cld_el[cld_el$sender %in% unique_concepts_subset &
                            cld_el$receiver %in% unique_concepts_subset,]
  adj_mat[cbind(cld_el_subset$sender,cld_el_subset$receiver)] <- 1

  # combinations of all agg targets
  all_agg_target_combs <- t(utils::combn(unique_agg_targets,2,simplify = TRUE))

  #make graph subset
  graph_subset <- igraph::graph_from_adjacency_matrix(adj_mat)

  if(agg_method == "dist"){
    dist_mat <-
      get_shortest_inv_distance(input_graph = graph_subset,
                          agg_target_vec = unique_agg_targets, max_path = max_path_length)
    return(dist_mat)
  }
  if(agg_method == "w_sum"){
    weighted_path_sums <-
      pbapply::pbapply(all_agg_target_combs,1,
            function(x){
              get_weighted_sum_of_paths(issue1 = x[1],
                                        issue2 = x[2],
                                        order_chosen = order_to_consider,
                                        input_graph = graph_subset,
                                        max_path = max_path_length)
            })
    weighted_path_sums_adjmat <- matrix(0,nrow = length(unique_agg_targets),
                                        ncol = length(unique_agg_targets),
                                        dimnames = list(unique_agg_targets,unique_agg_targets))
    weighted_path_sums_adjmat[all_agg_target_combs] <- weighted_path_sums
    # weighted_path_sums_graph <- graph_from_adjacency_matrix(weighted_path_sums_adjmat,
    #                                                         weighted = TRUE,mode = "undirected")
    return(weighted_path_sums_adjmat)
  }
}


# lower level functions -----

#' Get the weighted sum of paths between two issues
#'
#' @param issue1 First issue
#' @param issue2 Second issue
#' @param input_graph Input graph
#' @param order_chosen Integer setting the order of the neighborhood to consider in aggregation
#' @param max_path Integer setting the maximum path length to consider in aggregation
#'
#' @return
#' @export
#'
#' @examples
get_weighted_sum_of_paths <- function(issue1, issue2,
                                      input_graph,
                                      order_chosen, max_path){
  neighborhood_graph_issue1 <- igraph::make_ego_graph(input_graph,order = order_chosen,
                                              nodes = issue1,mode = "all")[[1]]
  neighborhood_graph_issue2 <- igraph::make_ego_graph(input_graph,order = order_chosen,
                                              nodes = issue2,mode = "all")[[1]]
  neighborhood_graph_union <- igraph::graph.union(neighborhood_graph_issue1, neighborhood_graph_issue2)
  paths <- igraph::all_simple_paths(neighborhood_graph_union,from = issue1,
                            to = issue2,
                            mode = "all")
  path_lengths <- unlist(lapply(paths,
                                function(x) ifelse(length(x) > 0, length(x)-1,0)))
  path_lengths <- path_lengths[path_lengths < max_path]
  weighted_sum <- sum(1/path_lengths)
  return(weighted_sum)
}

#' Get the shortest inverse distance between two issues
#'
#' @param input_graph Input graph
#' @param max_path Integer setting the maximum distance to still consider
#' @param agg_target_vec Character vector specifying nodes between which inverse shortest distances are to be calculated
#'
#' @return
#' @export
#'
#' @examples
get_shortest_inv_distance <- function(input_graph,max_path, agg_target_vec){
  shortest_distances <- igraph::distances(input_graph, v = agg_target_vec, to = agg_target_vec)
  shortest_distances <- ifelse(shortest_distances > max_path,
                               0,
                               shortest_distances)
  shortest_distances_inv <- 1/as.matrix(shortest_distances)
  shortest_distances_inv[is.infinite(shortest_distances_inv)] <- 0
  return(shortest_distances_inv)
  # distance_graph <- graph_from_adjacency_matrix(shortest_distances_inv,
  #                                               weighted = TRUE,
  #                                               mode = "undirected")
}

# quick tests:

load(file = "data/reussebene_mlnet.RData")
aggregate_cld(cld_concepts = cld_concepts, cld_el = cld_el,
              agg_method = "w_sum", type_col = "type",
              id_col = "concept",
              agg_target_type = "Indirect Threat",
              types_to_use_in_agg = "Indirect Threat",
              max_path_length = 2,order_to_consider = 2)



