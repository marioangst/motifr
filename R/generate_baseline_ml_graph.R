
#' Generate a random multi-level adjacency matrix
#'
#' @param n_social_nodes the number of social nodes in the network
#' @param n_non_social_nodes the number of "non-social" nodes in the network
#' @param density density of total network, defaults to random
#'
#' @return labeled adjacency matrix
#' @export
#'
#' @examples
generate_random_ml_adjmat <- function(n_social_nodes,
                                     n_non_social_nodes,
                                     density = NULL){
  load_python_sma()
  reticulate::source_python("inst/python/generate_random_graph.py",
                            convert = FALSE)
  adj_mat <- get_random_adj_mat(n_non_social_nodes = n_non_social_nodes,
                                n_social_nodes = n_social_nodes,
                                density = density)
  adj_mat <- reticulate::py_to_r(adj_mat)
  adj_mat
}

#' Generate a random multi-level network (statnet network object)
#'
#' @param n_social_nodes the number of social nodes in the network
#' @param n_non_social_nodes the number of "non-social" nodes in the network
#' @param density density of total network, defaults to random
#' @param to_py should the object be returned as a sma compatible python object
#'
#' @return either a statnet network object with an attribute "sesType" specifying node type or an equivalent python compatible object
#' @export
#'
#' @examples
generate_random_ml_net <- function(n_social_nodes,
                                   n_non_social_nodes,
                                   density = NULL,
                                   to_py = FALSE){
  adjmat <- generate_random_ml_adjmat(n_social_nodes = n_social_nodes,
                                      n_non_social_nodes = n_non_social_nodes,
                                      density = density)
  net <- network::network(adjmat)
  network::set.vertex.attribute(net, "sesType", ifelse(grepl(pattern = "non-social",
                                                             x = network::get.vertex.attribute(net,
                                                                                               "vertex.names")),
                                                       1,0))
  if(to_py == TRUE){
    pynet <- toPyGraph(net,typeAttr = "sesType")
    return(pynet)
  }
  else{
    return(net)
  }
}

# first idea for baseline distribution, incredibly slow
# hist(unlist(lapply(netlist[1:500], integrateR::count_motifs, "sesType", 3, "II.A", "social")))
