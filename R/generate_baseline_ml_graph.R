
generate_random_ml_graph <- function(n_social_nodes,
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
