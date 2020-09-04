# Script for creating directed_dummy_net

# https://gitlab.com/t.seppelt/sesmotifanalyser/-/blob/master/test/data/digraph.py
g <- igraph::make_empty_graph(directed = TRUE) %>%
  igraph::add_vertices(3, sesType = 0) %>%
  igraph::add_vertices(3, sesType = 1) %>%
  igraph::add_edges(c(1,4, 1,2, 1,5, 1,3, 1,6, 4,5, 4,3, 4,6, 3,6, 5,2,
                      3,4, 3,2, 3,1, 5,1))´
