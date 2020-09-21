# Script for creating dummy tidygraph object
library(tidygraph)

tidygraph_dummy_net <- tbl_graph(directed = FALSE) %>%
  bind_nodes(data.frame(new = 1:3, sesType = 0)) %>%
  bind_nodes(data.frame(new = 3:5, sesType = 1)) %>%
  bind_edges(data.frame(from = 1, to = 2:6)) %>%
  bind_edges(data.frame(from = 4, to = 5:6))
plot(tidygraph_dummy_net)

save(tidygraph_dummy_net, file = "data/tidygraph_dummy_net.RData")
