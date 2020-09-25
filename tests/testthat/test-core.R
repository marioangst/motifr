test_that("io_undirected_dummy_net", {
  testthat::expect_warning(motifr::to_py_graph(motifr::dummy_net, "sesType", directed = TRUE))

  py_g <- to_py_graph(motifr::dummy_net, "sesType")
  testthat::expect_false(nx$is_directed(py_g))

  testthat::expect_equal(
    network::network.size(dummy_net),
    nx$number_of_nodes(py_g)
  )
  testthat::expect_equal(
    network::network.edgecount(dummy_net),
    nx$number_of_edges(py_g)
  )
  nodes_count <- sma$nodesCount(py_g)
  types <- table(network::get.vertex.attribute(dummy_net, "sesType"))
  testthat::expect_equal(length(nodes_count), length(types))
  testthat::expect_equal(nodes_count$`0`, types[[1]])
  testthat::expect_equal(nodes_count$`1`, types[[2]])
  testthat::expect_equal(nodes_count$`2`, types[[3]])
})
test_that("io_undirected_dummy_net_issue#27", {
  # this checks that the workaround for
  # the issue https://github.com/marioangst/motifr/issues/27
  # works correctly
  ml_net27 <- network::set.vertex.attribute(ml_net,
                                            "lvl",
                                            as.character(network::get.vertex.attribute(ml_net,"sesType")))
  # suppress UserWarning issued by sma.translateGraph()
  ms <- reticulate::py_suppress_warnings(motif_summary(ml_net27, lvl_attr = "lvl"))
  testthat::expect_equal(ms$count,
                         c(543, 167, 217, 7, 73, 1))
})
test_that("io_directed_ml_net", {
  py_g <- to_py_graph(motifr::ml_net, "sesType")
  testthat::expect_false(nx$is_directed(py_g))

  testthat::expect_equal(
    network::network.size(ml_net),
    nx$number_of_nodes(py_g)
  )
  testthat::expect_equal(
    network::network.edgecount(ml_net),
    nx$number_of_edges(py_g)
  )
  nodes_count <- sma$nodesCount(py_g)
  types <- table(network::get.vertex.attribute(ml_net, "sesType"))
  testthat::expect_equal(length(nodes_count), length(types))
  testthat::expect_equal(nodes_count$`0`, types[[1]])
  testthat::expect_equal(nodes_count$`1`, types[[2]])
})

test_that("io_directed_igraph", {
  # https://gitlab.com/t.seppelt/sesmotifanalyser/-/blob/master/test/data/digraph.py
  py_g <- to_py_graph(motifr::directed_dummy_net, "sesType")
  testthat::expect_true(nx$is_directed(py_g))

  testthat::expect_equal(
    igraph::gorder(motifr::directed_dummy_net),
    nx$number_of_nodes(py_g)
  )
  testthat::expect_equal(
    igraph::gsize(motifr::directed_dummy_net),
    nx$number_of_edges(py_g)
  )
  nodes_count <- sma$nodesCount(py_g)
  types <- table(igraph::V(motifr::directed_dummy_net)$sesType)
  testthat::expect_equal(length(nodes_count), length(types))
  testthat::expect_equal(nodes_count$`0`, types[[1]])
  testthat::expect_equal(nodes_count$`1`, types[[2]])
})

test_that("io_tidygraph", {
  # tidygraph wraps around igraph so use igraph's functions here. Perhaps change
  # to tidygraph's network properties at a later stage.

  py_g <- to_py_graph(motifr::tidygraph_dummy_net, "sesType")
  testthat::expect_false(nx$is_directed(py_g))

  testthat::expect_equal(
    igraph::gorder(motifr::tidygraph_dummy_net),
    nx$number_of_nodes(py_g)
  )
  testthat::expect_equal(
    igraph::gsize(motifr::tidygraph_dummy_net),
    nx$number_of_edges(py_g)
  )
  nodes_count <- sma$nodesCount(py_g)
  types <- table(igraph::V(motifr::tidygraph_dummy_net)$sesType)
  testthat::expect_equal(length(nodes_count), length(types))
  testthat::expect_equal(nodes_count$`0`, types[[1]])
  testthat::expect_equal(nodes_count$`1`, types[[2]])
})


test_that("is_directed", {
  testthat::expect_equal(motifr::is.directed(dummy_net), FALSE)
  testthat::expect_equal(motifr::is.directed(directed_dummy_net), TRUE)
  testthat::expect_equal(motifr::is.directed(tidygraph_dummy_net), FALSE)
  testthat::expect_equal(motifr::is.directed(ml_net), FALSE)
})

test_that("induced_level_subgraph", {
  net <- motifr::dummy_net
  py_g <- motifr::to_py_graph(net, "sesType")
  nodes_count <- sma$nodesCount(py_g)
  edges_count <- sma$edgesCountMatrix(py_g)
  for (level in 1:3) {
    # on every level
    subgraph <- motifr::induced_level_subgraph(net, level - 1)
    testthat::expect_equal(nodes_count[[level]], network::network.size(subgraph))
    py_subgraph <- motifr::to_py_graph(subgraph, "sesType")
    edges_count_subgraph <- sma$edgesCountMatrix(py_subgraph, nTypes = 3L)
    testthat::expect_equal(sum(edges_count_subgraph > 0), 1)
    testthat::expect_equal(edges_count_subgraph[level, level],
                           edges_count[level, level])
  }
})
