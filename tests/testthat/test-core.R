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
test_that("io_directed_ml_net", {
  py_g <- to_py_graph(motifr::ml_net, "sesType")
  testthat::expect_true(nx$is_directed(py_g))

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
})
