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

test_that("count_directed_motifs_2", {
  # see https://gitlab.com/t.seppelt/sesmotifanalyser/-/blob/master/test/sma_test.py
  df <- motifr::count_motifs(directed_dummy_net,
    motifs = "2[1]",
    omit_total_result = FALSE
  )
  motifs <- c(
    "2[0]", "2[1]", "2[2]"
  )
  counts <- c(0, 2, 1)
  expected <- data.frame(motif = motifs, count = counts, row.names = motifs)
  testthat::expect_identical(df, expected)
})
test_that("count_directed_motifs_0_2", {
  # see https://gitlab.com/t.seppelt/sesmotifanalyser/-/blob/master/test/sma_test.py
  df <- motifr::count_motifs(directed_dummy_net,
    motifs = "0,2[1]",
    omit_total_result = FALSE
  )
  motifs <- c(
    "0,2[0]", "0,2[1]", "0,2[2]"
  )
  counts <- c(1, 2, 0)
  expected <- data.frame(motif = motifs, count = counts, row.names = motifs)
  testthat::expect_identical(df, expected)
})
test_that("count_directed_motifs_1_1", {
  # see https://gitlab.com/t.seppelt/sesmotifanalyser/-/blob/master/test/sma_test.py
  df <- motifr::count_motifs(directed_dummy_net,
    motifs = "1,1[1]",
    omit_total_result = FALSE
  )
  motifs <- c(
    "1,1[0]", "1,1[1]", "1,1[2]", "1,1[3]"
  )
  counts <- c(3, 1, 3, 2)
  expected <- data.frame(motif = motifs, count = counts, row.names = motifs)
  testthat::expect_identical(df, expected)
})


test_that("count_motifs_1_2", {
  # see https://gitlab.com/t.seppelt/sesmotifanalyser/-/blob/master/test/sma_test.py
  df <- motifr::count_motifs(dummy_net,
    motifs = list("1:0,2:2[I.A]"),
    omit_total_result = FALSE
  )
  motifs <- c(
    "1:0,2:2[I.A]", "1:0,2:2[I.B]", "1:0,2:2[I.C]", "1:0,2:2[II.A]",
    "1:0,2:2[II.B]", "1:0,2:2[II.C]"
  )
  counts <- c(49, 433, 1118, 13, 143, 344)
  expected <- data.frame(motif = motifs, count = counts, row.names = motifs)
  testthat::expect_identical(df, expected)
})
test_that("count_motifs_multiple", {
  # see https://gitlab.com/t.seppelt/sesmotifanalyser/-/blob/master/test/sma_test.py
  motifs <- c("2:1,2:2[I.A]", "2:1,2:2[IV.D]", "2:2,2:0[V.B]", "2:2,2:0[VI.A]")
  df <- motifr::count_motifs(dummy_net, motifs = motifs)
  counts <- c(901, 16, 998, 35)
  expected <- data.frame(motif = motifs, count = counts, row.names = motifs)
  testthat::expect_identical(df, expected)
})

test_that("simulate_ergm", {
  library(ergm)
  ergm_model <- ergm(dummy_net ~ density)
  n <- 10
  df <- simulate_baseline(dummy_net, list("1[1]", "0,1[1]", "0,0,1[1]"), model = "ergm", ergm_model = ergm_model, n = n)
  testthat::expect_equal(dim(df)[[1]], n)
  testthat::expect_true(all(df$`1[1]` == 20))
  testthat::expect_true(all(df$`0,1[1]` == 25))
  testthat::expect_true(all(df$`0,0,1[1]` == 15))
})
