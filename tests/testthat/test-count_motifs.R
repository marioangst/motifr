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
test_that("count_motifs_tidygraph_dummy", {
  motifs <- c("1,2[I.A]", "1,2[I.B]", "1,2[I.C]", "1,2[II.A]", "1,2[II.B]", "1,2[II.C]")
  df <- motifr::count_motifs(motifr::tidygraph_dummy_net, motifs = motifs)
  counts <- c(2, 0, 1, 4, 0, 2)
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
test_that("exemplify_motif", {
  # use small networks that have only one motif of a certain type (or none)
  testthat::expect_equal(
    motifr::exemplify_motif(motifr::tidygraph_dummy_net, "1,2[I.C]"),
    c(1, 5, 6)
  )
  # requires sma version 2.1.2
  testthat::expect_null(motifr::exemplify_motif(motifr::tidygraph_dummy_net, "1,2[I.B]"))
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
test_that("simulate_partial_ergm_level1", {
  library(ergm)
  net <- motifr::ml_net
  actors <- motifr::induced_level_subgraph(net, level = 1)
  # very simple model to keep testing costs low
  ergm_model <- ergm(actors ~ density)

  n <- 10
  df <- simulate_baseline(net,
    list(
      "1[1]", "0,1[1]",
      "2[1]", "1,1[1]", # "0,2[1]", is variable
      "1,2[I.A]", "1,2[II.A]",
      "1,2[I.B]", "1,2[II.B]",
      "1,2[I.C]", "1,2[II.C]"
    ),
    model = "partial_ergm",
    ergm_model = ergm_model,
    n = n,
    level = 1
  )
  testthat::expect_equal(dim(df)[[1]], n)
  # test node counts
  testthat::expect_true(all(df$`1[1]` == 52))
  testthat::expect_true(all(df$`0,1[1]` == 80))
  # test edge counts of fixed segments
  testthat::expect_true(all(df$`2[1]` == 113))
  testthat::expect_true(all(df$`1,1[1]` == 140))
  # test edge contribution in triangles
  testthat::expect_true(all(df$`1,2[I.A]` + df$`1,2[II.A]` == 153970))
  testthat::expect_true(all(df$`1,2[I.B]` + df$`1,2[II.B]` == 9640))
  testthat::expect_true(all(df$`1,2[I.C]` + df$`1,2[II.C]` == 710))
})
test_that("simulate_partial_ergm_level0", {
  library(ergm)
  net <- motifr::ml_net
  actors <- motifr::induced_level_subgraph(net, level = 0)
  # very simple model to keep testing costs low
  ergm_model <- ergm(actors ~ density)

  n <- 10
  df <- simulate_baseline(net,
    list(
      "1[1]", "0,1[1]",
      "0,2[1]", "1,1[1]", # "2[1]", is variable
      "2,1[I.A]", "2,1[II.A]",
      "2,1[I.B]", "2,1[II.B]",
      "2,1[I.C]", "2,1[II.C]"
    ),
    model = "partial_ergm",
    ergm_model = ergm_model,
    n = n,
    level = 0
  )
  testthat::expect_equal(dim(df)[[1]], n)
  # test node counts
  testthat::expect_true(all(df$`1[1]` == 52))
  testthat::expect_true(all(df$`0,1[1]` == 80))
  # test edge counts of fixed segments
  testthat::expect_true(all(df$`0,2[1]` == 288))
  testthat::expect_true(all(df$`1,1[1]` == 140))
  # test edge contribution in triangles
  testthat::expect_true(all(df$`2,1[I.A]` + df$`2,1[II.A]` == 99164))
  testthat::expect_true(all(df$`2,1[I.B]` + df$`2,1[II.B]` == 6692))
  testthat::expect_true(all(df$`2,1[I.C]` + df$`2,1[II.C]` == 224))
})
