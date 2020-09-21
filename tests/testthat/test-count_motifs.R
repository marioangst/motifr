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
