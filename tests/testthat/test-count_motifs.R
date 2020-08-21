test_that("count_motifs_1_2", {
  df <- count_motifs(dummy_net, motifs = list('1:0,2:2[I.A]'), omit_total_result = FALSE)
  motifs <- c('1:0,2:2[I.A]', '1:0,2:2[I.B]', '1:0,2:2[I.C]', '1:0,2:2[II.A]', '1:0,2:2[II.B]', '1:0,2:2[II.C]')
  counts <- c(49,433,1118,13,143,344)
  expected <- data.frame(motif = motifs, count = counts, row.names = motifs)
  expect_identical(df, expected)
})
test_that("count_motifs_multiple", {
  motifs <- c('2:1,2:2[I.A]', '2:1,2:2[IV.D]', '2:2,2:0[V.B]', '2:2,2:0[VI.A]')
  df <- count_motifs(dummy_net, motifs = motifs)
  counts <- c(901, 16, 998, 35)
  expected <- data.frame(motif = motifs, count = counts, row.names = motifs)
  expect_identical(df, expected)
})
