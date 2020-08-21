test_that("error is given for gap test in open motif", {
  expect_error(identify_gaps(ml_net, motif = "1,2[I.C]"))
})
test_that("error is given for gap test in closed motif", {
  expect_error(critical_dyads(ml_net, motif = "1,2[II.C]"))
})
