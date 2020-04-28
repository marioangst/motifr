
test_that("error is given for gap test in open motif", {
  expect_error(identify_gaps(ml_net, motif = "1,2[I.C]"))
})
