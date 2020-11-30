skip_if_no_sma <- function() {
  if (!reticulate::py_module_available("sma")) {
    skip("sma not available for testing")
  }
}
test_that("error is given for gap test in open motif", {
  skip_if_no_sma()
  expect_error(identify_gaps(ml_net, motif = "1,2[I.C]"))
})
test_that("error is given for gap test in closed motif", {
  skip_if_no_sma()
  expect_error(critical_dyads(ml_net, motif = "1,2[II.C]"))
})
