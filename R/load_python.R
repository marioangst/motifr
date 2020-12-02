
# https://github.com/rstudio/reticulate/issues/233 (delay loading till use)

# https://community.rstudio.com/t/build-package-namespace-dynamically-during-onload/4101
# https://mran.microsoft.com/snapshot/2017-08-06/web/packages/reticulate/vignettes/package.html

# global reference in order to prevent Python modules in user data space
pkg.env <- new.env(parent = emptyenv())
pkg.env$nx <- NULL
pkg.env$sma <- NULL

.onLoad <- function(libname, pkgname) {
  # https://rstudio.github.io/reticulate/articles/python_dependencies.html#onload-configuration
  reticulate::configure_environment(pkgname)
  # delay load Python modules (will only be loaded when accessed via $)
  pkg.env$nx <- reticulate::import("networkx", delay_load = TRUE)
  pkg.env$sma <- reticulate::import("sma", delay_load = TRUE)
}

#' Checks for updates for motifr's Python core, the sma package
#'
#' It might be necessary to restart your R session after updating the sma package.
#'
#' @export
update_motifr <- function() {
  reticulate::py_install("sma", pip = TRUE)
}
