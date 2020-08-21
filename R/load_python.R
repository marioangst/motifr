# this accesses python functions in the sesmotifs submodule. see eg. - I chose
# Version 1 here because sesmotifanalyzer functions do not need to be accessible
# at top-level

# github.com/rstudio/reticulate/issues/233 (delay loading till use)

# https://community.rstudio.com/t/build-package-namespace-dynamically-during-onload/4101
# https://mran.microsoft.com/snapshot/2017-08-06/web/packages/reticulate/vignettes/package.html

# global reference in order to prevent Python modules in user data space
itertools <- NULL
nx <- NULL
sma <- NULL

.onLoad <- function(libname, pkgname) {
  # delay load nx module (will only be loaded whlen accessed via $)
  nx <<- reticulate::import("networkx", delay_load = TRUE)
  itertools <<- reticulate::import("itertools", delay_load = TRUE)
  sma <<- reticulate::import("sma", delay_load = TRUE)
}
