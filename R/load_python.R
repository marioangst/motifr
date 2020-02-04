# this accesses python functions in the sesmotifs submodule. see eg. - I chose Version 1 here because
# sesmotifanalyzer functions do not need to be accessible at top-level

#github.com/rstudio/reticulate/issues/233 (delay loading till use)

#' Load submodule sma
#'
#' @return
#' @export
#'
#' @examples
load_python_sma <- function(){
  sma <<- reticulate::import_from_path(module = "sma", path = system.file("python",
                                                                          "sesmotifanalyser",
                                                                          package = utils::packageName()))
}


# https://community.rstudio.com/t/build-package-namespace-dynamically-during-onload/4101
# https://mran.microsoft.com/snapshot/2017-08-06/web/packages/reticulate/vignettes/package.html

nx <- NULL

.onLoad <- function(libname, pkgname) {
  # make sure networkx v2.3 is loaded (problems exist with 2.4 at the moment)
  # reticulate::py_install("networkx==2.3", pip = TRUE)
  # delay load nx module (will only be loaded whlen accessed via $)
  nx <<- reticulate::import("networkx", delay_load = TRUE)
  itertools <<- reticulate::import("itertools", delay_load = TRUE)
}
