


# this accesses python functions in the sesmotifs submodule. see eg. - I chose Version 1 here because
# sesmotifanalyzer functions do not need to be accessible at top-level
# https://community.rstudio.com/t/build-package-namespace-dynamically-during-onload/4101
# https://mran.microsoft.com/snapshot/2017-08-06/web/packages/reticulate/vignettes/package.html

sma <- NULL
.onLoad <- function(libname, pkgname){
  sma <<- reticulate::import_from_path(module = 'sma',
                                       path = system.file("python",
                                                          'sesmotifanalyser',
                                                          package = packageName(),
                                                          mustWork = TRUE))
}
