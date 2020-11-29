# Note on Pre-Build Vignettes

This directory contains the vignettes for motifr. Files ``*.Rmd.orig`` contain
the "real" Rmd vignettes. Using [``precompile.R``](./precompile.R) they have to 
be pre-compiled to ``*.Rmd`` files. This is done to circumvene problems with Python
on CRAN.

Please edit **only** the ``*.Rmd.orig`` files. All other files in this directory
are derivates of them.

For details see https://www.r-bloggers.com/2020/06/optimal-workflows-for-package-vignettes/
