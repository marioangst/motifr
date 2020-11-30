# Pre-Build Vignettes

# In order to circumvene problems with Python dependencies on CRAN, vignettes are
# pre-build following https://ropensci.org/technotes/2019/12/08/precompute-vignettes/.
# The following commands have to be run:

knitr::knit("vignettes/motif_zoo.Rmd.orig", output = "vignettes/motif_zoo.Rmd")
knitr::knit("vignettes/random_baselines.Rmd.orig", output = "vignettes/random_baselines.Rmd")

# copy figures from ./ to vignettes/
current_folder <- "./"
new_folder <- "vignettes/"
list_of_files <- list.files(current_folder, "(motif_zoo|random_baselines)_.*.svg$")
file.rename(from = file.path(current_folder,list_of_files),
            to = file.path(new_folder,list_of_files))
