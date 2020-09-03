library(testthat)
library(motifr)
library(reticulate)
library(igraph)

nx <<- reticulate::import("networkx", delay_load = TRUE)
itertools <<- reticulate::import("itertools", delay_load = TRUE)
sma <<- reticulate::import("sma", delay_load = TRUE)

test_check("motifr")
