motifr <img src='man/figures/logo.png' align="right" height="139" />
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- Thanks for this: https://r-pkgs.org/ -->

<!-- Must make a hex sticker at some point: https://cran.r-project.org/web/packages/hexSticker/readme/README.html -->

<!-- For packages that depend on python stuff, useful updates here: -->

<!-- https://blog.rstudio.com/2019/12/20/reticulate-1-14/ -->

<!-- "To that end, we’ve made the following changes. If the user has not explicitly instructed reticulate to use a pre-existing Python environment, then: -->

<!-- 1) reticulate will prompt the user to download and install Miniconda; -->

<!-- 2) reticulate will prepare a default r-reticulate Conda environment, using (currently) Python 3.6 and NumPy; -->

<!-- 3) When Python is initialized, reticulate will query any loaded R packages for their Python dependencies, and install those dependencies into the aforementioned r-reticulate Conda environment." -->

<!-- badges: start -->

<!-- badges: end -->

This package provides tools to analyze multi-level networks in terms of
*motifs*.

Multi-level networks combine multiple networks in one representation,
e.g. social-ecological networks, which connect a social network (eg.
interactions among fishermen) with an ecological network (eg.
interactions between fish species) and the ties inbetween (eg. fishers
who fish specific species).

[Motifs](https://en.wikipedia.org/wiki/Network_motif) are small
configurations of nodes and edges (subgraphs) within an overall network.

Package features include:

  - Visualization: The package provides functions to visualize
    multi-level networks, based on
    [ggraph](https://github.com/thomasp85/ggraph).

  - Motif counts: The package is in many parts a R wrapper for the
    excellent
    [sesmotifanalyser](https://gitlab.com/t.seppelt/sesmotifanalyser)
    Python framework written by Tim Seppelt to count multi-level network
    motifs, compare them to a baseline and much more. Only parts of of
    sesmotifanalyser are yet wrapped, so consult the python framework
    for additional functionality.

  - Contributions of edges to motifs: motifr further identifies and
    visualizes functional gaps and critical edges in multi-level
    networks based on contributions of existing or potential edges to
    given motifs (this is theoretically based on network theories of
    functional fit and misfit).

## Installation

The package is currently at an early stage of development. Explore at
your own risk and please report any issues using the [issue tracker on
github](https://github.com/marioangst/motifr/issues). You can install
the package from github, using devtools:

``` r
devtools::install_github("marioangst/motifr")
```

## Input

motifr currently can only reliably handle undirected and unweighted
networks. There is in principle no restriction on the number of levels
of a network to count motifs in, but typical use cases for which the
package was designed will likely involve two- or three-level networks.

Network data should currently be prepared as statnet network objects
with a numeric vertex attribute to specify a level for each node (named
eg. “lvl”) for best results.

## Examples

``` r
library(motifr)
```

### Visualize a multi-level network

The following network is an example network from an empirical analysis
of wetlands management in Switzerland. It consists of two levels - one
level specifies a network of relations between actors. A second level
specifies a network of relations between different activities occurring
in the wetland, based on causal interdependence among activites. Links
between the levels specify which actors carry out which activities.

It is possible to specify layouts for every network level separately.
Below, one level is plotted based on a circle layout, the second one
based on Kamada-Kawai.

``` r
plot_mnet(net = ml_net,
          lvl_attr = "sesType",
          layouts = list("kk","circle"))
```

<img src="man/figures/README-unnamed-chunk-4-1.svg" width="100%" />

### Count motifs

Motifs can be counted using the versatile function `count_motifs()`. It
takes as parameters a statnet network object (use `ml_net` or
`dummy_net` provided by this package as example) and a list of motif
identifiers (see below) specifying the motifs. See the [vignette on the
motif zoo](https://marioangst.github.io/motifr/articles/motif_zoo.html)
for details on nomenclature for motifs (motif identifier strings). Let’s
quickly check out two classic examples of three-node, two-level motifs
(open and closed triangles) in the wetlands management network
introduced above:

``` r
show_motif(motif = '1,2[I.C]', net = ml_net, label = TRUE) # open ('1,2[I.C]') triangle
```

<img src="man/figures/README-unnamed-chunk-5-1.svg" width="300px" />

``` r
show_motif(motif = '1,2[II.C]', net = ml_net, label = TRUE) # closed ('1,2[II.C]') triangle
```

<img src="man/figures/README-unnamed-chunk-5-2.svg" width="300px" />

Let’s count the number of of these motifs in the entire network.

``` r
motifs = list('1,2[I.C]', '1,2[II.C]') # open and closed triangle

count_motifs(ml_net, motifs)
#> # A tibble: 2 x 2
#>   motif     count
#>   <chr>     <dbl>
#> 1 1,2[I.C]    543
#> 2 1,2[II.C]   167
```

An exploratory approach can be taken by calling `motif_summary()`. This
function counts the occurrences of a couple of interesting motifs.
Furthermore it computes expectations and variances for the occurrence of
these motifs in a modified Erdős-Rényi model. See the package [vignette
on random
baselines](https://marioangst.github.io/motifr/articles/random_baselines.html)
for details.

``` r
motif_summary(ml_net)
#>        motif count  expectation  variance
#> 1   1,2[I.C]   543 169.14423077 949.77429
#> 2  1,2[II.C]   167  16.96153846  25.69287
#> 3   2,1[I.C]   217 109.90569527 437.59758
#> 4  2,1[II.C]     7  10.23853550  13.91309
#> 5 2,2[III.C]    73   0.44811771       NaN
#> 6 2,2[III.D]     1   0.04174551       NaN
```

### Identify gaps and critical edges

motifr makes it possible to identify gaps and critical edges in
multi-level networks. This is motivated by theories of functional fit
and misfit in networks, which posit that certain motifs are especially
valuable for network outcomes (depending on the context).

In relation to gaps, we can therefore try to identify potential edges
that would create a large number of a given motif if they were to exist
(“activated” or “flipped”). The number of such motifs created by an edge
is their contribution. For example, we can get all edges that would
create closed triangles (‘1,2\[II.C\]’), including the information about
how many such triangles they would create for the wetlands case study
network:

``` r
gaps <- identify_gaps(ml_net, motif = '1,2[II.C]')
head(gaps)
#>   vertex0 vertex1 contribution
#> 1 actor10 actor27            5
#> 2 actor18 actor44            5
#> 3  actor6 actor24            4
#> 4 actor16 actor55            4
#> 5 actor18 actor27            4
#> 6 actor18 actor31            4
```

We can also plot these gaps in our network, including option to only
look at gaps above a certain weight (contribution) and different levels
of focus to only show nodes involved in such gaps. Here again for the
wetlands management network, only showing gaps with a weight above 5.

``` r
plot_gaps(ml_net, 
          "1,2[II.C]", 
          level = -1, 
          subset_graph = "partial", 
          cutoff = 5, label = TRUE)
```

<img src="man/figures/README-unnamed-chunk-9-1.svg" width="100%" />

`identify_gaps` has a sibling in `critical_dyads`. Critical\_dyads works
in reverse to identifying gaps - it analyzes for every existing edge how
many instances of a given motif would disappear if the edge would be
removed.

### Comparing motif occurrence to a random baseline

Motifr can be used to simulate a random baseline of networks. Motif
counts in an empirical network can then be compared to the distribution
of motif counts in the random networks. We do so again here for open and
closed triangles in the wetland management network. Unsurprisingly, we
find that both of these motifs occur much more often in the empirically
observed network than in the random baseline. See the package [vignette
on random baselines](vignettes/random_baselines.Rmd) for details.

``` r
motifs = list('1,2[I.C]', '1,2[II.C]') # open ('1,2[I.C]') and closed ('1,2[II.C]') triangles

compare_to_baseline(ml_net, motifs = motifs, n = 100)
```

<img src="man/figures/README-unnamed-chunk-10-1.svg" width="100%" />
