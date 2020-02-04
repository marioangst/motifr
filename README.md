integrateR
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

This package provides tools to analyze and visualize multi-level
networks in general, and so-called integrated networks specifically.

Multi-level networks combine multiple networks in one,
e.g. social-ecological networks. Integrated networks are specific
networks combining causal loop diagrams (CLDs) and social networks.

The package has three main areas: Visualization, Aggregation and
Analysis.

  - Visualization: The package visualizes CLDs and two-level networks.

  - Aggregation: The package implements ways to aggregate CLDs to use as
    inputs in multi-level networks by way of finding common causes of
    parts of a CLD.

  - Analysis: The package is in many parts a R wrapper for the excellent
    [sesmotifanalyser](https://gitlab.com/t.seppelt/sesmotifanalyser)
    Python framework written by Tim Seppelt to count multi-level network
    motifs, compare them to a baseline and much more. It further
    identifies and visualizes functional gaps in integrated networks and
    virtuous and vicious cycles in CLDs.

## Installation

The package is currently at a very early stage of development. Explore
at your own risk and please report any issues using the [issue tracker
on github](https://github.com/marioangst/integrateR/issues). You can
install the package from github, using devtools:

``` r
devtools::install_github("marioangst/integrater")
```

## Examples

``` r
library(integrateR)
```

Visualize a causal loop diagram, based on DiagrammeR. This is based on
an expert interview conducted in a Swiss wetland.

<!--html_preserve-->

<div id="htmlwidget-62b88c7f996f13ceab1b" class="grViz html-widget" style="width:100%;height:480px;">

</div>

<script type="application/json" data-for="htmlwidget-62b88c7f996f13ceab1b">{"x":{"diagram":"digraph {\n\ngraph [layout = \"dot\",\n       outputorder = \"edgesfirst\",\n       bgcolor = \"white\",\n       rankdir = \"LR\",\n       ranksep = \"2  equally\",\n       outputorder = \"edgesfirst\",\n       pad = \"3\"]\n\nnode [fontname = \"Helvetica\",\n      fontsize = \"10\",\n      shape = \"circle\",\n      fixedsize = \"true\",\n      width = \"0.5\",\n      style = \"filled\",\n      fillcolor = \"aliceblue\",\n      color = \"gray70\",\n      fontcolor = \"gray50\",\n      fontsize = \"14\"]\n\nedge [fontname = \"Helvetica\",\n     fontsize = \"8\",\n     len = \"1.5\",\n     color = \"gray80\",\n     arrowsize = \"0.5\",\n     arrowsize = \"0.5\"]\n\n  \"1\" [label = \"knowlegde correct upkeep\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"2\" [label = \"pressure to act due to flood risks\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"3\" [label = \"market for litter\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"4\" [label = \"fish passes\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"5\" [label = \"planing spatial developement\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"6\" [label = \"recreational value\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"7\" [label = \"competition for space with agriculture\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"8\" [label = \"invasive species\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"9\" [label = \"space for river\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"10\" [label = \"habitat quality\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"11\" [label = \"damage to infrastructure by beaver activities\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"12\" [label = \"local population\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"13\" [label = \"species protection (primary rare)\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"14\" [label = \"political support\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"15\" [label = \"bog conservation\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"16\" [label = \"wwtp operation\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"17\" [label = \"fishing\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"18\" [label = \"ranger service\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"19\" [label = \"awareness for nature protection issues in population\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"20\" [label = \"operation of hydropower plant\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"21\" [label = \"flood protection\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"22\" [label = \"ecological ugrading of gravel pits\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"23\" [label = \"farming\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"24\" [label = \"planning of motorway feed\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"25\" [label = \"improve, connect and extensify outside protected area\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"26\" [label = \"climate change\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"27\" [label = \"fish migration\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"28\" [label = \"maintenance compensation areas\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"29\" [label = \"compensation areas\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"30\" [label = \"drinking water wells\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"31\" [label = \"nutrient input\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"32\" [label = \"river dynamics\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"33\" [label = \"planning new cycle paths\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"34\" [label = \"revitalisation of watercourse\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"35\" [label = \"impact of floods\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"36\" [label = \"cooperation among municipalities\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"37\" [label = \"flood protection measures (business as usual)\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"38\" [label = \"biodiversity protection projects\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"39\" [label = \"signalisation for entry\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"40\" [label = \"damage by boars and deers\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"41\" [label = \"intercantonal coordination flood protection\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"42\" [label = \"herbicide and pesticide\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"43\" [label = \"resources for upkeep\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"44\" [label = \"water quality (wastewater)\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"45\" [label = \"interested individuals\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"46\" [label = \"water withdrawal\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"47\" [label = \"extensified agriculture\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"48\" [label = \"popularity\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"49\" [label = \"beaver management\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"50\" [label = \"visitor pressure\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"51\" [label = \"boar and deer managment\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n  \"52\" [label = \"environmental education (zieglerhaus)\", shape = \"circle\", fontcolor = \"black\", fillcolor = \"#f4b24150\"] \n\"21\"->\"5\" [color = \"#ffb3b3\"] \n\"23\"->\"29\" [color = \"#ffb3b3\"] \n\"29\"->\"7\" [color = \"#809fff\"] \n\"4\"->\"27\" [color = \"#809fff\"] \n\"35\"->\"13\" [color = \"#ffb3b3\"] \n\"36\"->\"30\" [color = \"#ffb3b3\"] \n\"21\"->\"2\" [color = \"#ffb3b3\"] \n\"23\"->\"47\" [color = \"#ffb3b3\"] \n\"35\"->\"31\" [color = \"#809fff\"] \n\"10\"->\"6\" [color = \"#809fff\"] \n\"36\"->\"34\" [color = \"#809fff\"] \n\"36\"->\"41\" [color = \"#809fff\"] \n\"11\"->\"10\" [color = \"#809fff\"] \n\"20\"->\"32\" [color = \"#ffb3b3\"] \n\"34\"->\"9\" [color = \"#809fff\"] \n\"38\"->\"10\" [color = \"#809fff\"] \n\"7\"->\"21\" [color = \"#ffb3b3\"] \n\"10\"->\"25\" [color = \"#809fff\"] \n\"47\"->\"43\" [color = \"#ffb3b3\"] \n\"9\"->\"13\" [color = \"#809fff\"] \n\"35\"->\"44\" [color = \"#ffb3b3\"] \n\"37\"->\"9\" [color = \"#ffb3b3\"] \n\"1\"->\"43\" [color = \"#809fff\"] \n\"3\"->\"43\" [color = \"#809fff\"] \n\"7\"->\"9\" [color = \"#ffb3b3\"] \n\"18\"->\"19\" [color = \"#809fff\"] \n\"9\"->\"32\" [color = \"#809fff\"] \n\"31\"->\"25\" [color = \"#ffb3b3\"] \n\"1\"->\"46\" [color = \"#ffb3b3\"] \n\"42\"->\"25\" [color = \"#ffb3b3\"] \n\"23\"->\"31\" [color = \"#809fff\"] \n\"26\"->\"8\" [color = \"#809fff\"] \n\"41\"->\"21\" [color = \"#809fff\"] \n\"12\"->\"50\" [color = \"#809fff\"] \n\"18\"->\"8\" [color = \"#ffb3b3\"] \n\"40\"->\"25\" [color = \"#ffb3b3\"] \n\"17\"->\"45\" [color = \"#809fff\"] \n\"50\"->\"10\" [color = \"#ffb3b3\"] \n\"26\"->\"46\" [color = \"#809fff\"] \n\"9\"->\"6\" [color = \"#809fff\"] \n\"24\"->\"29\" [color = \"#809fff\"] \n\"16\"->\"29\" [color = \"#ffb3b3\"] \n\"45\"->\"19\" [color = \"#809fff\"] \n\"2\"->\"34\" [color = \"gray80\"] \n\"19\"->\"14\" [color = \"#809fff\"] \n\"34\"->\"35\" [color = \"#ffb3b3\"] \n\"10\"->\"13\" [color = \"#809fff\"] \n\"5\"->\"7\" [color = \"gray80\"] \n\"26\"->\"2\" [color = \"#809fff\"] \n\"36\"->\"39\" [color = \"#809fff\"] \n\"51\"->\"40\" [color = \"#ffb3b3\"] \n\"33\"->\"39\" [color = \"#809fff\"] \n\"14\"->\"38\" [color = \"#809fff\"] \n\"48\"->\"50\" [color = \"#809fff\"] \n\"46\"->\"15\" [color = \"#ffb3b3\"] \n\"8\"->\"13\" [color = \"#ffb3b3\"] \n\"36\"->\"19\" [color = \"#809fff\"] \n\"35\"->\"15\" [color = \"#ffb3b3\"] \n\"30\"->\"46\" [color = \"#809fff\"] \n\"18\"->\"48\" [color = \"#809fff\"] \n\"31\"->\"15\" [color = \"#ffb3b3\"] \n\"35\"->\"16\" [color = \"#ffb3b3\"] \n\"20\"->\"4\" [color = \"#ffb3b3\"] \n\"1\"->\"29\" [color = \"#809fff\"] \n\"43\"->\"15\" [color = \"#809fff\"] \n\"5\"->\"16\" [color = \"gray80\"] \n\"20\"->\"35\" [color = \"#ffb3b3\"] \n\"1\"->\"42\" [color = \"#ffb3b3\"] \n\"7\"->\"25\" [color = \"#ffb3b3\"] \n\"31\"->\"8\" [color = \"gray80\"] \n\"16\"->\"44\" [color = \"#809fff\"] \n\"38\"->\"9\" [color = \"#809fff\"] \n\"32\"->\"13\" [color = \"gray80\"] \n\"18\"->\"50\" [color = \"#ffb3b3\"] \n\"50\"->\"6\" [color = \"#ffb3b3\"] \n\"45\"->\"48\" [color = \"#809fff\"] \n\"48\"->\"14\" [color = \"#809fff\"] \n\"6\"->\"48\" [color = \"#809fff\"] \n\"42\"->\"13\" [color = \"#ffb3b3\"] \n\"19\"->\"25\" [color = \"#809fff\"] \n\"34\"->\"29\" [color = \"#809fff\"] \n\"49\"->\"11\" [color = \"#ffb3b3\"] \n\"40\"->\"13\" [color = \"#ffb3b3\"] \n\"1\"->\"15\" [color = \"#809fff\"] \n\"17\"->\"50\" [color = \"#809fff\"] \n\"24\"->\"7\" [color = \"#809fff\"] \n\"29\"->\"9\" [color = \"#809fff\"] \n\"52\"->\"45\" [color = \"#809fff\"] \n\"47\"->\"42\" [color = \"#ffb3b3\"] \n\"44\"->\"13\" [color = \"#809fff\"] \n\"43\"->\"29\" [color = \"#809fff\"] \n\"35\"->\"21\" [color = \"#ffb3b3\"] \n\"39\"->\"50\" [color = \"#ffb3b3\"] \n\"26\"->\"35\" [color = \"#809fff\"] \n\"15\"->\"30\" [color = \"#ffb3b3\"] \n\"9\"->\"21\" [color = \"#809fff\"] \n\"10\"->\"15\" [color = \"#809fff\"] \n\"11\"->\"21\" [color = \"#ffb3b3\"] \n\"38\"->\"47\" [color = \"#809fff\"] \n\"28\"->\"29\" [color = \"#809fff\"] \n\"50\"->\"15\" [color = \"#ffb3b3\"] \n\"37\"->\"35\" [color = \"#ffb3b3\"] \n\"5\"->\"38\" [color = \"#809fff\"] \n\"37\"->\"7\" [color = \"#ffb3b3\"] \n\"47\"->\"1\" [color = \"#809fff\"] \n\"38\"->\"27\" [color = \"#809fff\"] \n\"5\"->\"12\" [color = \"#809fff\"] \n\"34\"->\"7\" [color = \"#809fff\"] \n\"45\"->\"50\" [color = \"#809fff\"] \n\"34\"->\"32\" [color = \"#809fff\"] \n\"33\"->\"48\" [color = \"#809fff\"] \n\"22\"->\"9\" [color = \"#809fff\"] \n\"27\"->\"13\" [color = \"#ffb3b3\"] \n}","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>

<!--/html_preserve-->
