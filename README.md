integrateR
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

Thanks for this: <https://r-pkgs.org/>

<!-- badges: start -->

<!-- badges: end -->

Provides tools for integrated network analysis.

  - Visualize: Causal loop diagrams (CLD), Multi-level networks
  - Aggregate: Find dependencies between components of causal loop
    diagrams based on common effects on the rest of the CLD
  - Analyze: Count motifs, identify gaps, simulate baseline networks,
    identify virtuous and vicious circles
  - Visualize results: visualize loops, suggest ties, visualize gaps

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

## Installation

Not at this stage yet.

## Examples

``` r
library(integrateR)
library(DiagrammeR)
```

<!-- Recoding: -->

<!-- ```{r example} -->

<!-- library(integrater) -->

<!-- ## basic example code -->

<!-- ``` -->

<!-- ```{r} -->

<!-- animal_codebook <- data.frame( -->

<!--   animal = c("elephant", "pig", "lizard"), -->

<!--   family = c("mammal","mammal","reptile")) -->

<!-- vec <- sample(c("elephant", "pig", "lizard"), 50, TRUE) -->

<!-- vec -->

<!-- ``` -->

<!-- Let's recode this: -->

<!-- ```{r} -->

<!-- rename_based_on_codebook(input = vec, -->

<!--                          codebook = animal_codebook, -->

<!--                          rawvar = "animal", -->

<!--                          codevar = "family") -->

<!-- ``` -->
