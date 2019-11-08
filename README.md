
<!-- README.md is generated from README.Rmd. Please edit that file -->

# integrater

<!-- badges: start -->

<!-- badges: end -->

The goal of integrater is to …

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

## Installation

Not at this stage yet.

## Example

Recoding:

``` r
library(integrater)
## basic example code
```

``` r
animal_codebook <- data.frame(
  animal = c("elephant", "pig", "lizard"),
  family = c("mammal","mammal","reptile"))
vec <- sample(c("elephant", "pig", "lizard"), 50, TRUE)
vec
#>  [1] "elephant" "pig"      "pig"      "elephant" "elephant" "pig"     
#>  [7] "lizard"   "pig"      "pig"      "lizard"   "pig"      "lizard"  
#> [13] "elephant" "pig"      "lizard"   "elephant" "lizard"   "lizard"  
#> [19] "lizard"   "pig"      "elephant" "elephant" "elephant" "lizard"  
#> [25] "pig"      "elephant" "pig"      "pig"      "pig"      "pig"     
#> [31] "elephant" "lizard"   "pig"      "elephant" "pig"      "elephant"
#> [37] "elephant" "lizard"   "lizard"   "elephant" "pig"      "elephant"
#> [43] "lizard"   "lizard"   "elephant" "lizard"   "elephant" "pig"     
#> [49] "pig"      "elephant"
```

Let’s recode this:

``` r
rename_based_on_codebook(input = vec,
                         codebook = animal_codebook,
                         rawvar = "animal",
                         codevar = "family")
#>  elephant       pig       pig  elephant  elephant       pig    lizard 
#>  "mammal"  "mammal"  "mammal"  "mammal"  "mammal"  "mammal" "reptile" 
#>       pig       pig    lizard       pig    lizard  elephant       pig 
#>  "mammal"  "mammal" "reptile"  "mammal" "reptile"  "mammal"  "mammal" 
#>    lizard  elephant    lizard    lizard    lizard       pig  elephant 
#> "reptile"  "mammal" "reptile" "reptile" "reptile"  "mammal"  "mammal" 
#>  elephant  elephant    lizard       pig  elephant       pig       pig 
#>  "mammal"  "mammal" "reptile"  "mammal"  "mammal"  "mammal"  "mammal" 
#>       pig       pig  elephant    lizard       pig  elephant       pig 
#>  "mammal"  "mammal"  "mammal" "reptile"  "mammal"  "mammal"  "mammal" 
#>  elephant  elephant    lizard    lizard  elephant       pig  elephant 
#>  "mammal"  "mammal" "reptile" "reptile"  "mammal"  "mammal"  "mammal" 
#>    lizard    lizard  elephant    lizard  elephant       pig       pig 
#> "reptile" "reptile"  "mammal" "reptile"  "mammal"  "mammal"  "mammal" 
#>  elephant 
#>  "mammal"
```
