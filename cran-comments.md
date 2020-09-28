## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
> On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking examples ... ERROR
  Running examples in 'motifr-Ex.R' failed
  The error most likely occurred in:
  
  > base::assign(".ptime", proc.time(), pos = "CheckExEnv")
  > ### Title: Compare motif occurence in empirical network to occurence in a
  > ###   baseline model
  > ### Aliases: compare_to_baseline
  > 
  > 
  > ### ** Examples
  > ### Name: compare_to_baseline
  > compare_to_baseline(ml_net, list("1,2[I.C]", "1,2[II.C]"), directed = FALSE)
  Use reticulate::install_miniconda() if you'd like to install a Miniconda Python environment.
  Execution halted
  Error: Installation of Python not found, Python bindings not loaded.

> On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking tests ...
    Running 'testthat.R'
   ERROR
  Running the tests in 'tests/testthat.R' failed.
  Last 13 lines of output:
    [ OK: 6 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 17 ]
    1. Error: io_undirected_dummy_net (@test-core.R#2) 
    2. Error: io_undirected_dummy_net_issue#27 (@test-core.R#32) 
    == testthat results  ===========================================================
    3. Error: io_directed_ml_net (@test-core.R#39) 
    5. Error: io_tidygraph (@test-core.R#81) 
    4. Error: io_directed_igraph (@test-core.R#59) 
    6. Error: induced_level_subgraph (@test-core.R#109) 
    7. Error: count_directed_motifs_2 (@test-count_motifs.R#3) 
    8. Error: count_directed_motifs_0_2 (@test-count_motifs.R#16) 
    9. Error: count_directed_motifs_1_1 (@test-count_motifs.R#29) 
    1. ...
    Error: testthat unit tests failed
    
    Execution halted

> On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking re-building of vignette outputs ... WARNING
  Error(s) in re-building vignettes:
    ...
  --- re-building 'motif_zoo.Rmd' using rmarkdown
  Quitting from lines 50-51 (motif_zoo.Rmd) 
  Error: processing vignette 'motif_zoo.Rmd' failed with diagnostics:
  Installation of Python not found, Python bindings not loaded.
  Use reticulate::install_miniconda() if you'd like to install a Miniconda Python environment.
  --- failed re-building 'motif_zoo.Rmd'
  
  --- re-building 'random_baselines.Rmd' using rmarkdown
  Quitting from lines 49-50 (random_baselines.Rmd) 
  Error: processing vignette 'random_baselines.Rmd' failed with diagnostics:
  Installation of Python not found, Python bindings not loaded.
  Use reticulate::install_miniconda() if you'd like to install a Miniconda Python environment.
  --- failed re-building 'random_baselines.Rmd'
  
  SUMMARY: processing the following files failed:
    'motif_zoo.Rmd' 'random_baselines.Rmd'
  
  Error: Vignette re-building failed.
  Execution halted

> On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Mario Angst <mario.angst@wsl.ch>'
  
  New submission
  
  Possibly mis-spelled words in DESCRIPTION:
    Seppelt (24:54)
    motifr (19:50)
    subgraphs (19:16)

> On ubuntu-gcc-release (r-release)
  checking for future file timestamps ... NOTE
  unable to verify current time

2 errors x | 1 warning x | 2 notes x
