For a CRAN submission we recommend that you fix all NOTEs, WARNINGs and ERRORs.
## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
> On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Mario Angst <mario.angst@gmail.com>'
  New submission
  
  Possibly mis-spelled words in DESCRIPTION:
    Seppelt (24:54)
    motifr (19:50)
    subgraphs (19:16)

> On ubuntu-gcc-release (r-release)
  checking examples ... NOTE
  Examples with CPU or elapsed time > 5s
             user system elapsed
  dummy_net 4.039  0.174   5.882

0 errors ✓ | 0 warnings ✓ | 2 notes x
