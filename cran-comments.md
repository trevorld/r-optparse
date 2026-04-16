## Test environments

* local (linux), R 4.5.3
* win-builder (windows), R devel
* Github Actions (linux), R devel and R release

## R CMD check --as-cran results

Status: OK

## revdepcheck results

We checked 24 reverse dependencies (11 from CRAN + 13 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 1 new problems
 * We failed to check 0 packages

Issues with CRAN packages are summarised below.

### New problems

* W4MRUtils

  + They had a couple bugs that got triggered by this update.
  + More than two weeks ago I supplied a PR fixing these
    for both the current CRAN version of {optparse} and this update:
    https://github.com/workflow4metabolomics/W4MRUtils/pull/3

## Nota benes

* As in previous uploads while in a non-interactive session (i.e. in an Rscript) if `parse_args()` observes a help flag it will print a usage message and then call `quit()` unless the argument `print_help_and_exit` has been set `FALSE`.
