**Test environments**

* local (linux), R 4.3.2
* win-builder (windows), R devel
* Github Actions (linux), R devel and R release
* Github Actions (OSX), R release
* Github Actions (windows), R release

**R CMD check --as-cran results**

Status: OK

**revdepcheck results**

## revdepcheck results

We checked 13 reverse dependencies (9 from CRAN + 4 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

**Nota benes**

* As in previous uploads while in a non-interactive session (i.e. in an
  Rscript) if `parse_args()` observes a help flag it will print a usage
  message and then call ``quit()`` unless the argument ``print_help_and_exit``
  has been set ``FALSE``.  
