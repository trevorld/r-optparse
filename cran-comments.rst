**Test environments**

* local (linux), R 4.1.1
* win-builder (windows), R devel
* Github Actions (linux), R devel and R release
* Github Actions (OSX), R release
* Github Actions (windows), R release

**R CMD check --as-cran results**

Status: OK

**revdepcheck results**

We checked 14 reverse dependencies (7 from CRAN + 7 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

**Nota benes**

* Currently CRAN checks on the two OSX test machines for the previous release
  of ``optparse`` show a WARNING because they are unable to re-build the
  vignette due to a weird error::
     
      sh: +RTS: command not found
      Warning in system(command) : error in running command
      Error: processing vignette 'optparse.Rrst' failed with diagnostics:
      pandoc document conversion failed with error 127
      Execution halted

  Looking online it seems ``error 127`` is an issue with a virtual machine not
  having enough memory.  Perhaps ``--no-vignettes`` flag should be turned on
  for OSX test machines?  I am unable to reproduce this error in my Github Actions
  OSX test environment (i.e. the vignette always builds fine).

* As in previous uploads while in a non-interactive session (i.e. in an
  Rscript) if `parse_args` observes a help flag it will print a usage
  message and then call ``quit`` unless the argument ``print_help_and_exit``
  has been set ``FALSE``.  
