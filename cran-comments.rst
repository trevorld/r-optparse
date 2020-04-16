**Test environments**

* win-builder (windows), R devel
* appveyor (windows), R release and R devel
* local (linux), R 3.6.3
* travis-ci (linux), R release and R devel
* travis-ci (OSX), R release

**R CMD check --as-cran results**

Status: OK

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
  for OSX test machines?  I am unable to reproduce this error in my Travis-CI
  OSX test environment (i.e. the vignette always builds fine).

* As in previous uploads while in a non-interactive session (i.e. in an
  Rscript) if `parse_args` observes a help flag it will print a usage
  message and then call ``quit`` unless the argument ``print_help_and_exit``
  has been set ``FALSE``.  
