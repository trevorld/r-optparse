optparse
========

A command line parser inspired by Python's 'optparse' library to
be used with Rscript to write "#!" shebang scripts that accept short and
long flag/options.

To install the development version use::

    devtools::install_github("optparse", "trevorld")

dependencies
============

This package depends on the R packages ``getopt``.

To run the unit tests you will need the suggested R package ``testthat`` and in
order to build the vignette you will need the suggested R package ``knitr``
(and ``knitr`` probably requires the system tool ``rst2pdf``).
