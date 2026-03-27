optparse: Command line optional argument parser
===============================================

.. image:: https://www.r-pkg.org/badges/version/optparse
    :target: https://cran.r-project.org/package=optparse
    :alt: CRAN Status Badge

.. image:: https://github.com/trevorld/r-optparse/actions/workflows/R-CMD-check.yaml/badge.svg?branch=master
    :target: https://github.com/trevorld/r-optparse/actions
    :alt: R-CMD-check

.. image:: https://codecov.io/github/trevorld/r-optparse/branch/master/graph/badge.svg
    :target: https://app.codecov.io/github/trevorld/r-optparse?branch=master
    :alt: Coverage Status

.. image:: https://cranlogs.r-pkg.org/badges/optparse
    :target: https://cran.r-project.org/package=optparse
    :alt: RStudio CRAN mirror downloads

.. image:: https://tinyverse.netlify.app/badge/optparse
    :target: https://tinyverse.netlify.app/
    :alt: Dependencies

.. raw:: html

   <img src="man/figures/logo.png" align="right" width="200px" alt="optparse hex sticker">

A pure R language command line parser inspired by Python's `optparse <https://docs.python.org/3/library/optparse.html>`__ library to
be used with ``Rscript`` to write ``#!`` shebang scripts that accept short and
long flag/options.

.. _argparse: https://github.com/trevorld/r-argparse

.. _getopt: https://github.com/trevorld/r-getopt

.. _optparse: https://github.com/trevorld/r-optparse

To install the last version released on CRAN use the following command:

.. code:: r

    install.packages("optparse")

To install the development version use the following command:

.. code:: r

    install.packages("remotes")
    remotes::install_github("trevorld/r-optparse")

examples
--------

A simple example:


.. sourcecode:: r
    

        library("optparse")
        parser <- OptionParser() |>
            add_option(c("-v", "--verbose"), action = "store_true",
                       default = TRUE, help = "Print extra output [default]") |>
            add_option(c("-q", "--quietly"), action = "store_false",
                       dest = "verbose", help = "Print little output") |>
            add_option(c("-c", "--count"), type = "integer", default = 5,
                       help = "Number of random normals to generate [default %default]",
                       metavar = "number")
        parse_args(parser, args = c("--quietly", "--count=15"))


::

    ## $help
    ## [1] FALSE
    ## 
    ## $verbose
    ## [1] FALSE
    ## 
    ## $count
    ## [1] 15




Note that the ``args`` argument of ``parse_args()`` default is ``commandArgs(trailing=TRUE)``
so it typically doesn't need to be explicitly set if writing an Rscript.

optparse_ automatically creates a help option:

.. code:: r

    parse_args(parser, args = c("--help"))

::

    Usage: %prog [options]


    Options:
    	-h, --help
    		Show this help message and exit

    	-v, --verbose
    		Print extra output [default]

    	-q, --quietly
    		Print little output

    	-c NUMBER, --count=NUMBER
    		Number of random normals to generate [default 5]


    Error in parse_args(parser, args = c("--help")) : help requested

Note by default when ``optparse::parse_args()`` sees a ``--help`` flag it will first print out a usage message and then either throw an error in interactive use or call ``quit()`` in non-interactive use (i.e. when used within an ``Rscript`` called by a shell).  To disable the error/quit set ``print_help_and_exit = FALSE`` in ``parse_args()`` and to simply print out the usage string one can also use the function ``print_usage()``.

optparse_ has limited positional argument support,
other command-line parsers for R such as argparse_ have richer positional argument support:


.. sourcecode:: r
    

    parse_args(parser, args = c("-vc", "25", "75", "22"), positional_arguments = TRUE)


::

    ## $options
    ## $options$help
    ## [1] FALSE
    ## 
    ## $options$verbose
    ## [1] TRUE
    ## 
    ## $options$count
    ## [1] 25
    ## 
    ## 
    ## $args
    ## [1] "75" "22"



The function ``parse_args2`` wraps ``parse_args`` while setting ``positional_arguments=TRUE`` and ``convert_hyphens_to_underscores=TRUE``:


.. sourcecode:: r
    

        parse_args2(parser, args = c("-vc", "25", "75", "22"))


::

    ## $options
    ## $options$help
    ## [1] FALSE
    ## 
    ## $options$verbose
    ## [1] TRUE
    ## 
    ## $options$count
    ## [1] 25
    ## 
    ## 
    ## $args
    ## [1] "75" "22"



other R packages
----------------

* When optparse_ was originally written in 2009 the only option parsing package on CRAN was the comparatively low-level getopt_ package but as of 2026 there are *at least* 13 R `R Argument/Option Parser Packages <https://github.com/trevorld/r-cli-pkgs>`__.
* Personally I use optparse_ for most scripts I write:

  + It supports the most important features in an argument/option parsing packages with a reasonably high-level interface.
  + It is well-tested with `high code coverage <https://app.codecov.io/github/trevorld/r-optparse?branch=master>`_ and lots of users with millions of cumulative downloads over the past 15+ years.
  + It has no dependencies.
  + It features a stable API with a semantic version number greater than 1.0.

* Occasionally I'll also use the argparse_ package when I need advanced features unsupported by optparse_ like sub parsers and named positional arguments:

  + In general argparse_ supports more advanced features than optparse_ but depends on Python and three R packages whereas optparse_ is a pure R package with no dependencies.
  + However, unlike argparse_, optparse_ does support a callback action and also allows writing custom help usage formatters.

acknowledgements
----------------

A big thanks to:

* Steve Lianoglou for a bug report and patch
* Juan Carlos Borrás for a bug report
* Jim Nikelski for a bug report and patch
* Ino de Brujin and Benjamin Tyner for a bug report
* Jonas Zimmermann for a bug report
* Miroslav Posta for bug reports
* Stefan Seemayer for a bug report and patch
* Kirill Müller for patches
* Steve Humburg for a patch
