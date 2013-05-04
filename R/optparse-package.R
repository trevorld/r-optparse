#'Command line option parser
#'
#'Goal is to create an R package of a command line parser inspired by Python's
#'\dQuote{optparse} library.
#'
#'\code{optparse} is primarily intended to be used with
#'\dQuote{Rscript}. It facilitates writing \dQuote{#!} shebang scripts that
#'accept short and long flags/options. It can also be used from directly, but
#'is probably less useful in this context.
#'
#'See package vignette for a more detailed example.
#'
#'Notes on naming convention in package: 1. An option is one of the shell-split
#'input strings. 2. A flag is a type of option. a flag can be defined as having
#'no argument (defined below), a required argument, or an optional argument. 3.
#'An argument is a type of option, and is the value associated with a flag. 4.
#'A long flag is a type of flag, and begins with the string \dQuote{--}. If the
#'long flag has an associated argument, it may be delimited from the long flag
#'by either a trailing =, or may be the subsequent option. 5. A short flag is a
#'type of flag, and begins with the string \dQuote{-}. If a short flag has an
#'associated argument, it is the subsequent option. short flags may be bundled
#'together, sharing a single leading \dQuote{"-"}, but only the final short
#'flag is able to have a corresponding argument. %%%
#'
#'@name optparse-package
#'@aliases optparse-package optparse
#'@docType package
#'@author Trevor Davis.
#'
#'Some documentation and unit tests ported from Allen Day's getopt package.
#'
#'The documentation for Python's optparse library, which this package is based
#'on, is Copyright 1990-2009, Python Software Foundation.
#'@seealso \code{\link[getopt]{getopt}}
#'@references Python's \code{optparse} library, which this package is based on,
#'is described here: \url{http://docs.python.org/library/optparse.html}
#'@keywords package
#'@examples
#'
#'    example_file <- system.file("exec", "example.R", package = "optparse")
#'    example_file_2 <- system.file("exec", "display_file.R", package = "optparse")
#'    \dontrun{
#'        readLines(example_file)
#'        readLines(example_file_2)
#'    }
#'
NULL
