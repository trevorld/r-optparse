# Copyright (c) 2010-2024 Trevor L. Davis <trevor.l.davis@gmail.com>
# Copyright (c) 2015 Rick FitzJohn https://github.com/richfitz
# Copyright (c) 2013 Kirill Müller https://github.com/krlmlr
# Copyright (c) 2011 Jim Nikelski <nikelski@bic.mni.mcgill.ca>
# Copyright (c) 2010 Steve Lianoglou <lianos@cbio.mskcc.org>
#
#  This file is free software: you may copy, redistribute and/or modify it
#  under the terms of the GNU General Public License as published by the
#  Free Software Foundation, either version 2 of the License, or (at your
#  option) any later version.
#
#  This file is distributed in the hope that it will be useful, but
#  WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#  General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# This file incorporates work from the optparse module in Python 2.6.2.
#
#     Copyright (c) 1990-2009 Python Software Foundation; All Rights Reserved
#
# See (inst/)COPYRIGHTS or http://docs.python.org/2/license.html for the full
# Python (GPL-compatible) license stack.
#
# As mentioned above, this file incorporates some patches by Steve Lianoglou (c) 2010
# He explicitly gave me a non-exclusive unlimited license to code in his patches

#' Option Parser
#'
#' @slot usage The program usage message that will printed out if
#'     \code{parse_args} finds a help option, \code{\%prog} is substituted with the
#'     value of the \code{prog} argument.
#' @slot options A list of of \code{OptionParserOption} instances that will
#'     define how \code{parse_args} reacts to command line options.
#'     \code{OptionParserOption} instances are usually created by \code{make_option}
#'     and can also be added to an existing \code{OptionParser} instance via the
#'     \code{add_option} function.
#' @slot description  Additional text for \code{print_help} to print out between
#'     usage statement and options statement
#' @slot epilogue  Additional text for \code{print_help} to print out after
#'     the options statement
#' @slot formatter  A function that \code{print_help} will use to print out after
#'     the options statement.  Default is [IndentedHelpFormatter()].  This
#'     package also provides the builtin formatter [TitledHelpFormatter()].
#' @author Trevor Davis.
#' @seealso \code{\link{OptionParserOption}}
#' @import methods
#' @exportClass OptionParser
setClass("OptionParser", representation(usage = "character", options = "list",
                description = "character", epilogue = "character",
                formatter = "function"))

#' Class to hold information about command-line options
#'
#' @slot short_flag String of the desired short flag
#'     comprised of the \dQuote{-} followed by a letter.
#' @slot long_flag String of the desired long flag comprised of \dQuote{--}
#'     followed by a letter and then a sequence of alphanumeric characters.
#' @slot action A character string that describes the action \code{optparse}
#'     should take when it encounters an option, either \dQuote{store},
#'     \dQuote{store_true}, or \dQuote{store_false}.  The default is \dQuote{store}
#'     which signifies that \code{optparse} should store the specified following
#'     value if the option is found on the command string.  \dQuote{store_true}
#'     stores \code{TRUE} if the option is found and \dQuote{store_false} stores
#'     \code{FALSE} if the option is found.
#' @slot type A character string that describes specifies which data type
#'     should be stored, either \dQuote{logical}, \dQuote{integer}, \dQuote{double},
#'     \dQuote{complex}, or \dQuote{character}.  Default is \dQuote{logical} if
#'     \code{action \%in\% c("store_true", store_false)}, \code{typeof(default)} if
#'     \code{action == "store"} and default is not \code{NULL} and
#'     \dQuote{character} if \code{action == "store"} and default is \code{NULL}.
#'     \dQuote{numeric} will be converted to \dQuote{double}.
#' @slot dest A character string that specifies what field in the list returned
#'     by \code{parse_args} should \code{optparse} store option values.  Default is
#'     derived from the long flag in \code{opt_str}.
#' @slot default The default value \code{optparse} should use if it does not
#'     find the option on the command line.
#' @slot help A character string describing the option to be used by
#'     \code{print_help} in generating a usage message.  \code{\%default} will be
#'     substituted by the value of \code{default}.
#' @slot metavar A character string that stands in for the option argument when
#'     printing help text.  Default is the value of \code{dest}.
#' @slot callback A function that executes after the each option value is fully parsed
#' @slot callback_args Additional arguments that pass to the callback function.
#' @seealso \code{\link{make_option}}
#' @exportClass OptionParserOption
#' @export OptionParserOption
OptionParserOption <- setClass("OptionParserOption", representation(short_flag = "character", # nolint
                                    long_flag = "character",
                                    action = "character",
                                    type = "character",
                                    dest = "character",
                                    default = "ANY",
                                    help = "character",
                                    metavar = "character",
                                    callback = "ANY",
                                    callback_args = "ANY"))

#' A function to create an instance of a parser object
#'
#' This function is used to create an instance of a parser object
#' which when combined with the \code{parse_args}, \code{make_option}, and \code{add_option}
#' methods is very useful for parsing options from the command line.
#'
#' @param usage The program usage message that will printed out if
#'     \code{parse_args} finds a help option, \code{\%prog} is substituted with the
#'     value of the \code{prog} argument.
#' @param option_list A list of of \code{OptionParserOption} instances that will
#'     define how \code{parse_args} reacts to command line options.
#'     \code{OptionParserOption} instances are usually created by \code{make_option}
#'     and can also be added to an existing \code{OptionParser} instance via the
#'     \code{add_option} function.
#' @param add_help_option Whether a standard help option should be automatically
#'     added to the \code{OptionParser} instance.
#' @param prog Program name to be substituted for \code{\%prog} in the usage
#'     message (including description and epilogue if present),
#'     the default is to use the actual Rscript file name if called by an
#'     Rscript file and otherwise keep \code{\%prog}.
#' @param description  Additional text for \code{print_help} to print out between
#'     usage statement and options statement
#' @param epilogue  Additional text for \code{print_help} to print out after
#'     the options statement
#' @param formatter A function that formats usage text.
#'                  The function should take only one argument (an `OptionParser()` object).
#'                  Default is [IndentedHelpFormatter()].
#'                  The other builtin formatter provided by this package is [TitledHelpFormatter()].
#' @return An instance of the \code{OptionParser} class.
#' @author Trevor Davis.
#'
#' @seealso \code{\link{parse_args}} \code{\link{make_option}}
#'     \code{\link{add_option}}
#' @references Python's \code{optparse} library, which inspired this package,
#'    is described here: \url{https://docs.python.org/3/library/optparse.html}
#' @import getopt
#' @export
OptionParser <- function(usage = "usage: %prog [options]", option_list = list(), # nolint
                            add_help_option = TRUE, prog = NULL,
                            description = "", epilogue = "",
                            formatter = IndentedHelpFormatter) {

    if (is.null(prog)) {
        prog <- get_Rscript_filename()
    }
    if (length(prog) && !is.na(prog)) {
        usage <- gsub("%prog", prog, usage)
        description <- gsub("%prog", prog, description)
        epilogue <- gsub("%prog", prog, epilogue)
    }
    # Match behavior of usage string in Python optparse package
    usage <- sub("^usage: ", "Usage: ", usage)
    usage <- ifelse(grepl("^Usage: ", usage), usage, sub("^", "Usage: ", usage))
    if (add_help_option) {
        option_list[[length(option_list) + 1]] <-
            make_option(c("-h", "--help"),
                action = "store_true", dest = "help", default = FALSE,
                help = "Show this help message and exit")
    }

    return(new("OptionParser", usage = usage, options = option_list,
                    description = description, epilogue = epilogue,
                    formatter = formatter))
}

#' Functions to enable our OptionParser to recognize specific command line
#' options.
#'
#' \code{add_option} adds a option to a prexisting \code{OptionParser} instance
#' whereas \code{make_option} is used to create a list of
#' \code{OptionParserOption} instances that will be used in the
#' \code{option_list} argument of the \code{OptionParser} function to create a
#' new \code{OptionParser} instance.
#'
#' @rdname add_make_option
#' @param object An instance of the \code{OptionParser} class
#' @param opt_str A character vector containing the string of the desired long
#'     flag comprised of \dQuote{--} followed by a letter and then a sequence of
#'     alphanumeric characters and optionally a string of the desired short flag
#'     comprised of the \dQuote{-} followed by a letter.
#' @param action A character string that describes the action \code{optparse}
#'     should take when it encounters an option, either \dQuote{store},
#'     \dQuote{store_true}, \dQuote{store_false}, or \dQuote{callback}.
#'     An action of \dQuote{store} signifies that \code{optparse}
#'      should store the specified following value if the option is found on the command string.
#'     \dQuote{store_true} stores \code{TRUE} if the option is found
#'      and \dQuote{store_false} stores \code{FALSE} if the option is found.
#'     \dQuote{callback} stores the return value produced by the function
#'     specified in the \code{callback} argument.
#'     If \code{callback} is not \code{NULL} then the default is \dQuote{callback} else \dQuote{store}.
#' @param type A character string that describes specifies which data type
#'     should be stored, either \dQuote{logical}, \dQuote{integer}, \dQuote{double},
#'     \dQuote{complex}, or \dQuote{character}.  Default is \dQuote{logical} if
#'     \code{action \%in\% c("store_true", store_false)}, \code{typeof(default)} if
#'     \code{action == "store"} and default is not \code{NULL} and
#'     \dQuote{character} if \code{action == "store"} and default is \code{NULL}.
#'     \dQuote{numeric} will be converted to \dQuote{double}.
#' @param dest A character string that specifies what field in the list returned
#'     by \code{parse_args} should \code{optparse} store option values.  Default is
#'     derived from the long flag in \code{opt_str}.
#' @param default The default value \code{optparse} should use if it does not
#'     find the option on the command line.
#' @param help A character string describing the option to be used by
#'     \code{print_help} in generating a usage message.  \code{\%default} will be
#'     substituted by the value of \code{default}.
#' @param metavar A character string that stands in for the option argument when
#'     printing help text.  Default is the value of \code{dest}.
#' @param callback A function that executes after the each option value is fully
#'     parsed.  It's value is assigned to the option and its arguments are
#'     the option S4 object, the long flag string, the value of the option,
#'     the parser S4 object, and \code{...}.
#' @param callback_args A list of additional arguments passed to callback function (via \code{do.call}).
#' @return Both \code{make_option} and \code{add_option} return instances of
#'     class \code{OptionParserOption}.
#' @author Trevor Davis.
#'
#' @seealso \code{\link{parse_args}} \code{\link{OptionParser}}
#' @references Python's \code{optparse} library, which inspires this package,
#'     is described here: \url{https://docs.python.org/3/library/optparse.html}
#' @examples
#'
#'    make_option("--longflag")
#'    make_option(c("-l", "--longflag"))
#'    make_option("--integer", type = "integer", default = 5)
#'    make_option("--integer", default = as.integer(5))  # same as previous
#'
#'    # examples from package vignette
#'    make_option(c("-v", "--verbose"), action = "store_true", default = TRUE,
#'        help = "Print extra output [default]")
#'    make_option(c("-q", "--quietly"), action = "store_false",
#'      dest = "verbose", help = "Print little output")
#'    make_option(c("-c", "--count"), type = "integer", default = 5,
#'        help = "Number of random normals to generate [default %default]",
#'        metavar = "number")
#'    make_option("--generator", default = "rnorm",
#'        help = "Function to generate random deviates [default \"%default\"]")
#'    make_option("--mean", default = 0,
#'        help = "Mean if generator == \"rnorm\" [default %default]")
#'    make_option("--sd", default = 1, metavar = "standard deviation",
#'        help = "Standard deviation if generator == \"rnorm\" [default %default]")
#'
#' @export
make_option <- function(opt_str, action = NULL, type = NULL, dest = NULL, default = NULL,
                        help = "", metavar = NULL, callback = NULL, callback_args = NULL) {

    action <- ifelse(is.null(action), ifelse(is.null(callback), "store", "callback"), action)

    # flags
    short_flag <- opt_str[grepl("^-[[:alpha:]]", opt_str)]
    if (length(short_flag) == 0) {
        short_flag <- NA_character_
    } else {
        if (nchar(short_flag) > 2) {
            stop(paste("Short flag", short_flag, "must only be a '-' and a single letter"))
        }
    }
    long_flag <- opt_str[grepl("^--[[:alpha:]]", opt_str)]
    if (length(long_flag) == 0) stop("We require a long flag option")

    # type
    if (is.null(type)) {
        type <- infer_type(action, default)
    }
    if (type == "numeric") type <- "double"

    # default
    if ((action != "callback") &&
        (type != typeof(default)) &&
        !is.null(default)) {
        storage.mode(default) <- type
    }

    # dest
    if (is.null(dest)) dest <- sub("^--", "", long_flag)

    # metavar
    if (is.null(metavar)) {
        if (option_needs_argument_helper(action, type)) {
            metavar <- sub("^--", "", long_flag)
        } else {
            metavar <- character(0)
        }
    }
    warn_callback(action, callback, callback_args)
    if (is.null(callback_args))
        callback_args <- list()

    return(new("OptionParserOption", short_flag = short_flag, long_flag = long_flag,
                        action = action, type = type, dest = dest, default = default,
                        help = help, metavar = metavar,
                        callback = callback, callback_args = callback_args))
}

infer_type <- function(action, default) {
    switch(action,
           store = ifelse(is.null(default), "character", typeof(default)),
           store_false = "logical",
           store_true = "logical",
           callback = "NULL")
}

warn_callback <- function(action, callback, callback_args) {
    if (action == "callback") {
        if (!is.function(callback))
            warning(sprintf("callback argument is not a function"))
    } else {
        if (!is.null(callback))
            warning(sprintf("callback argument is supplied for non-callback action"))
        if (!is.null(callback_args))
            warning(sprintf("callback_args argument is supplied for non-callback action"))
    }
}

#' @rdname add_make_option
#' @export
add_option <- function(object, opt_str, action = NULL, type = NULL,
                    dest = NULL, default = NULL, help = "", metavar = NULL,
                    callback = NULL, callback_args = NULL) {
    options_list <- object@options
    n_original_options <- length(options_list)
    options_list[[n_original_options + 1]] <- make_option(opt_str = opt_str,
                                           action = action, type = type, dest = dest,
                                           default = default, help = help, metavar = metavar,
                                           callback = callback, callback_args = callback_args)
    object@options <- options_list
    return(object)
}

#' Printing an usage message from an OptionParser object
#'
#' \code{print_help} print an usage message from an OptionParser object, usually
#' called by \code{parse_args} when it encounters a help option.
#'
#' @param object A \code{OptionParser} instance.
#' @return \code{print_help} uses the \code{cat} function to print out a usage
#' message.  It returns \code{invisible(NULL)}.
#' @author Trevor Davis.
#'
#' @seealso \code{{parse_args}} \code{{OptionParser}}
#' @references Python's \code{optparse} library, which inspired this package,
#'     is described here: \url{https://docs.python.org/3/library/optparse.html}
#' @export
print_help <- function(object) {
    object@formatter(object)
}

#' Builtin help text formatters
#'
#' `IndentedHelpFormatter()` is the default help text formatter.
#' `TitledHelpFormatter()` is an alternative help text formatter.
#'
#' @param object An [OptionParser()] object.
#' @examples
#'  parser <- OptionParser(formatter = IndentedHelpFormatter)
#'  parser <- add_option(parser, "--generator", help = "Generator option")
#'  parser <- add_option(parser, "--count", help = "Count option")
#'  print_help(parser)
#'
#'  parser <- OptionParser(formatter = TitledHelpFormatter)
#'  parser <- add_option(parser, "--generator", help = "Generator option")
#'  parser <- add_option(parser, "--count", help = "Count option")
#'  print_help(parser)
#' @return `NULL` invisibly.  As a side effect prints out help text.
#' @rdname formatter
#' @export
IndentedHelpFormatter <- function(object) { # nolint
    cat(object@usage, fill = TRUE)
    cat(object@description, fill = TRUE)
    cat("\n")
    cat("Options:", sep = "\n")

    options_list <- object@options
    for (ii in seq_along(options_list)) {
        option <- options_list[[ii]]
        cat("\t")
        if (!is.na(option@short_flag)) {
            cat(option@short_flag)
            if (option_needs_argument(option)) {
                cat(" ", toupper(option@metavar), sep = "")
            }
            cat(", ")
        }
        if (!is.null(option@long_flag)) {
            cat(option@long_flag)
            if (option_needs_argument(option)) {
                cat("=", toupper(option@metavar), sep = "")
            }
        }
        cat("\n\t\t")
        cat(sub("%default", as_string(option@default), option@help))
        cat("\n\n")
    }
    cat(object@epilogue, fill = TRUE)
    return(invisible(NULL))
}

#' @rdname formatter
#' @export
TitledHelpFormatter <- function(object) { # nolint
    usage <- c("Usage\n=====\n", gsub("Usage: ", "", object@usage))
    cat(usage, fill = TRUE)
    cat(object@description, fill = TRUE)
    cat("\n")
    cat("Options", "=======", sep = "\n")

    options_list <- object@options
    for (ii in seq_along(options_list)) {
        option <- options_list[[ii]]
        if (!is.null(option@long_flag)) {
            cat(option@long_flag)
            if (option_needs_argument(option)) {
                cat("=", toupper(option@metavar), sep = "")
            }
        }
        if (!is.na(option@short_flag)) {
            cat(", ")
            cat(option@short_flag)
            if (option_needs_argument(option)) {
                cat(" ", toupper(option@metavar), sep = "")
            }
        }
        cat("\n\t\t")
        cat(sub("%default", as_string(option@default), option@help))
        cat("\n\n")
    }
    cat(object@epilogue, fill = TRUE)
    return(invisible(NULL))
}

# Turn default values into a string we can cat, handles NA's and NULL's
as_string <- function(default) {
    if (is.null(default)) {
        default_str <- "NULL"
    } else if (!length(default)) {
        default_str <- paste0(typeof(default), "(0)")
    } else if (is.na(default)) {
        default_str <- "NA"
    } else {
        default_str <- as.character(default)
    }
}

#' Parse command line options.
#'
#' \code{parse_args} parses command line options using an \code{OptionParser}
#' instance for guidance. \code{parse_args2} is a wrapper to \code{parse_args}
#' setting the options \code{positional_arguments} and \code{convert_hyphens_to_underscores}
#' to \code{TRUE}.
#'
#' @param object An \code{OptionParser} instance.
#' @param args A character vector containing command line options to be parsed.
#'     Default is everything after the Rscript program in the command line. If
#'     \code{positional_arguments} is not \code{FALSE} then \code{parse_args} will
#'     look for positional arguments at the end of this vector.
#' @param print_help_and_exit Whether \code{parse_args} should call
#'     \code{print_help} to print out a usage message and exit the program.  Default
#'     is \code{TRUE}.
#' @param positional_arguments Number of \emph{positional} arguments.  A numeric
#'     denoting the exact number of supported arguments, or a numeric vector of
#'     length two denoting the minimum and maximum number of arguments
#'     (\code{Inf} for no limit).  The value \code{TRUE} is equivalent to
#'     \code{c(0, Inf)}.  The default \code{FALSE} is
#'     supported for backward compatibility only, as it alters
#'     the format of the return value.
#' @param convert_hyphens_to_underscores If the names in the returned list of options
#'      contains hyphens then convert them to underscores.  The default \code{FALSE} is
#'      supported for backward compatibility reasons as it alters the format of the return value
#' @return Returns a list with field \code{options} containing our option values
#'     as well as another field \code{args} which contains a vector of
#'     positional arguments.  For backward compatibility, if and only if
#'     \code{positional_arguments} is \code{FALSE}, returns a list containing
#'     option values.
#' @section Acknowledgement:
#'     A big thanks to Steve Lianoglou for a bug report and patch;
#'     Juan Carlos \enc{Borrás}{Borras} for a bug report;
#'     Jim Nikelski for a bug report and patch;
#'     Ino de Brujin and Benjamin Tyner for a bug report;
#'     Jonas Zimmermann for bug report; Miroslav Posta for bug reports;
#'     Stefan Seemayer for bug report and patch;
#'     Kirill \enc{Müller}{Muller} for patches; Steve Humburg for patch.
#' @author Trevor Davis.
#'
#' @seealso \code{\link{OptionParser}} \code{\link{print_help}}
#' @references Python's \code{optparse} library, which inspired this package,
#'      is described here: \url{https://docs.python.org/3/library/optparse.html}
#' @encoding latin1
#' @examples
#' # example from vignette
#' option_list <- list(
#'    make_option(c("-v", "--verbose"), action = "store_true", default = TRUE,
#'        help = "Print extra output [default]"),
#'    make_option(c("-q", "--quietly"), action = "store_false",
#'        dest = "verbose", help = "Print little output"),
#'    make_option(c("-c", "--count"), type = "integer", default = 5,
#'        help = "Number of random normals to generate [default %default]",
#'        metavar = "number"),
#'    make_option("--generator", default = "rnorm",
#'        help = "Function to generate random deviates [default \"%default\"]"),
#'    make_option("--mean", default = 0,
#'        help = "Mean if generator == \"rnorm\" [default %default]"),
#'    make_option("--sd", default = 1, metavar = "standard deviation",
#'        help = "Standard deviation if generator == \"rnorm\" [default %default]")
#'    )
#'parse_args(OptionParser(option_list = option_list), args = c("--sd=3", "--quietly"))
#'
#'# example from vignette using positional arguments
#'option_list2 <- list(
#'    make_option(c("-n", "--add-numbers"), action = "store_true", default = FALSE,
#'        help = "Print line number at the beginning of each line [default]")
#'    )
#'parser <- OptionParser(usage = "%prog [options] file", option_list = option_list2)
#'
#'parse_args(parser, args = c("--add-numbers", "example.txt"), positional_arguments = TRUE)
#'
#'parse_args(parser, args = c("--add-numbers", "example.txt"), positional_arguments = TRUE,
#'          convert_hyphens_to_underscores = TRUE)
#'
#'parse_args2(parser, args = c("--add-numbers", "example.txt"))
#'
#' @import getopt
#' @importFrom utils tail
#' @export
parse_args <- function(object, args = commandArgs(trailingOnly = TRUE),
                    print_help_and_exit = TRUE, positional_arguments = FALSE,
                    convert_hyphens_to_underscores = FALSE) {

    tryCatch(parse_args_helper(object, args,
                               print_help_and_exit, positional_arguments,
                               convert_hyphens_to_underscores),
             error = function(e) pa_stop(object, e))
}

quieter_error_handler <- function(e) {
    quit('no', status = 1, runLast = FALSE)
}

pa_stop <- function(object, e) {
    cnd <- errorCondition(e$message,
                          call = "optparse::parse_args_helper()",
                          class = "optparse_parse_error")
    if (interactive()) {
        stop(cnd)
    } else {
        signalCondition(cnd)
        msg <- paste0("\n", get_Rscript_filename(), ": error: ", e$message)
        cat(object@usage, msg, sep = "\n", file = stderr())
        opt <- options(error = getOption("error",  quieter_error_handler),
                       show.error.messages = FALSE)
        on.exit(options(opt))
        stop(cnd)
    }
}

parse_args_helper <- function(object, args = commandArgs(trailingOnly = TRUE),
                              print_help_and_exit = TRUE, positional_arguments = FALSE,
                              convert_hyphens_to_underscores = FALSE) {

    pal <- should_include_any_args(positional_arguments)
    include_any_args <- pal$include_any_args
    positional_arguments <- pal$positional_arguments

    pal <- parse_positional_args(object, args, positional_arguments)
    arguments_positional <- pal$arguments_positional
    args <- pal$args

    options_list <- parse_options(object, args, convert_hyphens_to_underscores)

    if (any(grepl("^help$", names(options_list)))) {
        if (options_list[["help"]] && print_help_and_exit) {
            print_help(object)
            if (interactive())
                stop("help requested")
            else
                quit(status = 0)
        }
    }

    if (length(arguments_positional) < min(positional_arguments)) {
      stop(sprintf("required at least %g positional arguments, got %g",
                   min(positional_arguments), length(arguments_positional)))
    }
    if (length(arguments_positional) > max(positional_arguments)) {
      stop(sprintf("required at most %g positional arguments, got %g",
                   max(positional_arguments), length(arguments_positional)))
    }
    if (include_any_args) {
        return(list(options = options_list, args = arguments_positional))
    } else {
        return(options_list)
    }
}


getopt_options <- function(object, args) {
    # Convert our option specification into ``getopt`` format
    n_options <- length(object@options)
    spec <- matrix(NA, nrow = n_options, ncol = 5)
    for (ii in seq_along(object@options)) {
        spec[ii, ] <- convert_to_getopt(object@options[[ii]])
    }

    if (length(args)) {
        opt <- try(getopt(spec = spec, opt = args), silent = TRUE)
        if (inherits(opt, "try-error")) {
            if (grepl("redundant short names for flags", opt)) {
                opt <- paste(opt, "did you forget to set ``add_help_option=FALSE`` in ``OptionParser``")
            }
            stop(opt)
        }
    } else {
        opt <- list()
    }
    opt
}

should_include_any_args <- function(positional_arguments) {
    # pull out positional arguments if ``positional_arguments`` was set to TRUE
    # or not 0 or c(0, 0)
    if (!(length(positional_arguments) %in% 1L:2L))
        stop("positional_arguments must have length 1 or 2")
    if (is.logical(positional_arguments)) {
        if (positional_arguments) {
            positional_arguments <- c(0, Inf)
            include_any_args <- TRUE
        } else {
            include_any_args <- FALSE
        }
    } else if (is.numeric(positional_arguments)) {
        include_any_args <- TRUE
    } else {
        stop("positional_arguments must be logical or numeric")
    }
    list(include_any_args = include_any_args,
         positional_arguments = positional_arguments)
}

parse_positional_args <- function(object, args, positional_arguments) {
    arguments_positional <- character(0)
    if (max(positional_arguments) > 0) {
        original_arguments <- args
        args <- NULL
        is_taken <- FALSE # set to true if optional argument needs to take next argument
        for (argument in original_arguments) {
            if (is_taken) {
                args <- c(args, argument)
                is_taken <- FALSE
            } else {
                if (is_option_string(argument, object)) {
                    args <- c(args, argument)
                    if (requires_argument(argument, object))
                        is_taken <- TRUE
                } else {
                    arguments_positional <- c(arguments_positional, argument)
                }
            }
        }
    }
    list(arguments_positional = arguments_positional, args = args)
}

parse_options <- function(object, args, convert_hyphens_to_underscores) {

    opt <- getopt_options(object, args)

    options_list <- list()
    for (ii in seq_along(object@options)) {
        option <- object@options[[ii]]
        option_value <- opt[[sub("^--", "", option@long_flag)]]
        if (!is.null(option_value)) {
            if (option@action == "store_false") {
                options_list[[option@dest]] <- FALSE
            } else {
                options_list[[option@dest]] <- option_value
            }
            if (option@action == "callback") {
                callback_fn <- function(...) {
                    option@callback(option, option@long_flag,  option_value, object, ...) # nolint
                }
                options_list[[option@dest]] <- do.call(callback_fn, option@callback_args)
            }
        } else {
            if (!is.null(option@default) && is.null(options_list[[option@dest]])) {
                options_list[[option@dest]] <- option@default
            }
        }
    }
    if (convert_hyphens_to_underscores) {
        names(options_list) <- gsub("-", "_", names(options_list))
    }
    options_list
}

#' @rdname parse_args
#' @export
parse_args2 <- function(object, args = commandArgs(trailingOnly = TRUE),
                        print_help_and_exit = TRUE) {
    parse_args(object, args = args, print_help_and_exit = print_help_and_exit,
               positional_arguments = TRUE, convert_hyphens_to_underscores = TRUE)
}

# Tells me whether a string is a valid option
is_option_string <- function(argument, object) {
    if (is_long_flag(argument)) {
        return(TRUE)
    } else if (is_short_flag(argument)) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
# Tells me if an option string needs to take an argument
requires_argument <- function(argument, object) {
    if (is_long_flag(argument)) {
        requires_long_flag(argument, object)
    } else { # is a short flag
        requires_short_flag(argument, object)
    }
}

requires_long_flag <- function(argument, object) {
    if (grepl("=", argument)) {
        return(FALSE)
    } else {
        for (ii in seq_along(object@options)) {
            option <- object@options[[ii]]
            if (option@long_flag == argument)
                return(option_needs_argument(option))
        }
        stop(paste("no such option:", argument))
    }
}

requires_short_flag <- function(argument, object) {
    last_flag <- tail(expand_short_option(argument), 1)
    for (ii in seq_along(object@options)) {
        option <- object@options[[ii]]
        if (!is.na(option@short_flag) && option@short_flag == last_flag)
            return(option_needs_argument(option))
    }
    stop(paste("no such option:", last_flag))
}


# convenience functions that tells if argument is a type of flag and returns all long flag options or short flag options
is_long_flag <- function(argument) return(grepl("^--", argument))
is_short_flag <- function(argument) return(grepl("^-[^-]", argument))
# expand_short_option based on function by Jim Nikelski (c) 2011
# He gave me a non-exclusive unlimited license to this code
# expand_short_option("-cde") = c("-c", "-d", "-e") # nolint
expand_short_option <- function(argument) {
    if (nchar(argument) == 2) {
        return(argument)
    } else {
        argument <- substr(argument, 2, nchar(argument)) # remove leading dash
        argument <- strsplit(argument, "")[[1]] # split into individual characters
        argument <- paste("-", argument, sep = "") # add leading dash to each short option
        return(argument)
    }
}

# Converts our representation of options to format getopt can understand
convert_to_getopt <- function(object) {
    short_flag <- sub("^-", "", object@short_flag)
    long_flag <- sub("^--", "", object@long_flag)
    argument <- ifelse(option_needs_argument(object), 1, 0)
    type <- ifelse(object@type == "NULL", "logical", object@type)
    return(c(long_flag, short_flag, argument, type, object@help))
}
option_needs_argument <- function(option) {
    option_needs_argument_helper(option@action, option@type)
}
option_needs_argument_helper <- function(action, type) {
    switch(action,
           store = TRUE,
           callback = ifelse(type == "NULL", FALSE, TRUE),
           FALSE)
}
