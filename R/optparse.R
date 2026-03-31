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
#'     [parse_args()] finds a help option, `\%prog` is substituted with the
#'     value of the `prog` argument.
#' @slot options A list of of `OptionParserOption` instances that will
#'     define how [parse_args()] reacts to command line options.
#'     `OptionParserOption` instances are usually created by [make_option()]
#'     and can also be added to an existing `OptionParser` instance via the
#'     [add_option()] function.
#' @slot description  Additional text for [print_help()] to print out between
#'     usage statement and options statement
#' @slot epilogue  Additional text for [print_help()] to print out after
#'     the options statement
#' @slot formatter  A function that [print_help()] will use to print out after
#'     the options statement.  Default is [IndentedHelpFormatter()].  This
#'     package also provides the builtin formatter [TitledHelpFormatter()].
#' @seealso [OptionParserOption]
#' @import methods
#' @exportClass OptionParser
setClass(
	"OptionParser",
	representation(
		usage = "character",
		options = "list",
		description = "character",
		epilogue = "character",
		formatter = "function"
	)
)

setValidity("OptionParser", function(object) {
	long_flags <- vapply(object@options, function(o) o@long_flag, character(1L))
	if (anyDuplicated(long_flags)) {
		dup <- long_flags[duplicated(long_flags)][1L]
		msg <- paste0("duplicate long flag: ", dup)
		if (dup == "--help") {
			msg <- paste0(
				msg,
				" (did you forget to set `add_help_option = FALSE` in `OptionParser()`?)"
			)
		}
		option_conflict_error_stop(msg)
	}
	short_flags <- na_omit(vapply(object@options, function(o) o@short_flag, character(1L)))
	if (anyDuplicated(short_flags)) {
		dup <- short_flags[duplicated(short_flags)][1L]
		msg <- paste0("duplicate short flag: ", dup)
		if (dup == "-h") {
			msg <- paste0(
				msg,
				" (did you forget to set `add_help_option = FALSE` in `OptionParser()`?)"
			)
		}
		option_conflict_error_stop(msg)
	}
	TRUE
})

setGeneric("options<-", function(x, value) standardGeneric("options<-"))
setMethod("options<-", "OptionParser", function(x, value) {
	x@options <- value
	validObject(x)
	x
})

#' Class to hold information about command-line options
#'
#' @slot short_flag String of the desired short flag
#'     comprised of the `-` followed by a single non-dash character (but not `=` or a whitespace character).
#' @slot long_flag String of the desired long flag comprised of `--`
#'     followed by a non-dash character and then (optionally) more characters (but not any `=` or whitespace characters).
#' @slot action `r ro_action`
#' @slot type `r ro_type`
#' @slot dest `r ro_dest`
#' @slot default `r ro_default`
#' @slot const `r ro_const`
#' @slot required `r ro_required`
#' @slot help `r ro_help`
#' @slot metavar `r ro_metavar`
#' @slot callback `r ro_callback`
#' @slot callback_args `r ro_callback_args`
#' @seealso [make_option()]
#' @exportClass OptionParserOption
#' @export OptionParserOption
OptionParserOption <- setClass(
	"OptionParserOption",
	representation(
		short_flag = "character", # nolint
		long_flag = "character",
		action = "character",
		type = "character",
		dest = "character",
		default = "ANY",
		help = "character",
		metavar = "character",
		callback = "ANY",
		callback_args = "ANY",
		const = "ANY",
		required = "logical"
	)
)

check_action <- function(action) {
	valid_actions <- c(
		"append",
		"append_const",
		"callback",
		"count",
		"store",
		"store_const",
		"store_false",
		"store_true"
	)
	if (!action %in% valid_actions) {
		option_error_stop(
			paste0(
				'"',
				action,
				'" is not a valid action; ',
				"must be one of: ",
				paste(paste0('"', valid_actions, '"'), collapse = ", ")
			),
			call = NULL
		)
	}
}

check_type <- function(type) {
	valid_types <- c("character", "complex", "double", "integer", "logical", "NULL")
	if (length(type) == 1L && !type %in% valid_types) {
		option_error_stop(
			paste0(
				'"',
				type,
				'" is not a valid type; ',
				"must be one of: ",
				paste(paste0('"', valid_types, '"'), collapse = ", ")
			),
			call = NULL
		)
	}
}

check_short <- function(short_flag) {
	if (is.na(short_flag)) {
		return(invisible(NULL))
	}
	if (nchar(short_flag) > 2) {
		option_error_stop(
			paste("Short flag", short_flag, "must only be a '-' and a single non-dash character"),
			call = NULL
		)
	}
	if (grepl("[[:space:]]", short_flag)) {
		option_error_stop(
			paste("Short flag", short_flag, "must not contain whitespace"),
			call = NULL
		)
	}
	if (grepl("=", short_flag, fixed = TRUE)) {
		option_error_stop(
			paste("Short flag", short_flag, "must not contain '='"),
			call = NULL
		)
	}
}

check_long <- function(long_flag) {
	if (length(long_flag) == 0) {
		option_error_stop("We require a long flag option", call = NULL)
	}
	if (grepl("=", long_flag, fixed = TRUE)) {
		option_error_stop(
			paste("Long flag", long_flag, "must not contain '='"),
			call = NULL
		)
	}
	if (grepl("[[:space:]]", long_flag)) {
		option_error_stop(
			paste("Long flag", long_flag, "must not contain whitespace"),
			call = NULL
		)
	}
}

# Currently redundant since these are called by `make_option()`
# setValidity("OptionParserOption", function(object) {
# 	check_short(object@short_flag)
# 	check_long(object@long_flag)
# 	check_action(object@action)
# 	check_type(object@type)
# 	TRUE
# })

#' A function to create an instance of a parser object
#'
#' This function is used to create an instance of a parser object
#' which when combined with the [parse_args()], [make_option()], and [add_option()]
#' methods is very useful for parsing options from the command line.
#'
#' @param usage The program usage message that will printed out if
#'     [parse_args()] finds a help option, `\%prog` is substituted with the
#'     value of the `prog` argument.
#' @param option_list A list of of `OptionParserOption` instances that will
#'     define how [parse_args()] reacts to command line options.
#'     `OptionParserOption` instances are usually created by [make_option()]
#'     and can also be added to an existing `OptionParser` instance via the
#'     [add_option()] function.
#' @param add_help_option Whether a standard help option should be automatically
#'     added to the `OptionParser` instance.
#' @param prog Program name to be substituted for `\%prog` in the usage
#'     message (including description and epilogue if present),
#'     the default is to use the actual Rscript file name if called by an
#'     Rscript file and otherwise keep `\%prog`.
#' @param description  Additional text for [print_help()] to print out between
#'     usage statement and options statement
#' @param epilogue  Additional text for [print_help()] to print out after
#'     the options statement
#' @param formatter A function that formats usage text.
#'                  The function should take only one argument (an `OptionParser()` object).
#'                  Default is [IndentedHelpFormatter()].
#'                  The other builtin formatter provided by this package is [TitledHelpFormatter()].
#' @return An instance of the `OptionParser` class.
#'
#' @seealso [parse_args()] [make_option()] [add_option()]
#' @references Python's `optparse` library, which inspired this package,
#'    is described here: \url{https://docs.python.org/3/library/optparse.html}
#' @export
OptionParser <- function(
	usage = "usage: %prog [options]",
	option_list = list(), # nolint
	add_help_option = TRUE,
	prog = NULL,
	description = "",
	epilogue = "",
	formatter = IndentedHelpFormatter
) {
	if (is.null(prog)) {
		prog <- get_Rscript_filename()
	}
	if (length(prog) && !is.na(prog)) {
		usage <- gsub("%prog", prog, usage, fixed = TRUE)
		description <- gsub("%prog", prog, description, fixed = TRUE)
		epilogue <- gsub("%prog", prog, epilogue, fixed = TRUE)
	}
	# Match behavior of usage string in Python optparse package
	usage <- sub("^usage: ", "Usage: ", usage)
	usage <- ifelse(grepl("^Usage: ", usage), usage, sub("^", "Usage: ", usage))
	if (add_help_option) {
		option_list[[length(option_list) + 1]] <-
			make_option(
				c("-h", "--help"),
				action = "store_true",
				dest = "help",
				default = FALSE,
				help = "Show this help message and exit"
			)
	}

	return(new(
		"OptionParser",
		usage = usage,
		options = option_list,
		description = description,
		epilogue = epilogue,
		formatter = formatter
	))
}

option_error_stop <- function(msg, call = sys.call(-1)) {
	stop(errorCondition(msg, class = "optparse_option_error", call = call))
}

option_conflict_error_stop <- function(msg, call = sys.call(-1)) {
	stop(errorCondition(
		msg,
		class = c("optparse_option_conflict_error", "optparse_option_error"),
		call = call
	))
}

#' Functions to enable our OptionParser to recognize specific command line
#' options.
#'
#' [add_option()] adds a option to a prexisting `OptionParser` instance
#' whereas [make_option()] is used to create a list of
#' `OptionParserOption` instances that will be used in the
#' `option_list` argument of the `OptionParser` function to create a
#' new `OptionParser` instance.
#'
#' @rdname add_make_option
#' @param object An instance of the `OptionParser` class
#' @param opt_str A character vector containing the string of the desired long
#'     flag comprised of `--` followed by a non-dash character (and optionally more characters), and
#'     optionally a string of the desired short flag comprised of the
#'     `-` followed by a single non-dash character.  We don't allow `=` or whitespace characters in flags.
#' @param action `r ro_action`
#' @param type `r ro_type`
#' @param dest `r ro_dest`
#' @param default `r ro_default`
#' @param const `r ro_const`
#' @param required `r ro_required`
#' @param help `r ro_help`
#' @param metavar `r ro_metavar`
#' @param callback `r ro_callback`
#' @param callback_args `r ro_callback_args`
#' @return Both [make_option()] and [add_option()] return instances of
#'     class `OptionParserOption`.
#' @section Errors:
#'   The following classed errors may be thrown:
#'
#'   - `optparse_option_error`: invalid option definition (bad flag string,
#'     unrecognized action, etc.).
#'     - `optparse_option_conflict_error`: duplicate flag detected when
#'       adding an option to a parser.
#'
#' @seealso [parse_args()] [OptionParser()]
#' @references Python's `optparse` library, which inspires this package,
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
make_option <- function(
	opt_str,
	action = NULL,
	type = NULL,
	dest = NULL,
	default = NULL,
	help = "",
	metavar = NULL,
	callback = NULL,
	callback_args = NULL,
	const = NULL,
	required = FALSE
) {
	action <- ifelse(is.null(action), ifelse(is.null(callback), "store", "callback"), action)

	# flags
	short_flag <- opt_str[grepl("^-[^-]", opt_str)]
	if (length(short_flag) == 0) {
		short_flag <- NA_character_
	} else {
		check_short(short_flag)
	}
	long_flag <- opt_str[grepl("^--[^-]", opt_str)]
	check_long(long_flag)

	check_action(action)

	# type
	if (is.null(type)) {
		type <- infer_type(action, default, const)
	}
	if (type == "numeric") {
		type <- "double"
	}
	check_type(type)

	# default
	if (
		!(action %in% c("callback", "store_const", "append_const")) &&
			(type != typeof(default)) &&
			!is.null(default)
	) {
		storage.mode(default) <- type
	}

	# dest
	if (is.null(dest)) {
		dest <- sub("^--", "", long_flag)
	}

	# metavar
	if (is.null(metavar)) {
		if (option_needs_argument_helper(action, type)) {
			metavar <- sub("^--", "", long_flag)
		} else {
			metavar <- character(0)
		}
	}
	warn_callback(action, callback, callback_args)
	if (is.null(callback_args)) {
		callback_args <- list()
	}

	return(new(
		"OptionParserOption",
		short_flag = short_flag,
		long_flag = long_flag,
		action = action,
		type = type,
		dest = dest,
		default = default,
		const = const,
		required = required,
		help = help,
		metavar = metavar,
		callback = callback,
		callback_args = callback_args
	))
}

infer_type <- function(action, default, const) {
	switch(
		action,
		store = ifelse(is.null(default), "character", typeof(default)),
		append = ifelse(is.null(default), "character", typeof(default[[1]])),
		store_const = typeof(const),
		append_const = typeof(const),
		store_false = "logical",
		store_true = "logical",
		count = "integer",
		callback = "NULL"
	)
}

warn_callback <- function(action, callback, callback_args) {
	if (action == "callback") {
		if (!is.function(callback)) {
			warning(sprintf("callback argument is not a function"))
		}
	} else {
		if (!is.null(callback)) {
			warning(sprintf("callback argument is supplied for non-callback action"))
		}
		if (!is.null(callback_args)) {
			warning(sprintf("callback_args argument is supplied for non-callback action"))
		}
	}
}

#' @rdname add_make_option
#' @export
add_option <- function(
	object,
	opt_str,
	action = NULL,
	type = NULL,
	dest = NULL,
	default = NULL,
	help = "",
	metavar = NULL,
	callback = NULL,
	callback_args = NULL,
	const = NULL,
	required = FALSE
) {
	opts <- object@options
	opts[[length(opts) + 1L]] <- make_option(
		opt_str = opt_str,
		action = action,
		type = type,
		dest = dest,
		default = default,
		const = const,
		required = required,
		help = help,
		metavar = metavar,
		callback = callback,
		callback_args = callback_args
	)
	options(object) <- opts
	return(object)
}

#' Printing an usage message from an OptionParser object
#'
#' [print_help()] print an usage message from an OptionParser object, usually
#' called by [parse_args()] when it encounters a help option.
#'
#' @param object A `OptionParser` instance.
#' @return [print_help()] uses the `cat` function to print out a usage
#' message.  It returns `invisible(NULL)`.
#'
#' @seealso [parse_args()] [OptionParser()]
#' @references Python's `optparse` library, which inspired this package,
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
IndentedHelpFormatter <- function(object) {
	# nolint
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
TitledHelpFormatter <- function(object) {
	# nolint
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
		"NULL"
	} else if (!length(default)) {
		paste0(typeof(default), "(0)")
	} else if (is.na(default)) {
		"NA"
	} else {
		as.character(default)
	}
}

#' Parse command line options.
#'
#' [parse_args()] parses command line options using an `OptionParser`
#' instance for guidance. [parse_args2()] is a wrapper to [parse_args()]
#' setting the options `positional_arguments` and `convert_hyphens_to_underscores`
#' to `TRUE`.
#'
#' @param object An `OptionParser` instance.
#' @param args A character vector containing command line options to be parsed.
#'     Default is everything after the Rscript program in the command line. If
#'     `positional_arguments` is not `FALSE` then [parse_args()] will
#'     look for positional arguments at the end of this vector.
#' @param print_help_and_exit Whether [parse_args()] should call
#'     [print_help()] to print out a usage message and exit the program.  Default
#'     is `TRUE`.
#' @param positional_arguments Number of \emph{positional} arguments.  A numeric
#'     denoting the exact number of supported arguments, or a numeric vector of
#'     length two denoting the minimum and maximum number of arguments
#'     (`Inf` for no limit).  The value `TRUE` is equivalent to
#'     `c(0, Inf)`.  The default `FALSE` is
#'     supported for backward compatibility only, as it alters
#'     the format of the return value.
#' @param convert_hyphens_to_underscores If the names in the returned list of options
#'      contains hyphens then convert them to underscores.  The default `FALSE` is
#'      supported for backward compatibility reasons as it alters the format of the return value
#' @return Returns a list with field `options` containing our option values
#'     as well as another field `args` which contains a vector of
#'     positional arguments.  For backward compatibility, if and only if
#'     `positional_arguments` is `FALSE`, returns a list containing
#'     option values.
#' @section Errors:
#'   The following classed errors may be thrown:
#'
#'   - `optparse_parse_error`: base class for all parse-time errors.
#'     - `optparse_bad_option_error`: unrecognized, misused, or
#'       argument-requiring option.
#'       - `optparse_ambiguous_option_error`: ambiguous abbreviated long
#'         flag.
#'     - `optparse_bad_positional_arguments_error`: wrong number of
#'       positional arguments supplied.
#'     - `optparse_missing_required_error`: a required option was not supplied.
#'
#' @seealso [OptionParser()] [print_help()]
#' @references Python's `optparse` library, which inspired this package,
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
#' @export
parse_args <- function(
	object,
	args = commandArgs(trailingOnly = TRUE),
	print_help_and_exit = TRUE,
	positional_arguments = FALSE,
	convert_hyphens_to_underscores = FALSE
) {
	tryCatch(
		parse_args_helper(
			object,
			args,
			print_help_and_exit,
			positional_arguments,
			convert_hyphens_to_underscores
		),
		error = function(e) pa_stop(object, e)
	)
}

quieter_error_handler <- function(e) {
	quit('no', status = 1, runLast = FALSE)
}

bad_option_error_stop <- function(msg) {
	stop(errorCondition(
		msg,
		class = c("optparse_bad_option_error", "optparse_parse_error"),
		call = NULL
	))
}

ambiguous_option_error_stop <- function(msg) {
	stop(errorCondition(
		msg,
		class = c(
			"optparse_ambiguous_option_error",
			"optparse_bad_option_error",
			"optparse_parse_error"
		),
		call = NULL
	))
}

missing_required_error_stop <- function(msg) {
	stop(errorCondition(
		msg,
		class = c("optparse_missing_required_error", "optparse_parse_error"),
		call = NULL
	))
}

bad_positional_arguments_error_stop <- function(msg) {
	stop(errorCondition(
		msg,
		class = c("optparse_bad_positional_arguments_error", "optparse_parse_error"),
		call = NULL
	))
}

pa_stop <- function(object, e) {
	extra_classes <- setdiff(class(e), c("simpleError", "error", "condition"))
	classes <- unique(c(extra_classes, "optparse_parse_error"))
	cnd <- errorCondition(
		e$message,
		call = "optparse::parse_args_helper()",
		class = classes
	)
	if (is_interactive()) {
		stop(cnd)
	} else {
		signalCondition(cnd)
		msg <- paste0("\n", get_Rscript_filename(), ": error: ", e$message)
		cat(object@usage, msg, sep = "\n", file = stderr())
		opt <- options(
			error = getOption("error", quieter_error_handler),
			show.error.messages = FALSE
		)
		on.exit(options(opt))
		stop(cnd)
	}
}

parse_args_helper <- function(
	object,
	args = commandArgs(trailingOnly = TRUE),
	print_help_and_exit = TRUE,
	positional_arguments = FALSE,
	convert_hyphens_to_underscores = FALSE
) {
	validObject(object)

	pal <- should_include_any_args(positional_arguments)
	include_any_args <- pal$include_any_args
	positional_arguments <- pal$positional_arguments

	operand <- if (include_any_args) "strict" else "after--only"
	opt <- getopt_options(object, args, operand)

	arguments_positional <- character(0)
	if (include_any_args && length(args)) {
		oa <- getoperand(opt)
		if (!is.null(oa)) arguments_positional <- oa
	}

	options_list <- parse_options(object, opt, convert_hyphens_to_underscores)

	if (any(grepl("^help$", names(options_list)))) {
		if (options_list[["help"]] && print_help_and_exit) {
			print_help(object)
			if (is_interactive()) {
				stop("help requested")
			} else {
				quit(status = 0)
			}
		}
	}

	missing_required <- character(0)
	for (option in object@options) {
		if (isTRUE(option@required) && is.null(options_list[[option@dest]])) {
			missing_required <- c(missing_required, option@long_flag)
		}
	}
	if (length(missing_required)) {
		missing_required_error_stop(paste(
			"the following arguments are required:",
			paste(missing_required, collapse = ", ")
		))
	}

	if (length(arguments_positional) < min(positional_arguments)) {
		bad_positional_arguments_error_stop(sprintf(
			"required at least %g positional arguments, got %g",
			min(positional_arguments),
			length(arguments_positional)
		))
	}
	if (length(arguments_positional) > max(positional_arguments)) {
		bad_positional_arguments_error_stop(sprintf(
			"required at most %g positional arguments, got %g",
			max(positional_arguments),
			length(arguments_positional)
		))
	}
	if (include_any_args) {
		return(list(options = options_list, args = arguments_positional))
	} else {
		return(options_list)
	}
}


getopt_options <- function(object, args, operand = "after--only") {
	# Convert our option specification into ``getopt`` format
	n_options <- length(object@options)
	spec <- matrix(NA, nrow = n_options, ncol = 6)
	for (ii in seq_along(object@options)) {
		spec[ii, ] <- convert_to_getopt(object@options[[ii]])
	}

	# Pre-seed result with defaults; last default wins for shared dest (matching Python's optparse)
	defaults <- list()
	constants <- list()
	for (ii in seq_along(object@options)) {
		option <- object@options[[ii]]
		if (option@action %in% c("store_const", "append_const")) {
			constants[[sub("^--", "", option@long_flag)]] <- option@const
		}
		if (option@action == "callback" || is.null(option@default)) {
			next
		}
		defaults[[option@dest]] <- option@default
	}

	if (length(args)) {
		opt <- getopt(
			spec = spec,
			opt = args,
			operand = operand,
			defaults = defaults,
			constants = constants
		)
	} else {
		opt <- defaults
	}
	opt
}

should_include_any_args <- function(positional_arguments) {
	# pull out positional arguments if ``positional_arguments`` was set to TRUE
	# or not 0 or c(0, 0)
	if (!(length(positional_arguments) %in% 1L:2L)) {
		stop("positional_arguments must have length 1 or 2")
	}
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
	list(include_any_args = include_any_args, positional_arguments = positional_arguments)
}

parse_options <- function(object, opt, convert_hyphens_to_underscores) {
	options_list <- list()
	for (ii in seq_along(object@options)) {
		option <- object@options[[ii]]
		option_value <- opt[[option@dest]]
		if (option@action == "callback") {
			if (!is.null(option_value)) {
				options_list[[option@dest]] <- option_value
				callback_fn <- function(...) {
					option@callback(option, option@long_flag, option_value, object, ...) # nolint
				}
				options_list[[option@dest]] <- do.call(callback_fn, option@callback_args)
			} else if (!is.null(option@default)) {
				options_list[[option@dest]] <- option@default
			}
		} else if (!is.null(option_value)) {
			options_list[[option@dest]] <- option_value
		}
	}
	if (convert_hyphens_to_underscores) {
		names(options_list) <- gsub("-", "_", names(options_list))
	}
	options_list
}

#' @rdname parse_args
#' @export
parse_args2 <- function(
	object,
	args = commandArgs(trailingOnly = TRUE),
	print_help_and_exit = TRUE
) {
	parse_args(
		object,
		args = args,
		print_help_and_exit = print_help_and_exit,
		positional_arguments = TRUE,
		convert_hyphens_to_underscores = TRUE
	)
}

# Converts our representation of options to format getopt can understand
convert_to_getopt <- function(object) {
	short_flag <- sub("^-", "", object@short_flag)
	long_flag <- sub("^--", "", object@long_flag)
	action <- if (
		object@action %in% c("count", "append", "store_false", "store_const", "append_const")
	) {
		object@action
	} else if (option_needs_argument(object)) {
		"store"
	} else {
		"store_true"
	}
	type <- ifelse(object@type == "NULL", "logical", object@type)
	return(c(long_flag, short_flag, action, type, object@help, object@dest))
}
option_needs_argument <- function(option) {
	option_needs_argument_helper(option@action, option@type)
}
option_needs_argument_helper <- function(action, type) {
	switch(action, store = TRUE, append = TRUE, callback = !(type == "NULL"), FALSE)
}
