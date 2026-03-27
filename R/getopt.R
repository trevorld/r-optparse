# Copyright (c) 2008-2010 Allen Day
# Copyright (c) 2011-2026 Trevor L. Davis <trevor.l.davis@gmail.com>
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

COL_LONG_NAME <- 1L
COL_SHORT_NAME <- 2L
COL_ACTION <- 3L
COL_MODE <- 4L
COL_DESCRIPTION <- 5L
COL_DEST <- 6L

# Fork of `getopt::getopt()` adapted for {optparse}
getopt <- function(
	spec = NULL,
	opt = NULL,
	command = get_Rscript_filename(),
	operand = "after--only",
	defaults = list(),
	constants = list()
) {
	operand <- match.arg(operand, c("after--only", "strict"))

	dashdash <- match("--", opt, nomatch = NA_integer_)
	if (!is.na(dashdash)) {
		dashdash_operands <- opt[seq_len(length(opt) - dashdash) + dashdash]
		opt <- opt[seq_len(dashdash - 1L)]
	} else {
		dashdash_operands <- NULL
	}
	inline_operands <- NULL

	opt <- normalize_opt(opt, spec)

	result <- defaults

	i <- 1L

	while (i <= length(opt)) {
		optstring <- opt[i]

		if (is_long_flag(optstring)) {
			optstring <- substring(optstring, 3)
			if (grepl("=", optstring)) {
				kv <- strsplit(optstring, "=")[[1L]]
				this_flag <- kv[1L]
				this_argument <- paste(kv[-1L], collapse = "=")
			} else {
				this_flag <- optstring
				this_argument <- NA
			}

			rowmatch <- get_rowmatch(spec, long = this_flag)

			# if we have an argument embedded via '='
			if (!is.na(this_argument)) {
				long_name <- spec[rowmatch, COL_LONG_NAME]
				action <- spec[rowmatch, COL_ACTION]
				dest_name <- spec[rowmatch, COL_DEST]
				# if we can't accept the argument, bail out
				if (
					action %in%
						c("append_const", "count", "store_const", "store_false", "store_true")
				) {
					bad_option_error_stop(paste0(
						'long flag "',
						this_flag,
						'" accepts no arguments'
					))
				} else if (action == "append") {
					result[[dest_name]] <- c(result[[dest_name]], this_argument)
					i <- i + 1L
					next
				} else {
					result[[dest_name]] <- this_argument
					i <- i + 1L
					next
				}
			}
		} else if (is_short_flag(optstring)) {
			this_flag <- substring(optstring, 2L)
			rowmatch <- get_rowmatch(spec, short = this_flag)
		} else if (operand == "strict") {
			inline_operands <- c(inline_operands, optstring)
			i <- i + 1L
			next
		} else {
			bad_option_error_stop(paste0('"', optstring, '" is not a valid option'))
		}

		long_name <- spec[rowmatch, COL_LONG_NAME]
		action <- spec[rowmatch, COL_ACTION]
		dest_name <- spec[rowmatch, COL_DEST]

		if (action == "store_const") {
			result[[dest_name]] <- constants[[long_name]]
			i <- i + 1L
			next
		} else if (action == "append_const") {
			result[[dest_name]] <- c(result[[dest_name]], constants[[long_name]])
			i <- i + 1L
			next
		} else if (action == "count") {
			result[[dest_name]] <- (result[[dest_name]] %||% 0L) + 1L
			i <- i + 1L
			next
		} else if (action %in% c("store_true", "store_false")) {
			result[[dest_name]] <- action != "store_false"
			i <- i + 1L
			next
		} else {
			# append or store: peek ahead for argument
			if (length(opt) > i) {
				peek_optstring <- opt[i + 1L]

				if (
					!is_long_flag(peek_optstring) &&
						(is_negative_number(peek_optstring) || !is_short_flag(peek_optstring))
				) {
					if (action == "append") {
						result[[dest_name]] <- c(result[[dest_name]], peek_optstring)
					} else {
						result[[dest_name]] <- peek_optstring
					}
					i <- i + 2L
					next
				}
			}
			if (action %in% c("store", "append")) {
				flag_kind <- ifelse(is_long_flag(opt[i]), "long", "short")
				bad_option_error_stop(paste0(
					flag_kind,
					' flag "',
					this_flag,
					'" requires an argument'
				))
			}
		}
	}

	for (j in seq_len(nrow(spec))) {
		dest_name <- spec[j, COL_DEST]
		if (is.character(result[[dest_name]])) {
			mode <- spec[j, COL_MODE]
			val <- result[[dest_name]]
			tryCatch(
				storage.mode(result[[dest_name]]) <- mode,
				warning = function(w) {
					warning(paste(mode, "expected, got", dQuote(val)))
				}
			)
			if (any(is.na(result[[dest_name]]) & !is.na(val))) {
				warning(paste(
					mode,
					"expected, got",
					paste(dQuote(val[is.na(result[[dest_name]]) & !is.na(val)]), collapse = ", ")
				))
			}
		}
	}
	structure(result, class = "getopt", operand = c(inline_operands, dashdash_operands))
}

getoperand <- function(x) {
	stopifnot(inherits(x, "getopt"))
	attr(x, "operand")
}

get_Rscript_filename <- function() {
	args <- command_args()
	args_idx <- match("--args", args)
	if (!is.na(args_idx)) {
		args <- args[seq_len(args_idx - 1L)]
	}
	prog <- sub("--file=", "", grep("^--file=", args, value = TRUE)[1L])
	if (is.na(prog)) {
		prog <- littler_script_path()
	}
	if (!is.na(prog) && .Platform$OS.type == "windows") {
		prog <- gsub("\\\\", "\\\\\\\\", prog)
	}
	prog
}

command_args <- function() commandArgs()

is_interactive <- function() interactive()

littler_script_path <- function() Sys.getenv("LITTLER_SCRIPT_PATH", unset = NA_character_)

is_negative_number <- function(x) {
	regexpr("^-[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", x) > 0L
}

is_long_flag <- function(x) {
	startsWith(x, "--") && nchar(x) > 2L
}

is_short_flag <- function(x) {
	startsWith(x, "-") && !startsWith(x, "--") && nchar(x) == 2L
}

is_bundle <- function(o) {
	startsWith(o, "-") && !startsWith(o, "--") && nchar(o) > 2L
}

expand_bundle <- function(o) {
	paste0("-", strsplit(substring(o, 2L), "")[[1L]])
}

prev_takes_argument <- function(prev, spec) {
	if (is_long_flag(prev) && !grepl("=", prev, fixed = TRUE)) {
		rowmatch <- tryCatch(get_rowmatch(spec, long = substring(prev, 3L)), error = function(e) {
			NA_integer_
		})
	} else if (is_short_flag(prev)) {
		rowmatch <- tryCatch(get_rowmatch(spec, short = substring(prev, 2L)), error = function(e) {
			NA_integer_
		})
	} else {
		return(FALSE)
	}
	actions <- c("append", "store")
	!is.na(rowmatch) && spec[rowmatch, COL_ACTION] %in% actions
}

normalize_opt <- function(opt, spec) {
	result <- character(0L)
	for (o in opt) {
		if (!is_bundle(o)) {
			result <- c(result, o)
		} else if (!is_negative_number(o)) {
			result <- c(result, expand_bundle(o))
		} else if (length(result) > 0L && prev_takes_argument(result[length(result)], spec)) {
			result <- c(result, o)
		} else {
			result <- c(result, expand_bundle(o))
		}
	}
	result
}

`%||%` <- function(x, y) if (is.null(x)) y else x

get_rowmatch <- function(spec, long = NULL, short = NULL) {
	if (!is.null(long)) {
		rowmatch <- grep(long, spec[, COL_LONG_NAME], fixed = TRUE)
		if (length(rowmatch) == 0L) {
			bad_option_error_stop(paste0('long flag "', long, '" is invalid'))
		} else if (length(rowmatch) > 1L) {
			rowmatch <- which(long == spec[, COL_LONG_NAME])
			if (length(rowmatch) == 0L) {
				ambiguous_option_error_stop(paste0('long flag "', long, '" is ambiguous'))
			}
		}
	} else {
		rowmatch <- grep(short, spec[, COL_SHORT_NAME], fixed = TRUE)
		if (length(rowmatch) == 0L) {
			bad_option_error_stop(paste0('short flag "', short, '" is invalid'))
		}
	}
	rowmatch
}

na_omit <- function(x) {
	Filter(Negate(is.na), x)
}

sort_list <- function(unsorted_list) {
	for (ii in seq(along = unsorted_list)) {
		if (is.list(unsorted_list[[ii]])) {
			unsorted_list[[ii]] <- sort_list(unsorted_list[[ii]])
		}
	}
	unsorted_list[sort(names(unsorted_list))]
}
