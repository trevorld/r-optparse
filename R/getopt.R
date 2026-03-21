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

# Fork of `getopt::getopt()` optimized for {optparse}
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
				if (action %in% c("store_true", "store_false", "count")) {
					stop(paste0('long flag "', this_flag, '" accepts no arguments'))
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
			stop(paste0('"', optstring, '" is not a valid option, or does not support an argument'))
		}

		long_name <- spec[rowmatch, COL_LONG_NAME]
		action <- spec[rowmatch, COL_ACTION]
		dest_name <- spec[rowmatch, COL_DEST]

		if (action == "store_const") {
			result[[dest_name]] <- constants[[long_name]]
			i <- i + 1L
			next
		} else if (action == "count") {
			result[dest_name] <- (result[[dest_name]] %||% 0L) + 1L
			i <- i + 1L
			next
		} else if (action %in% c("store_true", "store_false")) {
			result[dest_name] <- action != "store_false"
			i <- i + 1L
			next
		} else {
			# store or store_optional: peek ahead for argument
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
				stop(paste0('flag `', this_flag, '` requires an argument'))
			} else {
				# action == "store_optional"
				result[dest_name] <- TRUE
			}
		}
		i <- i + 1L
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
