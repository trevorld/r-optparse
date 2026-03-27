# Copyright 2010-2019 Trevor L Davis <trevor.l.davis@gmail.com>
# Copyright 2013 Kirill Müller
# Copyright 2008 Allen Day
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

option_list <- list(
	make_option(
		c("-v", "--verbose"),
		action = "store_true",
		default = TRUE,
		help = "Print extra output [default]"
	),
	make_option(
		c("-q", "--quietly"),
		action = "store_false",
		dest = "verbose",
		help = "Print little output"
	),
	make_option(
		c("-c", "--count"),
		type = "integer",
		default = 5,
		help = "Number of random normals to generate [default \\%default]",
		metavar = "number"
	),
	make_option(
		"--generator",
		default = "rnorm",
		help = "Function to generate random deviates [default \"\\%default\"]"
	),
	make_option(
		"--mean",
		default = 0,
		help = "Mean if generator == \"rnorm\" [default \\%default]"
	),
	make_option(
		"--sd",
		default = 1,
		metavar = "standard deviation",
		help = "Standard deviation if generator == \"rnorm\" [default \\%default]"
	)
)
parser_ol <- OptionParser(option_list = option_list)


test_that("`make_option()` works as expected", {
	expect_equal(
		make_option("--integer", type = "integer", default = 5),
		make_option("--integer", default = as.integer(5))
	)
	expect_equal(
		make_option("--logical", type = "logical", default = "TRUE"),
		make_option("--logical", default = TRUE)
	)
	expect_equal(make_option("--filename")@type, "character")
	expect_snapshot(error = TRUE, make_option("badflag"))
	expect_snapshot(error = TRUE, make_option(c("-cd", "--foo")))
	expect_error(make_option("badflag"), class = "optparse_option_error")
	expect_error(make_option(c("-cd", "--foo")), class = "optparse_option_error")
	expect_equal(make_option(c("-1", "--one"))@short_flag, "-1")
	expect_equal(make_option("--1flag")@long_flag, "--1flag")
	expect_equal(make_option("--add-numbers")@long_flag, "--add-numbers")
	expect_snapshot(error = TRUE, make_option(c("- ", "--foo")))
	expect_snapshot(error = TRUE, make_option(c("-=", "--foo")))
	expect_snapshot(error = TRUE, make_option("--fo=o"))
	expect_snapshot(error = TRUE, make_option("--fo o"))
	expect_snapshot(error = TRUE, make_option("--foo", action = "storr"))
	expect_error(make_option("--foo", action = "storr"), class = "optparse_option_error")
	expect_snapshot(error = TRUE, make_option("--foo", type = "int"))
	expect_error(make_option("--foo", type = "int"), class = "optparse_option_error")
})

get_long_flags <- function(parser) {
	sort(sapply(parser@options, function(x) x@long_flag))
}
test_that("`add_option()` works as expected", {
	parser1 <- OptionParser(option_list = list(make_option("--generator"), make_option("--count")))
	parser2 <- OptionParser()
	parser2 <- add_option(parser2, "--generator")
	parser2 <- add_option(parser2, "--count")
	expect_equal(get_long_flags(parser1), get_long_flags(parser2))
})

test_that("`required` argument works as expected", {
	parser <- OptionParser()
	parser <- add_option(parser, "--foo", required = TRUE)
	expect_snapshot(error = TRUE, parse_args(parser, args = character(0)))
	expect_error(parse_args(parser, args = character(0)), class = "optparse_missing_required_error")
	expect_equal(
		parse_args(parser, args = "--foo=bar")[["foo"]],
		"bar"
	)
	parser2 <- OptionParser()
	parser2 <- add_option(parser2, "--foo", required = TRUE)
	parser2 <- add_option(parser2, "--bar", required = TRUE)
	expect_snapshot(error = TRUE, parse_args(parser2, args = character(0)))
})

test_that("`parse_args()` works as expected", {
	# option_list took outside test_that
	option_list2 <- list(
		make_option(
			c("-n", "--add-numbers"),
			action = "store_true",
			default = FALSE,
			help = "Print line number at the beginning of each line [default]"
		)
	)
	parser <- OptionParser(usage = "\\%prog [options] file", option_list = option_list2)
	expect_equal(
		sort_list(parse_args(parser_ol, args = c("--sd=3", "--quietly"))),
		sort_list(list(
			sd = 3,
			verbose = FALSE,
			help = FALSE,
			count = 5,
			mean = 0,
			generator = "rnorm"
		))
	)
	expect_equal(
		sort_list(parse_args(parser_ol, args = character(0), positional_arguments = TRUE)),
		sort_list(list(
			options = list(
				sd = 1,
				help = FALSE,
				verbose = TRUE,
				count = 5,
				mean = 0,
				generator = "rnorm"
			),
			args = character(0)
		))
	)
	expect_equal(
		sort_list(parse_args(parser_ol, args = c("-c", "10"))),
		sort_list(list(
			sd = 1,
			help = FALSE,
			verbose = TRUE,
			count = 10,
			mean = 0,
			generator = "rnorm"
		))
	)
	expect_equal(
		sort_list(parse_args(
			parser,
			args = c("--add-numbers", "example.txt"),
			positional_arguments = TRUE
		)),
		sort_list(list(options = list(`add-numbers` = TRUE, help = FALSE), args = c("example.txt")))
	)
	expect_equal(
		sort_list(parse_args(parser, args = c("--add-numbers"), positional_arguments = TRUE)),
		sort_list(list(options = list(`add-numbers` = TRUE, help = FALSE), args = character(0)))
	)
	expect_equal(
		sort_list(parse_args(
			parser,
			args = c("--add-numbers"),
			positional_arguments = TRUE,
			convert_hyphens_to_underscores = TRUE
		)),
		sort_list(list(options = list(add_numbers = TRUE, help = FALSE), args = character(0)))
	)
	expect_equal(
		sort_list(parse_args2(parser, args = c("--add-numbers"))),
		sort_list(list(options = list(add_numbers = TRUE, help = FALSE), args = character(0)))
	)
	expect_snapshot(
		error = TRUE,
		parse_args(parser, args = c("-add-numbers", "example.txt"), positional_arguments = FALSE)
	)
	expect_error(
		parse_args(parser, args = c("-add-numbers", "example.txt"), positional_arguments = FALSE),
		class = "optparse_bad_option_error"
	)
	expect_error(parse_args(
		parser,
		args = c("-add-numbers", "example.txt"),
		positional_arguments = TRUE
	))
	expect_equal(
		sort_list(parse_args(
			parser,
			args = c("--add-numbers", "example.txt"),
			positional_arguments = c(1, 3)
		)),
		sort_list(list(options = list(`add-numbers` = TRUE, help = FALSE), args = c("example.txt")))
	)
	expect_equal(
		sort_list(parse_args(parser, args = c("example.txt"), positional_arguments = 1)),
		sort_list(list(
			options = list(`add-numbers` = FALSE, help = FALSE),
			args = c("example.txt")
		))
	)
	expect_snapshot(
		error = TRUE,
		parse_args(parser, args = c("-add-numbers", "example.txt"), positional_arguments = c(0, 1))
	)
	expect_snapshot(
		error = TRUE,
		parse_args(parser, args = c("example.txt"), positional_arguments = c(2, Inf))
	)
	expect_error(
		parse_args(parser, args = c("example.txt"), positional_arguments = c(2, Inf)),
		class = "optparse_bad_positional_arguments_error"
	)
	expect_snapshot(
		error = TRUE,
		parse_args(parser, args = c("example.txt"), positional_arguments = 2)
	)
	expect_error(
		parse_args(parser, args = c("example.txt"), positional_arguments = 2),
		class = "optparse_bad_positional_arguments_error"
	)
	expect_snapshot(
		error = TRUE,
		parse_args(parser, args = c("example.txt"), positional_arguments = "any")
	)
	expect_snapshot(
		error = TRUE,
		parse_args(parser, args = c("example.txt"), positional_arguments = 1:3)
	)
})

test_that("--help raises an error in interactive mode", {
	parser <- OptionParser(
		usage = "%prog [options] file",
		option_list = list(
			make_option(c("-n", "--add-numbers"), action = "store_true", default = FALSE)
		)
	)
	local_mocked_bindings(is_interactive = function() TRUE)
	capture.output(expect_error(parse_args(parser, args = c("--help")), "help requested"))
	capture.output(expect_error(
		parse_args(parser, args = c("--help"), positional_arguments = c(1, 2)),
		"help requested"
	))
})

test_that("parse errors raise classed errors in interactive and non-interactive mode", {
	parser <- OptionParser()
	local_mocked_bindings(is_interactive = function() TRUE)
	expect_error(
		parse_args(parser, c("file.txt")),
		class = "optparse_bad_option_error"
	)
	local_mocked_bindings(is_interactive = function() FALSE)
	capture.output(
		expect_error(parse_args(parser, c("file.txt")), class = "optparse_bad_option_error"),
		type = "message"
	)
})

test_that("bare -- separator treats everything after it as positional arguments", {
	parser <- OptionParser()
	parser <- add_option(parser, c("-v", "--verbose"), action = "store_true")
	expect_equal(
		sort_list(parse_args(
			parser,
			c("--verbose", "--", "--not-a-flag", "-x"),
			positional_arguments = TRUE
		)),
		sort_list(list(
			options = list(verbose = TRUE, help = FALSE),
			args = c("--not-a-flag", "-x")
		))
	)
	expect_equal(
		sort_list(parse_args(parser, c("--", "--verbose"), positional_arguments = TRUE)),
		sort_list(list(options = list(help = FALSE), args = c("--verbose")))
	)
	expect_equal(
		sort_list(parse_args(parser, c("--", "-v"), positional_arguments = TRUE)),
		sort_list(list(options = list(help = FALSE), args = c("-v")))
	)
})

test_that("positional argument with positional_arguments = FALSE raises an error", {
	parser <- OptionParser()
	parser <- add_option(parser, c("-v", "--verbose"), action = "store_true")
	expect_snapshot(error = TRUE, parse_args(parser, c("file.txt")))
	expect_snapshot(error = TRUE, parse_args(parser, c("--verbose", "file.txt")))
})

# Patch from Gyu Jin Choi.
test_that("callback works as expected", {
	power <- function(x, n = 2) x^n
	callback_fn <- function(option, flag, option_value, parser, n = 2) {
		power(option_value, n)
	}

	parser0 <- OptionParser()
	parser1 <- add_option(
		parser0,
		c("-s", "--squared_distance"),
		type = "integer",
		action = "callback",
		help = "Squared distance between two points",
		callback = callback_fn,
		callback_args = list(2)
	)
	opts <- parse_args(parser1, args = c("--squared_distance=16"))
	expect_equal(opts$squared_distance, 256)
	# Bug found by Ni Huang (#28)
	opts <- parse_args(parser1, positional_argument = TRUE, args = c("-s", "3"))
	expect_equal(opts$options$squared_distance, 9)
	# Bug found by Ni Huang (#29)
	expect_output(print_help(parser1), "SQUARED_DISTANCE")

	parser2 <- add_option(
		parser0,
		c("-v", "--value"),
		type = "integer",
		action = "callback",
		callback = callback_fn,
		callback_args = list(n = 3)
	)
	opts <- parse_args(parser2, args = c("--value=2"))
	expect_equal(opts$value, 8)

	parser3 <- add_option(
		parser0,
		c("-v", "--value"),
		type = "integer",
		action = "callback",
		callback = callback_fn
	)
	opts <- parse_args(parser3, args = c("--value=2"))
	expect_equal(opts$value, 4)

	expect_warning(
		add_option(parser0, "--warning", action = "store", callback = as.list),
		"callback argument is supplied for non-callback action"
	)
	expect_warning(
		add_option(parser0, "--warning", callback_args = list(3, b = 4)),
		"callback_args argument is supplied for non-callback action"
	)
	expect_warning(
		add_option(parser0, "--warning", action = "callback", type = "numeric", callback = "hello"),
		"callback argument is not a function"
	)

	callback_fn <- function(option, flag, option_value, parser) {
		42
	}
	parser4 <- add_option(parser0, "--null_type", type = NULL, callback = callback_fn)
	opts <- parse_args(parser4, args = c("--null_type"))
	expect_equal(opts$null_type, 42)
})

# Bug found by Miroslav Posta
test_that("test using numeric instead of double", {
	option_list_neg <- list(make_option(c("-m", "--mean"), default = 0, type = "numeric"))
	parser <- OptionParser(usage = "\\%prog [options] file", option_list = option_list_neg)
	opts <- parse_args(parser, args = c("-m", "-5.0"))
	expect_equal(opts$mean, -5.0)
})

# Bug found by Juan Carlos Borrás
test_that("test bug of multiple '=' signs", {
	optlist <- list(
		make_option(
			c("-s", "--substitutions"),
			type = "character",
			dest = "substitutions",
			default = NULL,
			help = 'String of the form "KEY1=VALUE1 KEY2=VALUE2 ... KEY=VALUE"
    stating the SQL template substitutions',
			metavar = "substitution-list"
		)
	)
	optparser <- OptionParser(option_list = optlist)
	opt <- parse_args(optparser, c("-s", "FOO=bar"))
	opt_alt <- parse_args(optparser, c("--substitutions=FOO=bar"))
	expect_equal(opt, opt_alt)

	# also check when positional_arguments is set to true, like later bug unit test
	opt <- parse_args(optparser, c("-s", "FOO=bar"), positional_arguments = TRUE)
	opt_alt <- parse_args(optparser, c("--substitutions=FOO=bar"), positional_arguments = TRUE)
	expect_equal(opt, opt_alt)
})

# Bug found by Jim Nikelski
test_that("test bug when multiple short flag options '-abc' with positional_arguments = TRUE", {
	sort_list <- function(unsorted_list) {
		for (ii in seq_along(unsorted_list)) {
			if (is.list(unsorted_list[[ii]])) {
				unsorted_list[[ii]] <- sort_list(unsorted_list[[ii]])
			}
		}
		unsorted_list[sort(names(unsorted_list))]
	}
	expect_equal(
		sort_list(parse_args(parser_ol, args = c("-qc", "10"), positional_arguments = TRUE)),
		sort_list(list(
			options = list(
				sd = 1,
				help = FALSE,
				verbose = FALSE,
				count = 10,
				mean = 0,
				generator = "rnorm"
			),
			args = character(0)
		))
	)
	expect_error(parse_args(parser_ol, args = c("-qcde", "10"), positional_arguments = TRUE))
	expect_error(parse_args(
		parser_ol,
		args = c("a", "b", "c", "d", "e"),
		positional_arguments = c(1, 3)
	))
	expect_equal(
		sort_list(parse_args(
			parser_ol,
			args = c("CMD", "-qc", "10", "bumblebee"),
			positional_arguments = TRUE
		)),
		sort_list(list(
			options = list(
				sd = 1,
				help = FALSE,
				verbose = FALSE,
				count = 10,
				mean = 0,
				generator = "rnorm"
			),
			args = c("CMD", "bumblebee")
		))
	)
	args <- c("CMD", "-qc", "10", "bumblebee", "--qcdefg")
	expect_error(
		parse_args(parser_ol, args = args, positional_arguments = TRUE),
		'long flag "qcdefg" is invalid'
	)
	args <- c("-qxc", "10", "bumblebee")
	expect_error(
		parse_args(parser_ol, args = args, positional_arguments = TRUE),
		'short flag "x" is invalid'
	)
})

# Bug found by Ino de Brujin and Benjamin Tyner
test_that("test bug when long flag option with '=' with positional_arguments = TRUE", {
	expect_equal(
		sort_list(parse_args(parser_ol, args = c("--count=10"), positional_arguments = TRUE)),
		sort_list(list(
			options = list(
				sd = 1,
				help = FALSE,
				verbose = TRUE,
				count = 10,
				mean = 0,
				generator = "rnorm"
			),
			args = character(0)
		))
	)
})

# Bug found by Miroslav Posta
optlist <- list(make_option(c("--tmin"), type = "numeric", help = "Startup time [sec]. "))
parser <- OptionParser(option_list = optlist, usage = "", epilogue = "")
test_that("test bug with a NA short flag option with positional_arguments = TRUE", {
	expect_equal(
		sort_list(parse_args(
			args = c("-h", "foo"),
			parser,
			positional_arguments = TRUE,
			print_help_and_exit = FALSE
		)),
		sort_list(list(options = list(help = TRUE), args = "foo"))
	)
})

test_that("description and epilogue work as expected", {
	parser <- OptionParser()
	expect_output(print_help(parser), "Usage:")
	expect_output(print_help(parser), "Options:")
	parser2 <- OptionParser(usage = "program", description = "foo", epilogue = "bar")
	expect_output(print_help(parser2), "foo")
	expect_output(print_help(parser2), "bar$")
	expect_output(print_help(parser2), "^Usage: ")
	expect_equal(
		stringr::str_count(
			capture.output(print_help(OptionParser("usage: foo bar")))[1],
			"[Uu]sage"
		),
		1
	)

	parser <- OptionParser(formatter = TitledHelpFormatter)
	parser <- add_option(parser, c("-f", "--foo"), help = "Foobar")
	expect_output(print_help(parser), "Usage\n=====")

	# bug found by Stefan Seemayer for NA default
	optlist <- list(
		make_option(c("--na"), type = "character", default = NA, help = "NA default is %default"),
		make_option(
			c("--null"),
			type = "character",
			default = NULL,
			help = "NULL default is %default"
		),
		make_option(
			c("--str"),
			type = "character",
			default = "str",
			help = "str default is %default"
		),
		make_option(
			c("--bool"),
			type = "logical",
			default = TRUE,
			help = "bool default is %default"
		),
		make_option(c("--int"), type = "integer", default = 42, help = "int default is %default"),
		make_option(
			c("--dbl"),
			type = "double",
			default = 11.11,
			help = "double default is %default"
		)
	)
	parser <- OptionParser(option_list = optlist)
	expect_output(print_help(parser), "NA default is NA")
	expect_output(print_help(parser), "NULL default is NULL")
	expect_output(print_help(parser), "str default is str")
	expect_output(print_help(parser), "bool default is TRUE")
	expect_output(print_help(parser), "int default is 42")
	expect_output(print_help(parser), "double default is 11.11")

	# bug / feature request by Miroslav Posta
	parser <- OptionParser(
		usage = "test %prog test %prog",
		epilogue = "epilog test %prog %prog",
		description = "description %prog test %prog",
		prog = "unit_test.r"
	)
	expect_output(print_help(parser), "Usage:.*unit_test.r.*unit_test.r")
	expect_output(print_help(parser), "description unit_test.r test unit_test.r")
	expect_output(print_help(parser), "epilog test unit_test.r unit_test.r")
})

# Bug found by Benjamin Tyner
test_that("Can set zero length default options", {
	option_list_neg <- list(make_option(
		c("-m", "--mean"),
		default = numeric(0),
		type = "numeric",
		help = "Default %default"
	))
	parser <- OptionParser(usage = "\\%prog [options] file", option_list = option_list_neg)
	expect_equal(
		sort_list(parse_args(parser, args = c("-m", "-5.0"))),
		sort_list(list(mean = -5, help = FALSE))
	)
	expect_equal(sort_list(parse_args(parser)), sort_list(list(mean = numeric(0), help = FALSE)))
	expect_output(print_help(parser), "Default double")
})

# Bug found by Matthew Flickinger
test_that("Can parse empty string", {
	option_list <- list(make_option(c("", "--string")))
	parser <- OptionParser(usage = "\\%prog [options] file", option_list = option_list)
	expect_equal(
		sort_list(parse_args(parser, args = c("--string="))),
		sort_list(list(string = "", help = FALSE))
	)
})

# nolint start
# # Bug found by Rich FitzJohn
# oo <- options()
# on.exit(options(oo))
# options(warnPartialMatchArgs = TRUE)
# test_that("Avoid partial matching of arguments", {
#     expect_that(seq(along = 1:10), gives_warning("partial argument match"))
#     expect_that(seq_along(1:10), not(gives_warning()))
#     expect_that(parse_args(args = c("-h", "foo"), parser, positional_arguments = TRUE, print_help_and_exit = FALSE),
#                 not(gives_warning()))
#     expect_that(print_help(OptionParser()), not(gives_warning()))
# })
# nolint end

# Use h flag for non-help (Reported by Jeff Bruce)
test_that("Use h option for non-help", {
	local_reproducible_output()
	option_list_neg <- list(make_option(c("-h", "--mean"), default = 0.0))
	expect_snapshot(
		error = TRUE,
		OptionParser(usage = "\\%prog [options] file", option_list = option_list_neg)
	)
	expect_error(
		OptionParser(usage = "\\%prog [options] file", option_list = option_list_neg),
		class = "optparse_option_conflict_error"
	)
	expect_error(
		OptionParser(usage = "\\%prog [options] file", option_list = option_list_neg),
		class = "optparse_option_error"
	)

	option_list_neg <- list(make_option(c("-h", "--mean"), default = 0.0))
	parser <- OptionParser(
		usage = "\\%prog [options] file",
		option_list = option_list_neg,
		add_help_option = FALSE
	)
	args <- parse_args(parser, args = c("-h", "-5.0"))
	expect_equal(args, list(mean = -5.0))
})

test_that("no-argument actions reject --flag=value syntax", {
	parser <- OptionParser()
	parser <- add_option(parser, "--verbose", action = "store_true")
	parser <- add_option(parser, "--quiet", action = "store_false")
	parser <- add_option(parser, "--mode", action = "store_const", const = "fast")
	parser <- add_option(parser, "--tag", action = "append_const", const = "x", dest = "tags")
	parser <- add_option(parser, "--count", action = "count")
	expect_snapshot(error = TRUE, parse_args(parser, "--verbose=1"))
	expect_snapshot(error = TRUE, parse_args(parser, "--quiet=1"))
	expect_snapshot(error = TRUE, parse_args(parser, "--mode=1"))
	expect_snapshot(error = TRUE, parse_args(parser, "--tag=1"))
	expect_snapshot(error = TRUE, parse_args(parser, "--count=1"))
	expect_error(parse_args(parser, "--verbose=1"), class = "optparse_bad_option_error")
})

test_that("store_const action works", {
	parser <- OptionParser()
	parser <- add_option(parser, c("-v", "--verbose"), action = "store_const", const = 42L)
	expect_null(parse_args(parser, c())$verbose)
	expect_equal(parse_args(parser, c("-v"))$verbose, 42L)
	expect_equal(parse_args(parser, c("--verbose"))$verbose, 42L)
	parser2 <- add_option(
		OptionParser(),
		"--verbose",
		action = "store_const",
		const = 42L,
		default = 0L
	)
	expect_equal(parse_args(parser2, c())$verbose, 0L)
	expect_equal(parse_args(parser2, c("--verbose"))$verbose, 42L)
})

test_that("append_const action works", {
	parser <- OptionParser()
	parser <- add_option(parser, "--foo", action = "append_const", const = "foo", dest = "items")
	parser <- add_option(parser, "--bar", action = "append_const", const = "bar", dest = "items")
	expect_null(parse_args(parser, c())$items)
	expect_equal(parse_args(parser, c("--foo"))$items, "foo")
	expect_equal(parse_args(parser, c("--foo", "--bar"))$items, c("foo", "bar"))
	expect_equal(parse_args(parser, c("--bar", "--foo", "--bar"))$items, c("bar", "foo", "bar"))
	parser2 <- OptionParser()
	parser2 <- add_option(
		parser2,
		"--foo",
		action = "append_const",
		const = "foo",
		dest = "items",
		default = "default"
	)
	expect_equal(parse_args(parser2, c())$items, "default")
	expect_equal(parse_args(parser2, c("--foo"))$items, c("default", "foo"))
})

test_that("multiple append options sharing a dest accumulate in command line order", {
	parser <- OptionParser()
	parser <- add_option(parser, "--file", action = "append", dest = "inputs")
	parser <- add_option(parser, "--dir", action = "append", dest = "inputs")
	expect_equal(
		parse_args(parser, c("--dir", "mydir", "--file", "a.txt"))$inputs,
		c("mydir", "a.txt")
	)
})

test_that("append action works", {
	parser <- OptionParser()
	parser <- add_option(parser, c("-f", "--file"), action = "append")
	expect_null(parse_args(parser, c())$file)
	expect_equal(parse_args(parser, c("--file", "a.txt"))$file, "a.txt")
	expect_equal(parse_args(parser, c("--file=a.txt"))$file, "a.txt")
	expect_equal(parse_args(parser, c("--file", "a.txt", "-f", "b.txt"))$file, c("a.txt", "b.txt"))
	parser2 <- add_option(
		OptionParser(),
		c("-f", "--file"),
		action = "append",
		default = "default.txt"
	)
	expect_equal(parse_args(parser2, c())$file, "default.txt")
	expect_equal(parse_args(parser2, c("--file", "a.txt"))$file, c("default.txt", "a.txt"))
})

test_that("count action works", {
	parser <- OptionParser()
	parser <- add_option(parser, c("-v", "--verbose"), action = "count")
	expect_null(parse_args(parser, c())$verbose)
	expect_equal(parse_args(parser, c("-v"))$verbose, 1L)
	expect_equal(parse_args(parser, c("-v", "-v", "--verbose"))$verbose, 3L)
	parser2 <- add_option(OptionParser(), c("-v", "--verbose"), action = "count", default = 2L)
	expect_equal(parse_args(parser2, c())$verbose, 2L)
	expect_equal(parse_args(parser2, c("-v", "-v"))$verbose, 4L)
})

# Bug found by @husheng (#47)
test_that("Don't coerce `default` of callback action match that of `type`", {
	parser <- OptionParser()
	str2bool <- function(option, flag, option_value, parser) as.logical(option_value)
	parser <- add_option(parser, "--bool", type = "character", default = FALSE, callback = str2bool)
	expect_equal(parse_args(parser, c())$bool, FALSE)
	expect_equal(parse_args(parser, "--bool=T")$bool, TRUE)
})
