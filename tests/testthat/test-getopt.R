test_that("flag that requires an argument raises optparse_bad_option_error", {
	parser <- OptionParser()
	parser <- add_option(parser, c("-n", "--number"), type = "integer")
	expect_error(parse_args(parser, c("--number")), class = "optparse_bad_option_error")
	expect_error(parse_args(parser, c("-n")), class = "optparse_bad_option_error")
})

test_that("negative-number-looking bundle is passed as argument or expanded to short flags", {
	parser <- OptionParser()
	parser <- add_option(parser, c("-m", "--mean"), type = "double")
	# short flag preceding negative number bundle: treat as argument value (line 210-213)
	expect_equal(parse_args(parser, c("-m", "-12"))$mean, -12)
	# long flag without = preceding negative number bundle: treat as argument value (line 207)
	expect_equal(parse_args(parser, c("--mean", "-3.14"))$mean, -3.14)
	# long flag with = preceding negative number bundle: = already consumed value,
	# so prev_takes_argument() falls to else (line 215) and -3.14 is expanded → error
	expect_snapshot(error = TRUE, parse_args(parser, c("--mean=5", "-3.14")))
	expect_error(parse_args(parser, c("--mean=5", "-3.14")), class = "optparse_bad_option_error")
	# standalone: expanded to -1, -2 which are invalid short flags
	expect_snapshot(error = TRUE, parse_args(parser, c("-12")))
	expect_error(parse_args(parser, c("-12")), class = "optparse_bad_option_error")
})

test_that("type coercion warns when value cannot be converted to expected type", {
	parser <- OptionParser()
	parser <- add_option(parser, "--count", type = "integer")
	parser <- add_option(parser, "--mean", type = "double")
	# "foo" triggers the warning inside storage.mode() itself (line 150)
	expect_snapshot(parse_args(parser, c("--count", "foo")))
	# "" converts silently to NA_real_ without warning, triggering the post-coercion NA check (lines 154-157)
	expect_snapshot(parse_args(parser, c("--mean", "")))
})

test_that("abbreviated long flag matching two options raises ambiguous error", {
	parser <- OptionParser()
	parser <- add_option(parser, "--verbose", action = "store_true")
	parser <- add_option(parser, "--verbosity", type = "integer")
	expect_snapshot(error = TRUE, parse_args(parser, c("--verb")))
	expect_error(parse_args(parser, c("--verb")), class = "optparse_ambiguous_option_error")
	expect_error(parse_args(parser, c("--verb")), class = "optparse_bad_option_error")
	expect_error(parse_args(parser, c("--verb")), class = "optparse_parse_error")
})

test_that("get_Rscript_filename() ignores --file= after --args", {
	local_mocked_bindings(
		command_args = function() c("Rscript", "--file=script.R", "--args", "--file=fake.R")
	)
	expect_equal(get_Rscript_filename(), "script.R")
})

test_that("get_Rscript_filename() works with no --args", {
	local_mocked_bindings(
		command_args = function() c("Rscript", "--file=script.R")
	)
	expect_equal(get_Rscript_filename(), "script.R")
})

test_that("get_Rscript_filename() returns NA when no --file=", {
	local_mocked_bindings(
		command_args = function() character(0)
	)
	expect_equal(get_Rscript_filename(), NA_character_)
})

test_that("get_Rscript_filename() returns NA when no --file= before --args", {
	local_mocked_bindings(
		command_args = function() c("R", "--args", "--file=boo.r")
	)
	expect_equal(get_Rscript_filename(), NA_character_)
})

test_that("get_Rscript_filename() returns LITTLER_SCRIPT_PATH when set", {
	local_mocked_bindings(
		littler_script_path = function() "script.r"
	)
	expect_equal(get_Rscript_filename(), "script.r")
})
