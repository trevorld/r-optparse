# `make_option()` works as expected

    Code
      make_option("badflag")
    Condition
      Error in `make_option()`:
      ! We require a long flag option

---

    Code
      make_option("- ")
    Condition
      Error in `make_option()`:
      ! Short flag -  must not contain whitespace

---

    Code
      make_option("-=")
    Condition
      Error in `make_option()`:
      ! Short flag -= must not contain '='

---

    Code
      make_option("--fo=o")
    Condition
      Error in `make_option()`:
      ! Long flag --fo=o must not contain '='

---

    Code
      make_option("--fo o")
    Condition
      Error in `make_option()`:
      ! Long flag --fo o must not contain whitespace

# `required` argument works as expected

    Code
      parse_args(parser, args = character(0))
    Condition
      Error:
      ! the following arguments are required: --foo

---

    Code
      parse_args(parser2, args = character(0))
    Condition
      Error:
      ! the following arguments are required: --foo, --bar

# `parse_args()` works as expected

    Code
      parse_args(parser, args = c("-add-numbers", "example.txt"),
      positional_arguments = FALSE)
    Condition
      Error:
      ! short flag "a" is invalid

---

    Code
      parse_args(parser, args = c("-add-numbers", "example.txt"),
      positional_arguments = c(0, 1))
    Condition
      Error:
      ! short flag "a" is invalid

---

    Code
      parse_args(parser, args = c("example.txt"), positional_arguments = c(2, Inf))
    Condition
      Error:
      ! required at least 2 positional arguments, got 1

---

    Code
      parse_args(parser, args = c("example.txt"), positional_arguments = 2)
    Condition
      Error:
      ! required at least 2 positional arguments, got 1

---

    Code
      parse_args(parser, args = c("example.txt"), positional_arguments = "any")
    Condition
      Error:
      ! positional_arguments must be logical or numeric

---

    Code
      parse_args(parser, args = c("example.txt"), positional_arguments = 1:3)
    Condition
      Error:
      ! positional_arguments must have length 1 or 2

# positional argument with positional_arguments = FALSE raises an error

    Code
      parse_args(parser, c("file.txt"))
    Condition
      Error:
      ! "file.txt" is not a valid option

---

    Code
      parse_args(parser, c("--verbose", "file.txt"))
    Condition
      Error:
      ! "file.txt" is not a valid option

# Use h option for non-help

    Code
      OptionParser(usage = "\\%prog [options] file", option_list = option_list_neg)
    Condition
      Error in `validityMethod()`:
      ! duplicate short flag: -h (did you forget to set `add_help_option = FALSE` in `OptionParser()`?)

# no-argument actions reject --flag=value syntax

    Code
      parse_args(parser, "--verbose=1")
    Condition
      Error:
      ! long flag "verbose" accepts no arguments

---

    Code
      parse_args(parser, "--quiet=1")
    Condition
      Error:
      ! long flag "quiet" accepts no arguments

---

    Code
      parse_args(parser, "--mode=1")
    Condition
      Error:
      ! long flag "mode" accepts no arguments

---

    Code
      parse_args(parser, "--tag=1")
    Condition
      Error:
      ! long flag "tag" accepts no arguments

---

    Code
      parse_args(parser, "--count=1")
    Condition
      Error:
      ! long flag "count" accepts no arguments

