# `make_option()` works as expected

    Code
      make_option("badflag")
    Condition
      Error in `make_option()`:
      ! We require a long flag option

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

# Use h option for non-help

    Code
      OptionParser(usage = "\\%prog [options] file", option_list = option_list_neg)
    Condition
      Error in `validObject()`:
      ! invalid class "OptionParser" object: duplicate short flag: -h (did you forget to set `add_help_option = FALSE` in `OptionParser()`?)

