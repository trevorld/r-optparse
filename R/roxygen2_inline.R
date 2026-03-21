# Variables for use with roxygen2 inline R code (` `r variable` `).
# This file is only needed during documentation generation and is listed in
# .Rbuildignore so it is not included in the installed package.
# See https://roxygen2.r-lib.org/articles/rd-formatting.html#inline-r-code

ro_action <- r"(A character string describing the action `optparse` should take when it encounters an option. One of:

* `"callback"`: stores the return value of the `callback` function.
* `"count"`: counts the number of times the flag is seen and adds it to `default` (treated as `0L` if not supplied). Returns `NULL` if never seen and no `default` was supplied.
* `"store"` (default): stores the specified following value.
* `"store_true"`: stores `TRUE` if the option is found.
* `"store_false"`: stores `FALSE` if the option is found.

If `callback` is not `NULL` the default action is `"callback"` otherwise it is `"store"`.
)"

ro_type <- r"(A character string specifying which data type to store: `"logical"`, `"integer"`, `"double"`, `"complex"`, or `"character"` (`"numeric"` is an alias for `"double"`). Defaults:

* if `action == "count"` then `"integer"`
* if `action %in% c("store_false", "store_true")` then `"logical"`
* if `action == "store"` and `default` is not `NULL` then `typeof(default)`else if `default` is `NULL` then `"character"`
)"

ro_dest <- r"(A character string specifying what field in the list returned by [parse_args()] should `optparse` store the option value. Default is derived from the long flag in `opt_str`.)"

ro_default <- r"(The default value `optparse` should use if it does not find the option on the command line.)"

ro_help <- r"(A character string describing the option, used by [print_help()] in generating a usage message. `"%default"` will be substituted by the value of `default`.)"

ro_metavar <- r"(A character string that stands in for the option argument when printing help text. Default is the value of `dest`.)"

ro_callback <- r"(A function that executes after the option value is fully parsed. Its return value is assigned to the option. Arguments are: the option S4 object, the long flag string, the value of the option, the parser S4 object, and `...`.)"

ro_callback_args <- r"(A list of additional arguments passed to `callback` (via [do.call()]).)"
