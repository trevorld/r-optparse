# Use h option for non-help

    Code
      OptionParser(usage = "\\%prog [options] file", option_list = option_list_neg)
    Condition
      [1m[33mError[39m in `validObject()`:[22m
      [33m![39m invalid class “OptionParser” object: duplicate short flag: -h (did you forget to set `add_help_option = FALSE` in `OptionParser()`?)

