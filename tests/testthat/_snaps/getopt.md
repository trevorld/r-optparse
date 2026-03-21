# negative-number-looking bundle is passed as argument or expanded to short flags

    Code
      parse_args(parser, c("--mean=5", "-3.14"))
    Condition
      Error:
      ! short flag "3" is invalid

---

    Code
      parse_args(parser, c("-12"))
    Condition
      Error:
      ! short flag "1" is invalid

# type coercion warns when value cannot be converted to expected type

    Code
      parse_args(parser, c("--count", "foo"))
    Condition
      Warning in `value[[3L]]()`:
      integer expected, got "foo"
    Output
      $help
      [1] FALSE
      
      $count
      [1] "foo"
      

---

    Code
      parse_args(parser, c("--mean", ""))
    Condition
      Warning in `getopt()`:
      double expected, got ""
    Output
      $help
      [1] FALSE
      
      $mean
      [1] NA
      

# abbreviated long flag matching two options raises ambiguous error

    Code
      parse_args(parser, c("--verb"))
    Condition
      Error:
      ! long flag "verb" is ambiguous

