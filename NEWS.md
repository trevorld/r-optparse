optparse 1.6.0
==============

* Support for callback actions (#26).  Thanks Gyu Jin Choi for patch.

optparse 1.5.0
==============

* If ``interactive() == FALSE`` and ``print_help_and_exit == TRUE'' then
  ``optparse`` will now call ``quit(status=0)`` instead of ``quit(status=1)`` after
  printing the help message (this matches behaviour of the python package).
* Better help message when forgetting to set ``add_help_option=FALSE`` when
  defining an ``-h``` or ``-help`` flag.  Thanks Jeff P. Bruce for bug report.
* Increment ``getopt`` requirement so that empty strings are parsed correctly.
  Thanks Matthew Flickinger for bug report.

optparse 1.4.4
==============

* Minor documentation fixes.  Thanks jjramsey and Daeyoung Kim for bug reports.

optparse 1.4.1
==============

* Fix bug when ``add_help_option`` in ``OptionParser`` set to ``FALSE``.  Thanks to Jeff Bruce for bug report.

optparse 1.4.0
==============

* parse_args now supports convert_hyphens_to_underscores argument which converts any hyphens to underscores 
  when returning the list of options
* Now includes the convenience function parse_args2 which wraps parse_args with positional_arguments set to TRUE
  and convert_hyphens_to_underscores set to TRUE.

optparse 1.3.1
==============

* optparse should no longer give any warnings when options(warnPartialMatchArgs=TRUE).  Thanks Rich FitzJohn for patch.
* print_help no longer throws an error if we have a default argument of length zero.  Thanks Benjamin Tyner for bug report.

optparse 1.3.0
==============

* OptionParser and OptionParserOption are now exported.  Thanks Peter Humburg for patch.

optparse 1.2.0
==============

* Parameter ``positional_arguments`` of function ``parse_args`` now accepts one
  or two numeric values that denote the minimum and maximum number of supported
  positional arguments.
  Thanks Kirill Müller for patch.
* If ``interactive() == TRUE`` then ``parse_args`` will no longer ``quit(status=1)`` 
  after printing a help message but will instead throw an error.
  ``optparse`` will continue to ``quit(status=1)`` after printing a help message
  for non-interactive Rscripts unless ``print_help_and_exit == FALSE``.

optparse 1.1.0
==============

* In ``make_option`` argument ``type="numeric"`` automatically cast to ``double``.
  Previously users might have received an error passing negative numbers if they
  accidentally specified "numeric" instead of "double".
* Bug fixed in printing usage message for options with default value of NA 
  and a help string including "%default".
  Thanks Stefan Seemayer for bug report and patch.

optparse 1.0.2
==============

* Project website moved to https://github.com/trevorld/optparse
* We now replace all occurrences of %prog in usage message (including description and epilogue).
  Previously we would only replace one occurrence and didn't make replacements in description and epilogue.
* Fix bug in ``parse_args`` when we have options with no short flag and positional_arguments=TRUE.
  Thanks Miroslav Posta for bug report.

optparse 1.0.0
==============

* Added `description` and `epilogue` arguments to `OptionParser` to allow
  users to add more information to generated help messages
* Slightly alters the generated usage string 
  to match more closely what the Python module does
* No longer exports S4 classes that represent OptionParser and OptionParserOption
* Now requires package getopt (>= 1.19) which has also been moved to 
  Imports field from Depends field in DESCRIPTION
* Now also Suggests stringr package in DESCRIPTION
