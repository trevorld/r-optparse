optparse 1.7.5
==============

* We no longer coerce the type of an option `default` to match that of
  its `type` argument when `action = "callback"`.
  Thanks husheng (@hs3434) for bug report (#47).

optparse 1.7.4
==============

* Revises vignette engine specification in the `DESCRIPTION` to suppress new CRAN check NOTE (#43).

optparse 1.7.3
==============

* The errors raised by `parse_args()` (and `parse_args2()`) are now of class "optparse\_parse\_error".

  When `interactive()` is `FALSE` we now print out a usage string followed by 
  a (less verbose) error message.

* Throws a more informative error message for unknown short flags when ``positional_arguments=TRUE``.
  Thanks Greg Minshall for bug report (#42).

optparse 1.7.1
==============

* Add a ``formatter`` argument to ``OptionParser()`` for a function to format the usage message (#30).
  By default uses the new function ``IndentedHelpFormatter()``.
  `{optparse}` also provides the new function ``TitledHelpFormatter()``.
  Thanks Ni Huang for suggestion.

optparse 1.6.6
==============

* Throws an error for unknown short flags and long flags when ``positional_arguments=TRUE``.
  Thanks Greg Minshall for bug report (#34).
* If the ``callback`` parameter of ``add_option`` / ``make_option`` is not ``NULL`` then
  the default of ``action`` is now ``"callback"``.  Thanks Greg Minshall for suggestion (#35).
* Better documentation of ``action=="callback"`` in the man page for ``add_option`` and ``make_option``.
  Thanks Greg Minshall for bug report (#35).

optparse 1.6.4
==============

* Fixes bug in printing help for ``action=="callback"`` when ``metavar==NULL`` (#29).
  Thanks Ni Huang for bug report.
* Throws an error when passed short flags more than one letter long (#32).
  Thanks Gautier Richard for bug report.

optparse 1.6.2
==============

* Fixs a parsing bug when ``action=="callback"`` and ``positional_argument==TRUE`` (#28).
  Thanks Ni Huang for bug report.

optparse 1.6.1
==============

* Improves accuracy of README (#27).  Thanks Alex Penson for bug report.

optparse 1.6.0
==============

* Supports callback actions (#26).  Thanks Gyu Jin Choi for patch.
* If ``interactive() == FALSE`` and ``print_help_and_exit == TRUE`` then
  ``optparse`` will now call ``quit(status=0)`` instead of ``quit(status=1)`` after
  printing the help message (this matches behaviour of the python package).
* Better error message when forgetting to set ``add_help_option=FALSE`` when
  defining an ``-h`` or ``--help`` flag.  Thanks Jeff P. Bruce for bug report.
* Increment ``getopt`` requirement so that empty strings are parsed correctly.
  Thanks Matthew Flickinger for bug report.

optparse 1.4.4
==============

* Minor documentation fixes.  Thanks J. J. Ramsey and Daeyoung Kim for bug reports.
* Fix bug when ``add_help_option`` in ``OptionParser`` set to ``FALSE``.  Thanks to Jeff Bruce for bug report.
* ``parse_args`` now supports ``convert_hyphens_to_underscores`` argument which converts any hyphens to underscores 
  when returning the list of options
* Now includes the convenience function ``parse_args2`` which wraps ``parse_args`` with ``positional_arguments`` set to ``TRUE``
  and ``convert_hyphens_to_underscores`` set to ``TRUE``.

optparse 1.3.2
==============

* ``optparse`` should no longer give any warnings when ``options(warnPartialMatchArgs=TRUE)``.  Thanks Rich FitzJohn for patch.
* ``print_help`` no longer throws an error if we have a default argument of length zero.  Thanks Benjamin Tyner for bug report.

optparse 1.3.0
==============

* ``OptionParser`` and ``OptionParserOption`` S4 classes are now exported.  Thanks Peter Humburg for patch.

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
