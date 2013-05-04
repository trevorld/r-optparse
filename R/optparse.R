# .expand_short_option based on function by Jim Nikelski (c) 2011
# He explicitly gave me a non-exclusive unlimited license to his patch

# Incorporates some patches by Steve Lianoglou (c) 2010
# He explicitly gave me a non-exclusive unlimited license to code in his patches

#' @import methods
setClass("OptionParser", representation(usage = "character", options = "list", 
                description="character", epilogue="character"))

setClass("OptionParserOption", representation(short_flag="character", 
                                    long_flag="character",
                                    action="character",
                                    type="character",
                                    dest="character",
                                    default="ANY",
                                    help="character",
                                    metavar="character"),)

#' A function to create an instance of a parser object
#'
#' This function is used to create an instance of a parser object
#' which when combined with the \code{parse_args}, \code{make_option}, and \code{add_option}
#' methods is very useful for parsing options from the command line.
#'
#' @param usage The program usage message that will printed out if
#'     \code{parse_args} finds a help option, \code{\%prog} is substituted with the
#'     value of the \code{prog} argument.
#' @param option_list A list of of \code{OptionParserOption} instances that will
#'     define how \code{parse_args} reacts to command line options.
#'     \code{OptionParserOption} instances are usually created by \code{make_option}
#'     and can also be added to an existing \code{OptionParser} instance via the
#'     \code{add_option} function.
#' @param add_help_option Whether a standard help option should be automatically
#'     added to the \code{OptionParser} instance.
#' @param prog Program name to be substituted for \code{\%prog} in the usage
#'     message (including description and epilogue if present),
#'     the default is to use the actual Rscript file name if called by an
#'     Rscript file and otherwise keep \code{\%prog}.
#' @param description  Additional text for \code{print_help} to print out between
#'     usage statement and options statement
#' @param epilogue  Additional text for \code{print_help} to print out after
#'     the options statement
#' @return An instance of the \code{OptionParser} class.
#' @author Trevor Davis.
#'
#' @seealso \code{\link{parse_args}} \code{\link{make_option}}
#'     \code{\link{add_option}}
#' @references Python's \code{optparse} library, which inspired this package,
#'    is described here: \url{http://docs.python.org/library/optparse.html}
#' @import getopt
#' @export 
OptionParser <- function(usage = "usage: %prog [options]", option_list=list(),
                            add_help_option=TRUE, prog=NULL, 
                            description="", epilogue="") {
    
    if(is.null(prog)) {
        prog <- get_Rscript_filename()
    }
    if(length(prog) && !is.na(prog)) {
        usage <- gsub("%prog", prog, usage)
        description <- gsub("%prog", prog, description)
        epilogue <- gsub("%prog", prog, epilogue)
    }
    # Match behavior of usage string in Python optparse package
    usage <- sub("^usage: ", "Usage: ", usage)
    usage <- ifelse(grepl("^Usage: ", usage), usage, sub("^", "Usage: ", usage))
    if(add_help_option) {
        option_list[[length(option_list) + 1]] <- 
            make_option(c("-h", "--help"),
                action="store_true", dest="help", default=FALSE,
                help="Show this help message and exit")
    }

    return(new("OptionParser", usage=usage, options=option_list, 
                    description=description, epilogue=epilogue))
}

#'Functions to enable our OptionParser to recognize specific command line
#'options.
#'
#'\code{add_option} adds a option to a prexisting \code{OptionParser} instance
#'whereas \code{make_option} is used to create a list of
#'\code{OptionParserOption} instances that will be used in the
#'\code{option_list} argument of the \code{OptionParser} function to create a
#'new \code{OptionParser} instance.
#'
#' @rdname add_make_option
#' @param object An instance of the \code{OptionParser} class
#' @param opt_str A character vector containing the string of the desired long
#'       flag comprised of \dQuote{--} followed by a letter and then a sequence of
#'       alphanumeric characters and optionally a string of the desired short flag
#'       comprised of the \dQuote{-} followed by a letter.
#' @param action A character string that describes the action \code{optparse}
#'       should take when it encounters an option, either \dQuote{store},
#'       \dQuote{store_true}, or \dQuote{store_false}.  The default is \dQuote{store}
#'       which signifies that \code{optparse} should store the specified following
#'       value if the option is found on the command string.  \dQuote{store_true}
#'       stores \code{TRUE} if the option is found and \dQuote{store_false} stores
#'       \code{FALSE} if the option is found.
#'  @param type A character string that describes specifies which data type
#'       should be stored, either \dQuote{logical}, \dQuote{integer}, \dQuote{double},
#'       \dQuote{complex}, or \dQuote{character}.  Default is \dQuote{logical} if
#'       \code{action %in% c("store_true", store_false)}, \code{typeof(default)} if
#'       \code{action == "store"} and default is not \code{NULL} and
#'       \dQuote{character} if \code{action == "store"} and default is \code{NULL}.
#'       @param dest A character string that specifies what field in the list returned
#'       by \code{parse_args} should \code{optparse} store option values.  Default is
#'       derived from the long flag in \code{opt_str}.
#'       @param default The default value \code{optparse} should use if it does not
#'       find the option on the command line.  Default is derived from the long flag
#'       in \code{opt_str}.
#' @param help A character string describing the option to be used by
#'     \code{print_help} in generating a usage message.  \code{\%default} will be
#'     substituted by the value of \code{default}.
#' @param metavar A character string that stands in for the option argument when
#'     printing help text.  Default is the value of \code{dest}.
#' @return Both \code{make_option} and \code{add_option} return instances of
#'     class \code{OptionParserOption}.
#' @author Trevor Davis.
#' 
#' @seealso \code{\link{parse_args}} \code{\link{OptionParser}}
#' @references Python's \code{optparse} library, which inspires this package,
#'     is described here: \url{http://docs.python.org/library/optparse.html}
#' @examples
#'
#'    make_option("--longflag")
#'    make_option(c("-l", "--longflag"))
#'    make_option("--integer", type="integer", default=5)
#'    make_option("--integer", default=as.integer(5))  # same as previous
#'
#'    # examples from package vignette
#'    make_option(c("-v", "--verbose"), action="store_true", default=TRUE,
#'        help="Print extra output [default]")
#'    make_option(c("-c", "--count"), type="integer", default=5, 
#'        help="Number of random normals to generate [default %default]",
#'        metavar="number")
#'    make_option("--generator", default="rnorm", 
#'        help = "Function to generate random deviates [default \"%default\"]")
#'    make_option("--mean", default=0, 
#'        help="Mean if generator == \"rnorm\" [default %default]")
#'    make_option("--sd", default=1, metavar="standard deviation",
#'        help="Standard deviation if generator == \"rnorm\" [default %default]")
#'
#' @export
make_option <- function(opt_str, action="store", type=NULL,
                     dest=NULL, default=NULL, help="", metavar=NULL) {
    short_flag <- opt_str[grepl("^-[[:alpha:]]", opt_str)]
    if(length(short_flag)) {} else { short_flag <- as.character(NA) }
    long_flag <- opt_str[grepl("^--[[:alpha:]]", opt_str)]
    if(length(long_flag)) {} else {stop("We require a long flag option")}

    if(is.null(type)) {
        if( action %in% c("store_true", "store_false") ) {
            type <- "logical"
        }
        else {
            if( action %in% c("store") ) {
                if (is.null(default)) {
                    type <- "character"
                } else {
                    type <- typeof(default)
                }
            }
        }
    }
    if((type != typeof(default)) & !is.null(default)) {
        storage.mode(default) <- type
    }
    if(is.null(dest)) { dest <- sub("^--", "", long_flag) }
    if(is.null(metavar)) {
        if(action == "store") { 
            metavar <- sub("^--", "", long_flag)
        } else {
            metavar <- character(0)
        }
    }
        
    return(new("OptionParserOption", short_flag=short_flag, long_flag=long_flag,
                        action=action, type=type, dest=dest, default=default, 
                        help=help, metavar=metavar))
}
#' @rdname add_make_option
#' @export
add_option <- function(object, opt_str, action="store", type=NULL, 
                    dest=NULL, default=NULL, help="", metavar=NULL) {
    options_list <- object@options
    n_original_options <- length(options_list)
    options_list[[n_original_options + 1]] <- make_option(opt_str=opt_str,
                                           action=action, type=type, dest=dest,
                                           default=default, help=help, metavar=metavar)        
    object@options <- options_list
    return(object)
}

#'Printing an usage message from an OptionParser object
#'
#'\code{print_help} print an usage message from an OptionParser object, usually
#'called by \code{parse_args} when it encounters a help option.
#'
#'
#'@param object A \code{OptionParser} instance.
#'@return \code{print_help} uses the \code{cat} function to print out a usage
#'message.  It returns \code{invisible(NULL)}.
#'@author Trevor Davis.
#'
#'@seealso \code{{parse_args}} \code{{OptionParser}}
#'@references Python's \code{optparse} library, which inspired this package,
#'     is described here: \url{http://docs.python.org/library/optparse.html}
#' @export
print_help <- function(object) {
    cat(object@usage, fill = TRUE)
    cat(object@description, fill=TRUE)
    cat("\n")
    cat("Options:", sep="\n")    

    options_list <- object@options
    for(ii in seq(along=options_list)) {
        option <- options_list[[ii]]
        cat("\t")
        if(!is.na(option@short_flag)) {
            cat(option@short_flag)
            if( option@action == "store" ) {
                cat(" ", toupper(option@metavar), sep="")
            }
            cat(", ")
        }
        if(!is.null(option@long_flag)) {
            cat(option@long_flag)
            if( option@action == "store" ) {
                cat("=", toupper(option@metavar), sep="")
            }
        }
        cat("\n\t\t")
        default <- as.character(option@default)
        default_str <- ifelse(length(default), default, "NULL")
        cat(sub("%default", default_str, option@help))
        cat("\n\n")
    }
    cat(object@epilogue, fill=TRUE)
    return(invisible(NULL))
}

#' Parse command line options.
#' 
#' \code{parse_args} parses command line options using an \code{OptionParser}
#' instance for guidance.
#' 
#' @param object An \code{OptionParser} instance.
#' @param args A character vector containing command line options to be parsed.
#'     Default is everything after the Rscript program in the command line. If
#'     \code{positional_arguments} is \code{TRUE} then \code{parse_args} will only
#'     look for positional arguments at the end of this vector.
#' @param print_help_and_exit Whether \code{parse_args} should call
#'     \code{print_help} to print out a usage message and exit the program.  Default
#'     is \code{TRUE}.
#' @param positional_arguments Whether \code{parse_args} should look for and
#'     return a character vector of \emph{positional} arguments.  Default is
#'     \code{FALSE}.
#' @return Returns a list containing option values if
#'     \code{positional_arguments} is \code{FALSE} (the default).  Otherwise returns
#'     a list with field \code{options} containing our option values as well as
#'     another field \code{args} which contains a vector of positional arguments.
#' @section Acknowledgement: 
#'     A big thanks to Steve Lianoglou for a bug report and patch;
#'     Juan Carlos \enc{Borrás}{Borras} for a bug report; 
#'     Jim Nikelski for a bug report and patch; 
#'     Ino de Brujin and Benjamin Tyner for a bug report;
#'     Jonas Zimmermann for bug report; Miroslav Posta for bug reports.
#' @author Trevor Davis.
#'
#' @seealso \code{\link{OptionParser}} \code{\link{print_help}}
#' @references Python's \code{optparse} library, which inspired this package,
#'      is described here: \url{http://docs.python.org/library/optparse.html}
#' @encoding latin1
#' @examples
#' # example from vignette
#' option_list <- list( 
#'    make_option(c("-v", "--verbose"), action="store_true", default=TRUE,
#'        help="Print extra output [default]"),
#'    make_option(c("-q", "--quietly"), action="store_false", 
#'        dest="verbose", help="Print little output"),
#'    make_option(c("-c", "--count"), type="integer", default=5, 
#'        help="Number of random normals to generate [default %default]",
#'        metavar="number"),
#'    make_option("--generator", default="rnorm", 
#'        help = "Function to generate random deviates [default \"%default\"]"),
#'    make_option("--mean", default=0, 
#'        help="Mean if generator == \"rnorm\" [default %default]"),
#'    make_option("--sd", default=1, metavar="standard deviation",
#'        help="Standard deviation if generator == \"rnorm\" [default %default]")
#'    )
#'parse_args(OptionParser(option_list = option_list), args = c("--sd=3", "--quietly"))
#'
#'# example from vignette using positional arguments
#'option_list2 <- list( 
#'    make_option(c("-n", "--add_numbers"), action="store_true", default=FALSE,
#'        help="Print line number at the beginning of each line [default]")
#'    )
#'parser <- OptionParser(usage = "%prog [options] file", option_list=option_list2)
#'
#'parse_args(parser, args = c("--add_numbers", "example.txt"), positional_arguments = TRUE)
#'
#'parse_args(parser, args = c("-add_numbers", "example.txt"), positional_arguments = TRUE)
#'
#' @import getopt
#' @export 
parse_args <- function(object, args = commandArgs(trailingOnly = TRUE), 
                    print_help_and_exit = TRUE, positional_arguments = FALSE) {
    n_options <- length( object@options )

    # Convert our option specification into ``getopt`` format
    spec <- matrix(NA, nrow = n_options, ncol = 5)
    for (ii in seq(along = object@options)) {
        spec[ii, ] <- .convert_to_getopt( object@options[[ii]] )
    }

    # pull out positional arguments if ``positional_arguments`` was set to TRUE
    if(positional_arguments) {
        os_and_n_arg <- .get_option_strings_and_n_arguments(object)
        original_arguments <- args
        args <- NULL
        arguments_positional <- character(0)
        is_taken <- FALSE # set to true if optional argument needs to take next argument
        for(argument in original_arguments) {
            if(is_taken) {
                args <- c(args, argument)
                is_taken <- FALSE
            } else {
                if(.is_option_string(argument, object)) {
                    args <- c(args, argument)
                    if(.requires_argument(argument, object))
                        is_taken <- TRUE
                } else {
                    arguments_positional <- c(arguments_positional, argument)
                }          
            }
        }
    }

    options_list <- list()
    if(length(args)) {
        opt <- getopt(spec=spec, opt=args)
    } else {
        opt <- list()
    }

    for (ii in seq(along = object@options)) {
        option <- object@options[[ii]]
        option_value <- opt[[sub("^--", "", option@long_flag)]] 
        if( !is.null(option_value) ) {
            if ( option@action == "store_false" ) {
                options_list[[option@dest]] <- FALSE
            } else {    
                options_list[[option@dest]] <- option_value
            }
        } else {
            if( !is.null(option@default) & is.null(options_list[[option@dest]]) ) {
                options_list[[option@dest]] <- option@default  
            }
        }
    }
    if(options_list[["help"]] & print_help_and_exit) {
        print_help(object)
        quit(status=1)
    }
    if(positional_arguments) {
        return(list(options = options_list, args = arguments_positional))
    } else {    
        return(options_list)
    }
}

# Tells me whether a string is a valid option
.is_option_string <- function(argument, object) {
    if(.is_long_flag(argument)) {
        if(grepl("=", argument)) {
            argument <- sub("(.*?)=.*", "\\1", argument)
        }
        return(argument %in% .get_long_options(object))
    } else if(.is_short_flag(argument)) {
        return(all(.expand_short_option(argument) %in% .get_short_options(object)))
    } else {
        return(FALSE)
    }
}
# Tells me if an option string needs to take an argument
.requires_argument <- function(argument, object) {
    if(.is_long_flag(argument)) {
        if(grepl("=", argument)) {
            return(FALSE)
        } else {
            for (ii in seq(along = object@options)) {
                option <- object@options[[ii]]
                if(option@long_flag == argument)
                    return(option@action == "store") 
            }
        }
    } else { # is a short flag
        last_flag <- tail(.expand_short_option(argument), 1)
        for (ii in seq(along = object@options)) {
            option <- object@options[[ii]]
            if(!is.na(option@short_flag) && option@short_flag == last_flag)
                return(option@action == "store") 
        }
    }
}
# convenience functions that tells if argument is a type of flag and returns all long flag options or short flag options
.is_long_flag <- function(argument) { return(grepl("^--", argument)) }
.is_short_flag <- function(argument) { return(grepl("^-[^-]", argument)) }
.get_long_options <- function(object) {
    long_options <- vector("character")
    for(ii in seq(along = object@options)) {
        long_options <- c(long_options, object@options[[ii]]@long_flag)
    }
    return(long_options)
}
.get_short_options <- function(object) {
    short_options <- vector("character")
    for(ii in seq(along = object@options)) {
        short_options <- c(short_options, object@options[[ii]]@short_flag)
    }
    return(short_options)
}
# .expand_short_option based on function by Jim Nikelski (c) 2011
# He gave me a non-exclusive unlimited license to this code
# .expand_short_option("-cde") = c("-c", "-d", "-e")
.expand_short_option <- function(argument) {
    if(nchar(argument) == 2) {
        return(argument)
    } else {
        argument <- substr(argument, 2, nchar(argument)) # remove leading dash
        argument <- strsplit(argument, "")[[1]] # split into individual characters
        argument <- paste("-", argument, sep="") # add leading dash to each short option
        return(argument)
    }
}

# Returns a list with a field of option names and a field of number arguments option names take
.get_option_strings_and_n_arguments <- function(object) {
    option_strings <- vector("character")
    n_arguments <- vector("numeric")
    for (ii in seq(along = object@options)) {
        option <- object@options[[ii]]
        option_strings <- c(option_strings, option@short_flag)
        option_strings <- c(option_strings, option@long_flag)
        if (option@action == "store") {
            n_arguments <- c(n_arguments, 1, 1)
        } else {
            n_arguments <- c(n_arguments, 0, 0)
        }
    }
    return(list(option_strings = option_strings, n_arguments = n_arguments))
}

# Converts our representation of options to format getopt can understand
.convert_to_getopt <- function(object) {
    short_flag <- sub("^-", "", object@short_flag)
    long_flag <- sub("^--", "", object@long_flag)
    if( object@action %in% c("store_true", "store_false") ) {
        argument <- 0
    } else {
        argument <- 1
    }
    return( c( long_flag, short_flag, argument, object@type, object@help) )
}

