#!/usr/bin/env Rscript
suppressPackageStartupMessages(library("optparse"))

option_list <- list( 
    make_option(c("-n", "--add_numbers"), action="store_true", default=FALSE,
        help="Print line number at the beginning of each line [default]")
    )
parser <- OptionParser(usage = "%prog [options] file", option_list=option_list)

arguments <- parse_args(parser, positional_arguments = TRUE)
opt <- arguments$options

if(length(arguments$args) != 1) {
    cat("Incorrect number of required positional arguments\n\n")
    print_help(parser)
    stop()
} else {
    file <- arguments$args
}

if( file.access(file) == -1) {
    stop(sprintf("Specified file ( %s ) does not exist", file))
} else {
    file_text <- readLines(file)
}

if(opt$add_numbers) {
    cat(paste(1:length(file_text), file_text), sep = "\n")
} else {
    cat(file_text, sep = "\n")
}
