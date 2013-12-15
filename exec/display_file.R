#!/usr/bin/env Rscript
# Copyright 2010-2013 Trevor L Davis <trevor.l.davis@stanford.edu>
# Copyright 2013 Kirill Müller 
#  
#  This file is free software: you may copy, redistribute and/or modify it  
#  under the terms of the GNU General Public License as published by the  
#  Free Software Foundation, either version 2 of the License, or (at your  
#  option) any later version.  
#  
#  This file is distributed in the hope that it will be useful, but  
#  WITHOUT ANY WARRANTY; without even the implied warranty of  
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  
#  General Public License for more details.  
#  
#  You should have received a copy of the GNU General Public License  
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.  
suppressPackageStartupMessages(library("optparse"))

option_list <- list( 
    make_option(c("-n", "--add_numbers"), action="store_true", default=FALSE,
        help="Print line number at the beginning of each line [default]")
    )
parser <- OptionParser(usage = "%prog [options] file", option_list=option_list)

arguments <- parse_args(parser, positional_arguments = 1)
opt <- arguments$options
file <- arguments$args

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
