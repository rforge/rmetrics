
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA 02111-1307 USA

# Copyrights (C)
# for this R-port: 
#   1999 - 2004, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:             DESCRIPTION:
#  xmpSeries             Sets prompt
#  xmpfSeries            Popups the example menu
################################################################################


sink("@sink@")
library(mgcv) # gam
library(nnet) # nnet
sink()
unlink("@sink@")
    
    
# ------------------------------------------------------------------------------


xmpSeries = 
function(prompt = "") 
{
    invisible(prompt)
}


# ------------------------------------------------------------------------------


xmpfSeries =
function() 
{   # A function implemented by Diethelm WUertz

    # Description:
    #   Popups the example menu
    
    # FUNCTION:
    
    # Popup:    
    path = paste(.Library,"/fSeries", sep = "") 
    entries = .read.fSeries.00Index (file.path(path, "demoIndex"))  
    example = select.list(entries[,1])
    selected = 0
    for (i in 1:length(entries[,1])) {
        if (example == entries[i,1]) selected = i
    }
    if (example == "") {
        cat("\nNo demo selected\n")
    } else {
        cat("\nLibrary: ", "fSeries", "\nExample: ", 
            entries[selected, 1], "\nTitle:   ", entries[selected, 2], "\n")
        source(paste(path, "/demo/", example, ".R", sep = ""))
    }
    if(TRUE) {
        cat("\n")
    }
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.read.fSeries.00Index = 
function (file) 
{
    if (is.character(file)) {
        if (file == "") {
            file <- stdin()
        } else {
            file <- file(file, "r")
            on.exit(close(file))
        }
    }
    if (!inherits(file, "connection")) 
        stop(paste("argument", 
            sQuote("file"), "must be a character string or connection"))
    y <- matrix("", nr = 0, nc = 2)
    x <- paste(readLines(file), collapse = "\n")
    for (chunk in unlist(strsplit(x, "\n[       \n]*\n"))) {
        entries <- try({
            if (regexpr("(   |  )", chunk) == -1) 
                NULL
            else {
                chunk <- gsub("\n[      ]+", "  ", chunk)
                x <- strsplit(unlist(strsplit(chunk, "\n")), "[    ]")
                cbind(unlist(lapply(x, "[[", 1)), unlist(lapply(x, 
                    function(t) {
                        paste(t[-c(1, which(nchar(t) == 0))], collapse = " ")
                    })))
            }
        })
        if (!inherits(entries, "try-error") && NCOL(entries) == 2) 
            y <- rbind(y, entries)
    }
    colnames(y) <- c("Item", "Description")
    y
}

    
################################################################################

