
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
#  myFinCenter           My financial Center
#  currentYear           Date of the Current Year
#  myUnits               Date Units
#  xmpCalendar           Sets prompt
#  xmpfCalendar          Popups the example menu
# FUNCTION:             DESCRIPTION:
#  modify                Modify a 'timeSeries' object
#  modify.default        S3 Default Method
#  atoms                 Extract atoms from 'timeSeries' object
#  atoms.default         S3 Default Method
################################################################################

    
# Default Parameters:
 
   
.currentYear = 
function()    
{
	if (class(version) != "Sversion") {
	    currentYear = as.POSIXlt(Sys.time())$year + 1900
	} else { 
    	currentDate = timeDate(date(), in.format="%w %m %d %H:%M:%S %Z %Y")
    	currentYear = as.integer(attr(years(currentDate), "levels"))
	} 
	currentYear 
}

    
	myFinCenter =  "Zurich"
	myUnits = "days"
	currentYear = .currentYear()  


# ------------------------------------------------------------------------------
    
    
# Set a timezone if none found in environment variables or options()
# ... as suggested by Dirk Eddelbuettel, thanks Dirk.

	if (class(version) != "Sversion") {
	    if (Sys.getenv("TZ") == "") {
	        if (is.null(getOption("TZ"))) {
	            cat("No timezone information found, using default of GMT\n")
	            Sys.putenv("TZ" = "GMT") 
	        } else {
	            cat("No timezone information found, applying option() value of",
	                getOption("TZ"), "\n")
	            Sys.putenv("TZ" = getOption("TZ")) 
	        } 
		}
	}


# ------------------------------------------------------------------------------


xmpCalendar = 
function(prompt = "") 
{   # A function implemented by Diethelm WUertz

    # Description:
    #   Sets prompt
    
    # FUNCTION:
    
    # Return Value:
    invisible(prompt)
}

    
# ------------------------------------------------------------------------------


xmpfCalendar = 
function() 
{   # A function implemented by Diethelm WUertz

    # Description:
    #   Popups the example menu
    
    # FUNCTION:
    
    # Popup:
    path = paste(.Library,"/fCalendar", sep = "") 
    entries = .read.fCalendar.00Index (file.path(path, "demoIndex"))    
    example = select.list(entries[,1])
    selected = 0
    for (i in 1:length(entries[,1])) {
        if (example == entries[i,1]) selected = i
    }
    if (example == "") {
        cat("\nNo demo selected\n")
    } else {
        cat("\nLibrary: ", "fCalendar", "\nExample: ", 
            entries[selected, 1], "\nTitle:   ", entries[selected, 2], "\n")
        source(paste(path, "/demo/", example, ".R", sep = ""))
    }
    if (TRUE) {
        cat("\n") 
    }
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.read.fCalendar.00Index = 
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


# ******************************************************************************
# R - Modifications and Problems:


modify =
function(x, method, units) 
{
    UseMethod("modify") 
}


# ------------------------------------------------------------------------------


modify.default =
function(x, method = c("sort", "round", "trunc"), units = NULL )
{   
    # Modify:
    ans = NA
    if (method[1] == "sort") return(sort(x))
    if (method[1] == "round") return(round(x))
    if (method[1] == "trunc") return(trunc(x))
    
    # Return Value:
    ans
}   


# ------------------------------------------------------------------------------


atoms = 
function(x, ...) 
{
    UseMethod("atoms")
}


# ------------------------------------------------------------------------------


atoms.default = 
function(x, ...) 
{
    invisible(x)
}


################################################################################

 