
#
# Example:
#   A Compendium for R and Rmetrics users to the book 
#     "Analysis of Financial Time Series", Wiley 2002, 
#     written by Ruey Tsay
#   ISBN 0-471-41544-8 
#
# Details:
#   Downloadable from: http://www.gsb.uchicago.edu/fac/ruey.tsay/teaching/fts/
#
# Notes:
#   Diethelm Wuertz
#     www.rmetrics.org
#     www.itp.phys.ethz.ch
#     www.finance.ch
#
# Author:
#   (C) 2002-2004, Diethelm Wuertz, GPL
#


################################################################################


tsaySeries = 
function(query = "d-ibmln", file = "tempfile", 
source = "http://www.gsb.uchicago.edu/fac/ruey.tsay/teaching/fts/", 
save = FALSE, sepCSV = ";", try = TRUE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Downloads *.dat data sets from R Tsay's textbook
    
    # Value:
    #   A data frame.
        
    # Examples:
    #   tsaySeries(query = "d-ibmln")   1-column file
    #   tsaySeries(query = "d-vwew")    3-columns file

    # FUNCTION:
     
    # Download:
    if (try) {
        # Try for Internet Connection:
        z = try(tsaySeries(query = query, file = file, source = source, 
            save = save, try = FALSE))
        if (class(z) == "try-error" || class(z) == "Error") {
            return("No Internet Access") 
        } else {
            return(z) 
        } 
    } else { 
        # File name:
        queryFile = paste(query, ".dat", sep = "")
        
        # For S-Plus Compatibility:
        if (class(version) != "Sversion") {
            # R:
            method = NULL
        } else { 
            # SPlus
            method = "wget"
        }
    
        # Download and temporarily store:
        download.file(url = paste(source, queryFile, sep = ""), 
            destfile = file, method = method) 
        
        # Scan the file and transform into data frame:
        ans = read.table(file, ...)
        
        # Save download ?
        if (save) {
            write.table(z, file, quote = FALSE, col.names = FALSE, 
                sep = sepCSV) 
        } else {
            unlink(file) 
        } 
        
        # Return Value:
        return(ans)
    }
    
    # Return Value:
    invisible()
}


################################################################################

