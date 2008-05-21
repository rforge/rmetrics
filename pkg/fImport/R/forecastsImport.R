
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
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port: 
#   1999 - 2008, Diethelm Wuertz, Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:             DESCRIPTION: 
#  forecastsImport       Downloads monthly data from www.forecasts.org 
#  forecastsSeries       Easy to use download from www.forecasts.org 
################################################################################


forecastsImport <-  
    function(query, file = "tempfile", frequency = "auto",
    from = NULL, to = Sys.timeDate(), nDaysBack = NULL,
    save = FALSE, sep = ";", try = TRUE) 
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Downloads monthly data from www.forecasts.org 
        
    # Examples:
    #   forecastsImport(query = "GOLD")
    #   forecastsImport(query = "MDISCRT")
    #   forecastsImport(query = "EXJPUS")
    #   forecastsImport(query = "GS3M")
    #   forecastsImport(query = "FEDFUNDS")

    # FUNCTION:
    
    # Settings:
    stopifnot(length(query) == 1)
    
    # Source:
    if (is.null(source)) 
        source = "http://www.forecasts.org/data/data/"
    
    # Download:
    if (try) {
        # Try for Internet Connection:
        z = try(forecastsImport(file = file, source = source, query = query, 
            save = save, try = FALSE))
        if (inherits(z, "try-error") || inherits(z, "Error")) {
            return("No Internet Access")
        } else {
            return(z) 
        } 
    } else { 
        # File Name:
        queryFile = paste(query, ".htm", sep = "")
        
        # Construct URL:
        url = paste(source, queryFile, sep = "")
        
        # Download file:
        download.file(url, file) 
        
        # Scan the file:
        x = scan(file, what = "", sep = "\n")
        
        # Extract dates ^19XX and ^20XX:
        x = x[regexpr("^[12][90]", x) > 0]
        # Write back to file:
        write(x, file)  
        
        # Read as data frame:
        x = read.table(file)
        
        # Two types of date strings are used %Y-%m-%d and %Y.%m
        # transform to %Y%m and paste the 28th to the format string:
        x[, 1] = substr(gsub("-", ".", as.vector(x[, 1])), 1, 7)
        charvec = as.character(10000*as.numeric(x[, 1]) + 1)
        rowNames = as.character(timeLastDayInMonth(charvec, format = "%Y%m%d"))
        x = data.frame(cbind(rowNames, as.numeric(x[, -1])))
        colnames(x) = c("DATE", query)
    }
    
    # Save Download ?
    if (save) {
        # Header:
        write.table(paste("%Y-%m-%d;", query, sep = sep), file, 
            quote = FALSE, row.names = FALSE, col.names = FALSE)
        # Data:
        write.table(x, file, quote = FALSE, append = TRUE, 
            col.names = FALSE, sep = sep) 
    } else {
        unlink(file) 
    }  
    
    # Result:
    ans = new("fWEBDATA",     
        call = match.call(),
        param = c("Instrument Query" = query),
        data = x,
        title = "Data Import from www.forecasts.org", 
        description = description() )
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


forecastsSeries <-  
    function(symbols, from = NULL, to = Sys.timeDate(), nDaysBack = 366, ...)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Downloads easily time series data from www.forecasts.org
    
    # Arguments:
        
    # Examples:
    #   forecastsSeries(query = "GOLD")
    #   forecastsSeries(query = "MDISCRT")
    #   forecastsSeries(query = "EXJPUS")
    #   forecastsSeries(query = "GS3M")
    #   forecastsSeries(query = "FEDFUNDS")

    # FUNCTION:
    
    # Settings:
    query = symbols
        
    # Download:
    Y = forecastsImport(query = query[1], ...)@data
    X = as.timeSeries(Y)
    N = length(query)
    if (N > 1) {
        for (i in 2:N) {
            Y = forecastsImport(query = query[i], ...)@data   
            X = merge(X, as.timeSeries(Y))
        }
    }    
    colnames(X)<-query
    
    # Time Window:
    if (is.null(from)) from = to - nDaysBack*24*3600
    X = window(X, from, to)
    
    # Return Value:
    X  
}


################################################################################

