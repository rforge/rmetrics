
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
#  fredImport            Downloads market data from research.stlouisfed.org
#  fredSeries            Easy to use download from research.stlouisfed.org
################################################################################


fredImport <-  
    function(query, file = "tempfile", 
    source = "http://research.stlouisfed.org/fred2/series/", 
    frequency = "daily", save = FALSE, sep = ";", try = TRUE) 
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Downloads Monthly Market Data, Indices and Benchmarks from 
    #   St. Louis FED, "research.stlouisfed.org".
    
    # Value:
    #   An One Column data frame with row names denoting the dates
    #   given in the POSIX format "%Y%m%d". The column lists the
    #   downloaded data records.
        
    # Examples:
    #   fredImport("DPRIME")
    
    # Notes:
    #   This function is written for one-column daily data sets.
    #   Some example data sets include:  
    #     DEXUSEU   U.S. / Euro Foreign Exchange Rate 
    #     DEXSZUS   Switzerland / U.S. Foreign Exchange Rate 
    #     DGS1      1-Year Treasury Constant Maturity Rate 
    #     DPRIME    Bank Prime Loan Rate
    #      
  
    # FUNCTION:
    
    # Check:
    if (frequency != "daily")
        stop("Only daily dat records are supported!")
    
    # Download:
    if (try) {
        # Try for Internet Connection:
        z = try(fredImport(query = query, file = file, source = source, 
            save = save, try = FALSE))
        if (inherits(z, "try-error") || inherits(z, "Error")) {
            return("No Internet Access") 
        } else {
            return(z) 
        } 
    } else { 
        # File name:
        queryFile = paste(query, "/downloaddata/", query, ".txt", sep = "")
        
        # For S-Plus Compatibility:
        method <- if(is.R()) NULL else "wget"
    
        # Download and temporarily store:
        download.file(url = paste(source, queryFile, sep = ""), 
            destfile = file, method = method) 
        
        # Scan the file:
        x1 = scan(file, what = "", sep = "\n")
        # Extract dates ^19XX and ^20XX:
        x2 = x1[regexpr("^[12][90]", x1) > 0]
        x1 = x2[regexpr(" .$", x2) < 0]
        
        # Transform to one-column matrix:
        z = matrix(as.numeric(substring(x1, 11, 999)), byrow = TRUE, ncol = 1)
        
        # Add column names:
        colNames = query
        # DW: Change to ISO-8601
        # rowNames = paste(substring(x1, 1, 4), substring(x1, 6, 7), 
        #    substring(x1, 9, 10), sep = "")
        rowNames = substring(x1, 1, 10)
        
        # Save download ?
        if (save) {
            write.table(paste("%Y%m%d", query, sep = ";"), file, 
                quote = FALSE, row.names = FALSE, col.names = FALSE)
            write.table(z, file, quote = FALSE, append = TRUE, 
                col.names = FALSE, sep = ";") 
        } else {
            unlink(file) 
        } 
               
        # Return Value:
        X = data.frame(rowNames, z)
        colnames(X) = c("DATE", query)
        
        # Return Value:
        ans = new("fWEBDATA",     
            call = match.call(),
            param = c(
                "Instrument Query" = query,
                "Frequency" = frequency),
            data = X, 
            title = "Web Data Import from research.stlouisfed.org", 
            description = as.character(date()) )
        return(ans)
    }
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


fredSeries <-  
    function(symbols, from = NULL, to = Sys.timeDate(), nDaysBack = 366, ...)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Downloads easily time series data from St. Louis FRED
    
    # Arguments:
    #   symbols - a character string value, the St. Louis FRED 
    #       symbol name.
    #   frequency - a character string value, the frquency of the
    #       data records.
    #   ... - arguments passed to the function returns
    
    # Examples:
    #   fredSeries("DPRIME")
    
    # FUNCTION:
    
    # Settings:
    query = symbols
    
    # Download:
    Y = fredImport(query = query, ...)@data
    X = as.timeSeries(Y)
    N = length(query)
    if (N > 1) {
        for (i in 2:N) {
            Y = fredImport(query = query[i], ...)@data   
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

     