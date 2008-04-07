
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
# FUNCTION:         LPP/SWX DATA IMPORT:
#  swxImport         Downloads SWX traded instruments from www.swx.com
#  swxSeries         Downloads easily time series data from www.swx.com
################################################################################


.swxImport <-
    function(query = "CH0009980894", file = "tempfile",
    frequency = "daily", save = FALSE, sep = ";", try = TRUE )
{
    # A function implemented by Diethelm Wuertz and Yohan Chalabi

    # Description:
    #   Downloads SWX traded instruments from SWX Web Site

    # Arguments:
    #   query - ISIN code number of asset
    #   units - an optional asset name, by default the ISIN number.

    # Notes:
    #   SWX Series: SBI SPI SWIIT

    # Example:
    #   swxImport("CH0009980894") # *CHF9 unique ? 

    # FUNCTION:
    
    # Require:
    require(gdata)

    # Check:
    if (frequency != "daily")
        stop("Only daily dat records are supported!")

    # Download:
    if (try) {
        # Try for Internet Connection:
        z = try(.swxImport(query = query, file = file, frequency = frequency,
            save = save, sep = sep, try = FALSE))
        if (inherits(z, "try-error") || inherits(z, "Error")) {
            return("No Internet Access")
        } else {
            return(z)
        }
    } else {
        # For S-Plus Compatibility:
        method <- if(is.R()) NULL else "wget"

        # Paste URL:
        URL = paste(
            "http://www.swx.com/market/chart_data.csv?id=" ,
            query, "CHF9&domain=max", sep = "")

        # Download and temporarily store:
        download.file(url = URL, destfile = file, method = method)

        # Scan the file:
        x = scan(file, what = "", sep = "\n", skip = 1)
        unlink(file)
        nx = length(x)
        x = matrix(unlist(strsplit(x, ";")), byrow = TRUE, nrow = nx)
        x = x[-1, ]

        # Extract charvec:
        dd = substring(x[, 1], 1, 2)
        mm = substring(x[, 1], 4, 5)
        YY = substring(x[, 1], 7, 10)
        charvec = paste(YY, mm, dd, sep = "-")

        # Extract data Matrix:
        z = matrix(as.numeric(x[, 3]), ncol = 1)

        # Add column names:
        colNames = query

        # Return Value:
        X = data.frame(charvec, z)
        colnames(X) = c("DATE", query)

        # Save download ?
        if (save) {
            write.table(paste("%Y%m%d", query, sep = sep), file,
                        quote = FALSE, row.names = FALSE, col.names = FALSE)
            write.table(X, file, quote = FALSE, append = TRUE,
                        col.names = FALSE, sep = ";")
        } else {
            unlink(file)
        }

        # Return Value:
        ans = new("fWEBDATA",
        call = match.call(),
        param = c(
            "Instrument" = query,
            "Frequency" = frequency),
        data = X,
        title = "Web Data Import from SWX web site",
        description = description() )
        return(ans)
    }

    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


################################################################################
# FUNCTION:         LPP/SWX DATA IMPORT:

################################################################################


.swxSeries <-
    function(
        symbols, 
        from = NULL, 
        to = Sys.timeDate(), 
        nDaysBack = 366, ...)
{
    # A function implemented by Yohan Chalabi and Diethelm Wuertz
    
    # Description:
    #   Downloads easily time series data from SWX web site

    # Arguments:
    #   query - a character string value, the SWX symbol name
    #   from -
    #   to - 
    #   nDaysBack -  
    #   frequency - a character string value, the frquency of the
    #       data records.

    # Examples:
    #   swxSeries("CH0009980894")

    # FUNCTION:

    # Settings:
    query = symbols
    
    # Download:
    Y = .swxImport(query = query, ...)@data
    X = as.timeSeries(Y)
    N = length(query)
    if (N > 1) {
        for (i in 2:N) {
            Y = .swxImport(query = query[i], ...)@data   
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

