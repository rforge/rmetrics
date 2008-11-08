
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


################################################################################
# FUNCTION:             DESCRIPTION:  
#  oandaImport           Downloads market data from www.oanda.com
#  oandaSeries           Easy to use download from www.oanda.com
################################################################################


oandaImport <-  
    function(query, file = "tempfile", source = NULL,
    frequency = "daily", 
    from = NULL, to = Sys.timeDate(), nDaysBack = 366, 
    save = FALSE, sep = ";", try = TRUE) 
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Downloads market data from www.oanda.com
    
    # Example:
    #   oandaImport("USD/EUR") 
    
    # Value:
    #   An One Column data frame with row names denoting the dates
    #   given in the POSIX format "%Y%m%d". The column lists the
    #   downloaded data records.      
  
    # FUNCTION:
    
    # Settings:
    stopifnot(length(query) == 1)
    
    # Check:
    stopifnot (frequency == "daily")
    
    # Source:
    if (is.null(source))
        source = "http://www.oanda.com/convert/fxhistory?lang=en&"
    
    # Download:
    if (try) {
        # Try for Internet Connection:
        z = try(oandaImport(query, file, source, frequency, from, to, 
            nDaysBack, save, sep, try = FALSE))
        if (inherits(z, "try-error") || inherits(z, "Error")) {
            return("No Internet Access") 
        } else {
            return(z) 
        } 
    } else { 
        # Download File:
        to = trunc(to, "days")
        from = to - nDaysBack*24*3600
        from.date <- format(timeDate(from),
            "date1=%m%%2F%d%%2F%y&")
        to.date <- format(timeDate(to),
            "date=%m%%2F%d%%2F%y&date_fmt=us&")
        ccy.pair <- strsplit(toupper(query),"/")[[1]]
        tmp <- tempfile()
        download.file(
            paste(source, from.date, to.date, "exch=", ccy.pair[1], 
                "&expr2=", ccy.pair[2], "&margin_fixed=0&SUBMIT=Get+Table&",
                "format=CSV&redirected=1", sep = ""), 
            destfile = tmp)
            
        # Compose Time Series:
        fx <- readLines(tmp)       
        fx <- unlist(strsplit(
            gsub("<PRE>|</PRE>", "", 
            fx[(grep("PRE", fx)[1]):(grep("PRE",fx)[2])]), ","))
        fx = matrix(fx, byrow = TRUE, ncol = 2)
        data = as.numeric(fx[, 2])
        time = fx[, 1]
        charvec = paste(substr(time, 7, 10), substr(time, 1, 2),
            substr(time, 4, 5), sep = "-")
        X = timeSeries(data, charvec, units = query)
    }
    
    # Save to file:
    if (save) {
        write.table(as.data.frame(X)) 
    } else {
        unlink(file) 
    }
    
    # Result:
    ans = new("fWEBDATA",     
        call = match.call(),
        param = c(
            "Instrument" = query, 
            "Frequency " = frequency),
        data = X, 
        title = "Data Import from www.oanda.com", 
        description = description() )
        
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


oandaSeries <-
    function(symbols, from = NULL, to = Sys.timeDate(), nDaysBack = 366,  ...) 
{ 
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Easy to use download from www.oanda.com
    
    # Arguments:
    #   symbols - a character vector of symbol names
    #   from - from date
    #   to - to date
    #   nDaysBack - number of n-days back
    #   ... - arguments passed to the *Import()
    
    # Example:
    #   oandaSeries("USD/EUR") 
    #   oandaSeries(c("USD/EUR", "USD/JPY"), nDaysBack = 10) 
    
    # FUNCTION:
        
    # Download:
    X = oandaImport(query = symbols[1], ...)@data
    N = length(symbols)
    if (N > 1) {
        for (i in 2:N) {
            X = merge(X, oandaImport(query = symbols[i], ...)@data)  
        }
    }    
    
    # Time Window:
    if (is.null(from)) from = to - nDaysBack*24*3600
    X = window(X, from, to)
    
    # Return Value:
    X  
}

         
################################################################################

