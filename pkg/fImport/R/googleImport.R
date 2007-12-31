
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
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:             DESCRIPTION:
#  googleImport          Downloads market data from Google's web site  
#  googleSeries          Easy to use download from Google 
################################################################################


googleImport <-  
    function (query, file = "tempfile", 
    source = "http://finance.google.com/finance/historical?", 
    save = FALSE, sep = ";", try = TRUE)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Downloads market data from Google's web site 
    
    # Example:
    #   IBM SHARES, test 19/20 century change 01-12-1999 -- 31-01-2000:
    #   googleImport("q=IBM&startdate=Dec+01+1999&enddate=Jan+31+2000&output=csv")
    
    # FUNCTION:
    
    # Settings:
    stopifnot(length(query) == 1)
    
    # Download:
    if (try) {
        # First try if the Internet can be accessed:
        z = try(googleImport(query = query, file = file, source = source, 
            save = save, try = FALSE))
        if (inherits(z, "try-error") || inherits(z, "Error")) {
            return("No Internet Access") 
        } else {
            return(z) 
        }
    } else {
        # For S-Plus Compatibility:
        method <- if(is.R()) NULL else "wget"
        
        # Download:
        google.URL <- "http://finance.google.com/finance/historical?"   
        fr <- read.csv(paste(google.URL, query, sep = ""))
        
        # Note, google data is backwards
        fr <- fr[nrow(fr):1, ]
        
        # Fix google bug:
        # bad.dates <- c('29-Dec-04','30-Dec-04','31-Dec-04')
        # dup.dates <- which(fr[, 1] %in% bad.dates)[(1:3)]
        # fr <- fr[-dup.dates, ]
        
        # Convert to timeSeries:
        quote = c("Open", "High", "Low", "Close", "Volume")
        charvec = as.Date(fr[, 1], format = "%d-%B-%y")
        data =  matrix(as.numeric(as.matrix(fr)[, quote]), 
            ncol = length(quote))
        z = cbind.data.frame(charvec, data)
        colnames(z) = c("%Y-%m-%d", quote)
   
        # Save Download ?
        colNames = colnames(z)[-1]
        if (save) {
            # Header:
            write.table(t(c("%Y-%m-%d", colNames)), file, quote = FALSE, 
                row.names = FALSE, col.names = FALSE, sep = ";")
            # Data:
            write.table(z, file, quote = FALSE, append = TRUE, 
                col.names = FALSE, row.names = FALSE, sep = ";") 
            # Check:
            # read.table(file, header = TRUE, sep = ";")
        } else {
            unlink(file) 
        } 
        
        # Return Value:
        ans = new("fWEBDATA",     
            call = match.call(),
            param = c("Instrument Query" = query),
            data = z, 
            title = "Web Data Import from Yahoo", 
            description = as.character(date()) )
        return(ans)
    }
    
    # Return Value:
    invisible()
}

    
# ------------------------------------------------------------------------------


googleSeries <-  
    function(symbols = c("IBM", "MSFT"), from = NULL, to = NULL, 
    nDaysBack = 365, quote = c("Open", "High", "Low", "Close", "Volume"), 
    aggregation = c("d", "m"), 
    returnClass = c("timeSeries", "ts", "matrix", "data.frame"), 
    getReturns = FALSE, ...)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Downloads easily time series data from Yahoo
    
    # Arguments:
    #   symbols - a character string value or vector, the Yahoo 
    #       symbol name(s).
    #   from - an ISO-8601 formatted character string of the starting 
    #       date, e.g. "2005-01-01".
    #   to - ISO formatted character string of the end date,
    #       e.g. "2005-12-31".
    #   ndaysBack - an integer giving the length of the download 
    #       period in number of days starting n days back from today.
    #       Only in use if 'from' and 'to' are not specified.
    #   quote - a character value or vector of strings giving the 
    #       column names of those instruments to be extracted from
    #       the download. 
    #   aggregation - a character string denoting the aggregation
    #       level of the downloaded data records, 'd' for daily, 'w' 
    #       for weekly and 'm' for monthly data records.
    #   returns - a logical flag. Should return values be computed
    #       using the function 'returnSeries'?
    #   returnClass = a character string which decides how the downloaded
    #       time series will be returned. By default an object of
    #       class 'timeSeries' will be returned .
    
    # Examples:
    #   googleSeries(symbols = "IBM")
    #   googleSeries(symbols = "IBM", aggregation = "m")
    #   googleSeries(symbols = c("MSFT", "IBM"), quote = "Close")
    #   googleSeries("IBM", aggregation = "m", nDaysBack = 10*366)
    
    # FUNCTION:
    
    # Match Arguments:
    returnClass = match.arg(returnClass)
    aggregation = match.arg(aggregation)
    
    # Internal Univariate Download Function:
    # symbol = "IBM", from = NULL, to = NULL, nDaysBack = 365, 
    # quote = "Close", aggregation = c("d", "w", "m"), 
    # returnClass = c("timeSeries", "zoo", "ts")) 
 
    # Automatic Selection of From / To: 
    if (is.null(from) & is.null(to)) {
        to = Sys.Date() 
        from = as.character(to - nDaysBack)
        to = as.character(to) }
    
    # Extract Atoms - From:
    yearFrom = as.numeric(substring(from, 1, 4))
    monthFrom = as.numeric(substring(from, 6,7))
    dayFrom = as.numeric(substring(from, 9, 10))
    
    # Extract Atoms - To:
    yearTo = as.numeric(substring(to, 1, 4))
    monthTo = as.numeric(substring(to, 6,7))
    dayTo = as.numeric(substring(to, 9, 10))

    google.URL <- "http://finance.google.com/finance/historical?"   
    for (i in 1:length(symbols)) 
    {        
        query = paste("q=", symbols[i],
            "&startdate=", month.abb[monthFrom],
            "+", sprintf('%.2d', dayFrom),
            ",+", yearFrom,
            "&enddate=", month.abb[monthTo],
            "+", sprintf('%.2d', dayTo),
            ",+", yearTo,
            "&output=csv",
            sep = "")
        print(query)
        imported = googleImport(query)@data
        charvec = as.character(imported[, 1])
        data = imported[, quote]
        Y = timeSeries(data, charvec)
        UNITS = paste(symbols[i], ".", quote, sep = "")
        if (aggregation == "d") Y = alignDailySeries(Y, ...) 
        Y@units = UNITS
        colnames(Y@Data) = UNITS
        if (i == 1) X = Y else X = merge(X, Y)
    }
        
    # Compute Return Series ?
    if (getReturns) X = returns(X, ...)  
    
    # Return as Object ?
    if (returnClass == "matrix") {
        X = X@data 
    }
    if (returnClass == "data.frame") {
        X = data.frame(X@Data)
    }
    if (returnClass == "ts") {
        X = as.ts(X@Data)
    }
    
    # Return Value:
    X     
}   
   

################################################################################

