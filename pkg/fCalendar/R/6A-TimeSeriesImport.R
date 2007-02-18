
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
#   1999 - 2006, Diethelm Wuertz, GPL
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
#  fWEBDATA              Class Representation
#  show.fWEBDATA         S4 Show Method
# FUNCTION:             IMPORT DATA FUNCTIONS:
#  economagicImport      Downloads market data from EconoMagic's web site
#  yahooImport           Downloads market data from Yahoo's web site 
#  .yahooImport          ... the old download function 
#  fredImport            Downloads market data from St. Louis FED web site
#  forecastsImport       Downloads monthly data from www.forecasts.org
# FUNCTION:             IMPORT STATISTICS - EXPERIMENTELL:
#  keystatsImport        Downloads key statistics from Yahoo's web site  
#  print.keystats        Print Method for internal function .keystatsImport
# FUNCTION:             EASY TO USE ROUTINES:
#  yahooSeries           Easy to use download from Yahoo
#  .yahooSeries          Utility function  called by 'yahooSeries'
# FUNCTION:             ONLY FOR SPLUS VERSION:
#  as.Date               S-PLUS: Converts date represenatation
#  data                  S-PLUS: Loads or lists specified data sets
#  download.file         S-PLUS: Downloads using "lynx" or "wget"
#  strsplit              S-PLUS: Splits character vector into substrings
################################################################################


################################################################################
#  fWEBDATA              Class Representation
#  show.fWEBDATA         S4 Show Method


setClass("fWEBDATA", 
    representation(
        call = "call",
        data = "data.frame",
        param = "character",
        title = "character",
        description = "character")  
)


# ------------------------------------------------------------------------------


show.fWEBDATA = 
function(object)
{   # A function implemented by Diethelm Wuertz

    # Changes:
    #
    
    # FUNCTION:
       
    # Unlike print the argument for show is 'object'.
    x = object
    
    # Title:
    cat("\nTitle:\n", object@title, "\n", sep = "")
    
    # Parameter:
    cat("\nParameter:\n")
    param = cbind(object@param)
    colnames(param) = "Value:"
    print(param, quotes = FALSE) 
    
    # Description:
    cat("\nDescription:\n", object@description, sep = "")   
    cat("\n\n")
    
    # Return Value:
    invisible()
}


setMethod("show", "fWEBDATA", show.fWEBDATA)


################################################################################
#  economagicImport      Downloads market data from EconoMagic's web site
#  yahooImport           Downloads market data from Yahoo's web site 
#  .yahooImport          ... the old download function 
#  .keystatsImport       Downloads key statistics from Yahoo's web site  
#  fredImport            Downloads market data from St. Louis FED web site
#  forecastsImport       Downloads monthly data from www.forecasts.org


economagicImport =
function (query, file = "tempfile", 
source = "http://www.economagic.com/em-cgi/data.exe/", 
frequency = c("quarterly", "monthly", "daily"), 
save = FALSE, colname = "VALUE", try = TRUE)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Downloads market data from EconoMagic's web site
    
    # Notes:
    #   Note, only the first column is returned, the remaining are  
    #     usually percentual changes which can be calculated otherwise.

    # Examples:
    #   USDEUR Foreign Exchange Rate:
    #    economagicImport("fedny/day-fxus2eu", "USDEUR.CSV", 
    #       frequency = "daily", colname = "USDEUR")
    #   USFEDFUNDS US FedFunds Rate:
    #    economagicImport("fedstl/fedfunds+2", "USFEDFUNDS.CSV", 
    #       frequency = "monthly", colname = "USFEDFUNDS")
    #   USDGNP:
    #    economagicImport("fedstl/gnp", "USGNP.CSV", 
    #       frequency = "monthly", colname = "USGNP")

    # Changes:
    #
    
    # FUNCTION:
    
    # Frequency:
    freq = match.arg(frequency)
              
    # Download:
    if (try) {
        # First try if the Internet can be accessed:
        z = try(economagicImport(query = query, 
            file = file, source = source, frequency = freq, 
            save = save, colname = colname, try = FALSE)) 
        if (class(z) == "try-error" || class(z) == "Error") {
            return("No Internet Access") 
        } else {
            return(z) 
        }
    } else {  
        # Settings:
        if (freq == "quarterly" || freq == "monthly") n = 2
        if (freq == "daily") n = 3
       
        # For S-Plus Compatibility:
        if (class(version) != "Sversion") {
            # R:
            method = NULL
        } else { 
            # SPlus
            method = "lynx"
        }
    
        # Download the file:
        url = paste(source, query, sep = "")
        download.file(url = url, destfile = file, method = method)
        SCAN = scan(file, what = "", sep = "\n")
        
        # Extract all the data records and build the table:
        lines = grep("font color=white", SCAN)
        z1 = SCAN[lines][-(1:2)]
        
        # Remove irrelevant HTML markup strings
        z2 = gsub("font", "          ", x = z1)  
        z1 = gsub("color=white........", " ", x = z2)      
        z2 = gsub(">", " ", x = z1) 
        z1 = gsub("<", " ", x = z2) 
        
        # Next - Compose Matrix: 
        n.rows = length(z1)                         
        z2 = unlist(apply(matrix(z1, ncol = 1), 2, strsplit, split = " "))
        z1 = as.numeric(z2[z2 != ""])
        n.fields = length(z1)     
        z = matrix(z1, byrow = TRUE, ncol = n.fields/n.rows) 
        if (n == 2) z = cbind(z[,1]*100+z[,2], z[,3])
        if (n == 3) z = cbind(z[,1]*10000+z[,2]*100+z[,3], z[,4])
        
        # Create the dates in ISO-8601 format:
        # For quarterly data multiplay quarters by 3 to get monthly base
        if (freq == "quarterly") z[,1] = 100*(z[,1]%/%100)+3*z[,1]%%100
        z = data.frame(cbind(z[, 1], z[, 2]))
        ## znames = as.character(1:(length(names(z)) - 1))
        names(z) = c("DATE", colname)   
        # DW - add hyphens:
        rowNames = as.character(z[, 1])
        if (freq == "daily") {
            rowNames = paste(
                substring(rowNames, 1, 4), "-",
                substring(rowNames, 5, 6), "-",
                substring(rowNames, 7, 8), sep = "")
        } else {
            rowNames = paste(
                substring(rowNames, 1, 4), "-",
                substring(rowNames, 5, 6), "-01", sep = "")
        }
        z[, 1] = rowNames   
        
        # Save to file:
        if (save) {
            write.table(z, file, quote = FALSE, sep = ";", row.names = FALSE) 
        } else {
            unlink(file) 
        }
        
        # Return Value:
        ans = new("fWEBDATA",     
            call = match.call(),
            param = c(
                "Instrument Query" = query, 
                "Frequency" = frequency, 
                "Instrument Name" = colname),
            data = z, 
            title = "Web Data Import from Economagic", 
            description = as.character(date()) )
        return(ans)
    }
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------
    

yahooImport = 
function (query, file = "tempfile", 
source = "http://chart.yahoo.com/table.csv?", save = FALSE, sep = ";", 
swap = 20, try = TRUE)
{   # A function implemented by Diethelm Wuertz

    # Example:
    #   IBM SHARES, test 19/20 century change 01-12-1999 -- 31-01-2000:
    #   yahooImport("s=IBM&a=11&b=1&c=1999&d=0&e=31&f=2000&g=d&x=.csv")

    # Notes:
    #   Requires: fields() cuts a string in fields
    #   Yahoo Token Description:           
    #   s     Selected Ticker-Symbol
    #   a     First Quote starts with Month (mm): 0-11, Jan-Dec
    #   b     First Quote starts with Day (dd)
    #   c     First Quote starts with Year: as CCYY
    #   d     Last Quote ends with Month (mm): 0-11, Jan-Dec
    #   e     Last Quote ends with Day (dd)
    #   f     Last Quote ends with Year (yy): as CCYY
    #   r     Aggregation Level
    #   z     Selected Ticker-Symbol [optional]
    
    # Changes:
    #   2007-02-18 DW: Update to new %Y-%m-%d format
    
    # FUNCTION:
    
    # Download:
    if (try) {
        # First try if the Internet can be accessed:
        z = try(yahooImport(file = file, source = source, 
            query = query, save = save, try = FALSE))
        if (class(z) == "try-error" || class(z) == "Error") {
            return("No Internet Access") 
        } else {
            return(z) 
        }
    } else {
        # For S-Plus Compatibility:
        if (class(version) != "Sversion") {
            # R:
            method = NULL
        } else { 
            # SPlus
            method = "wget"
        }
    
        # Download the file:
        url = paste(source, query, sep = "")
        download.file(url = url, destfile = file, method = method)
        
        # Read data and revert:
        x = X = read.table(file, header = TRUE, sep = ",")
        n = dim(x)[1]
        x = x[n:1, ]
        
        # Result:
        colnames(x)[1] <- "%Y-%m-%d"
        rownames(x) = 1:n
        z = data.frame(x)
   
        # Save Download ?
        colNames = colnames(z)[-1]
        if (save) {
            # Header:
            write.table(t(c("%Y-%m-%d", colNames)), file, quote = FALSE, 
                row.names = FALSE, col.names = FALSE, sep = ";")
            # Data:
            write.table(z, file, quote = FALSE, append = TRUE, 
                col.names = FALSE, sep = ";") 
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
  

fredImport = 
function(query, file = "tempfile", 
source = "http://research.stlouisfed.org/fred2/series/", 
frequency = "daily", save = FALSE, sep = ";", try = TRUE) 
{   # A function implemented by Diethelm Wuertz

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
        if (class(z) == "try-error" || class(z) == "Error") {
            return("No Internet Access") 
        } else {
            return(z) 
        } 
    } else { 
        # File name:
        queryFile = paste(query, "/downloaddata/", query, ".txt", sep = "")
        
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
        # 
        dimnames(z) = list(rowNames, colNames)
        
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
        z = data.frame(DATE = rowNames, z, row.names = NULL)
        
        # Return Value:
        ans = new("fWEBDATA",     
            call = match.call(),
            param = c(
                "Instrument Query" = query,
                "Frequency" = frequency),
            data = z, 
            title = "Web Data Import from FED St. Louis", 
            description = as.character(date()) )
        return(ans)
    }
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


forecastsImport = 
function(query, file = "tempfile", 
source = "http://www.forecasts.org/data/data/", save = FALSE, try = TRUE) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Downloads Monthly Market Data, Indices and Benchmarks from the 
    #   Financial Forecast Center, "www.forecasts.org".
    
    # Value:
    #   An One Column data frame with row names denoting the dates
    #   given in the POSIX format "%Y%m%d".
        
    # Examples:
    #   forecastsImport(query = "GOLD")
    #   forecastsImport(query = "MDISCRT")
    #   forecastsImport(query = "EXJPUS")
    #   forecastsImport(query = "GS3M")
    #   forecastsImport(query = "FEDFUNDS")
    
    # Note:
    #   This function is not written for daily data sets.
    #   Some example data sets include:  
    #   Indices:
    #     djiaM     sp500M   sp100M     nysecompM  nasdcompM  djcompM  
    #     djtransM  djutilM  spmc400M   spsc600M   r1000M     r2000M    
    #     r3000M    w5000M   valuM   
    #     nik225M  daxM      hangsengM  ftse100M   tse300M    mtM
    #   Ohter:  
    #     MDISCRT      
    #     EXJPUS        
    #     GS3M

    # FUNCTION:
    
    # Download:
    if (try) {
        # Try for Internet Connection:
        z = try(forecastsImport(file = file, source = source, query = query, 
            save = save, try = FALSE))
        if (class(z) == "try-error") {
            print("No Internet Access")
            return(NULL) 
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
        x = data.frame(x[, 2], row.names = 
            as.character(10000*as.numeric(x[, 1]) + 28))
        # Add column name:
        colnames(x) = query
        # Save Download ?
        if (save) {
            write.table(paste("%Y%m%d;", query, sep = ""), file, 
                quote = FALSE, row.names = FALSE, col.names = FALSE)
            write.table(x, file, quote = FALSE, append = TRUE, 
                col.names = FALSE, sep=";") 
        } else {
            unlink(file) 
        }  
              
        # Return Value:
        ans = new("fWEBDATA",     
            call = match.call(),
            param = c("Instrument Query" = query),
            data = x,
            title = "Web Data Import from Forecasts", 
            description = as.character(date()) )
        return(ans)
    }
    
    # Return Value:
    invisible()
}


################################################################################
#  keystatsImport       Downloads key statistics from Yahoo's web site 
#  .keystatsImport      Downloads key statistics from Yahoo's web site  
#  print.keystats       Print Method for internal function .keystatsImpor


keystatsImport =  
function (query, file = "tempfile", source = "http://finance.yahoo.com/q/ks?s=", 
save = FALSE, try = TRUE) 
{   # A function implemented by Diethelm Wuertz and Matthew C.Keller

    # Description:
    #   Downloads Key Statistics on shares from Yahoo's Internet site
    
    # Example:
    #   keystatsImport("YHOO")
    
    # Changes:
    #   2006-08-26 update by MCK
    
    # FUNCTION:
    
    # Download:
    if (try) {
        # First try if the Internet can be accessed:
        z = try(keystatsImport(file = file, source = source, 
            query = query, save = save, try = FALSE))
        if (class(z) == "try-error" || class(z) == "Error") {
            return("No Internet Access")
        }
        else {
            return(z)
        }
    } else {
        # offset = 2
        url = paste(source, query, sep = "")
        # For S-Plus Compatibility:
        if (class(version) != "Sversion") {
            method = NULL
        } else {
            method = "wget"
        }
        # Download:
        download.file(url = url, destfile = file, method = method)
        .warn = options()$warn
        options(warn = -1)
        x = scan(file, what = "", sep = ">")
        options(warn = .warn)
        keynames <- c(
            "Market Cap ", "Enterprise Value ", "Trailing P/E ",
            "Forward P/E ", "PEG Ratio ", "Price/Sales ", "Price/Book ", 
            "Enterprise Value/Revenue ", "Enterprise Value/EBITDA ",
            "Forward Annual Dividend Rate ", "Forward Annual Dividend Yield ",
            "Beta: ", "52-Week Change ", 
            "P500 52-Week Change ", "52-Week High ", "52-Week Low ")

        #
        if (class(version) != "Sversion") {
            stats = as.character(Sys.Date())
        } else {
            currentDate = timeDate(date(), in.format = "%w %m %d %H:%M:%S %Z %Y")
            mdy = month.day.year(currentDate)
            stats = as.character(mdy$year * 10000 + mdy$month * 
                100 + mdy$day)
        }

        #
        offset <- c(2,6,2,7,2,2,2,6,6,6,6,2,6,6,6,6)
        fff <- 0
        for (s in keynames) {
            fff <- fff+1
            grepped <- paste(gsub("</td", "", x[grep(s, x) + offset[fff]]))
                    if (length(grepped) > 1)
                    grepped <- grepped[1]
                if (length(grepped) == 0) 
                        grepped <- "NA"
                    if (grepped == "") 
                        grepped <- "NA"
            stats <- c(stats, grepped)
        }

        #
        for (i in 1:length(keynames)) {
            keynames[i] = substring(keynames[i], 1, nchar(keynames[i]) -  1)
        }
        keynames = c("Date", keynames)
        ans = list(
            query = query, 
            keystats = data.frame(cbind(Keyname = keynames, Statistic = stats)))
        class(ans) = "keystats"
        ans
    }
}


# ------------------------------------------------------------------------------
 

.keystatsImport = 
function(query, file = "tempfile", source = "http://finance.yahoo.com/q/ks?s=", 
save = FALSE, try = TRUE) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Downloads Key Statistics on shares from Yahoo's Internet site
     
    # Example:
    #   keystatsImport("YHOO")
    
    # Note:
    #   Old Version, no longer used ...
       
    # FUNCTION:
    
    # Download:
    if (try) {
        # First try if the Internet can be accessed:
        z = try(keystatsImport(file = file, source = source, 
            query = query, save = save, try = FALSE))
        if (class(z) == "try-error" || class(z) == "Error") {
            return("No Internet Access")
        } else {
            return(z) 
        } 
    } else {                 
        offset = 2
        url = paste(source, query, sep = "")
        # For S-Plus Compatibility:
        if (class(version) != "Sversion") {
            # R:
            method = NULL
        } else { 
            # SPlus
            method = "wget"
        }
        # Download:
        download.file(url = url, destfile = file, method = method)
        .warn = options()$warn
        options(warn = -1)
        x = scan(file, what = "", sep = ">")
        options(warn = .warn)
        keynames = c(
            "Market Cap ", 
            "Enterprise Value ",
            "Trailing P/E ",
            "Forward P/E ",
            "PEG Ratio ",
            "Price/Sales ",
            "Price/Book ",
            "Enterprise Value/Revenue ",
            "Enterprise Value/EBITDA ",
            "Annual Dividend:",
            "Dividend Yield:",
            "Beta:",
            "52-Week Change:",
            "52-Week High ",
            "52-Week Low " )
        if (class(version) != "Sversion") {
            # R:
            stats = as.character(Sys.Date())
        } else {
            # SPlus:
            currentDate = timeDate(date(), in.format="%w %m %d %H:%M:%S %Z %Y")
            mdy = month.day.year(currentDate)
            stats = as.character(mdy$year*10000+mdy$month*100+mdy$day)
        }
        for (s in keynames) {
            grepped = paste(gsub("</td", "", x[grep(s, x) + offset]))
            # DW 2005-05-30
            if (length(grepped) == 0) grepped = "NA"
            if (grepped == "") grepped = "NA"
            ### DW
            stats = c(stats, grepped)
        }
        for (i in 1:length(keynames)) {
            keynames[i] = substring(keynames[i], 1, nchar(keynames[i])-1)
        }
        keynames = c("Date", keynames)  
             
        # Return Value:
        ans = list(
            query = query, 
            keystats = data.frame(cbind(Keyname = keynames, Statistic = stats)))   
        class(ans) = "keystats"
        ans    
    }
}


# ------------------------------------------------------------------------------


print.keystats = 
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Print Method for an object of class 'keystats'
    
    # Changes:
    #   2006-08-26 DW description modified
    
    # FUNCTION:
    
    # Title:
    cat("\nTitle:\n ")
    cat("Yahoo Key Statistics\n", sep = "")
    
    # Key Statistics:
    cat("\nKey Statistics for", x$query, "\n")
    print(x$keystats)
    
    # Description:
    cat("\nDescription:\n ")   
    cat(.description())
    cat("\n\n")
    
    # Return Value:
    invisible()
}


################################################################################
#  yahooSeries           Easy to use download from Yahoo
#  .yahooSeries          Utility function  called by 'yahooSeries'


yahooSeries = 
function(symbols = c("^DJI", "IBM"), from = NULL, to = NULL, 
nDaysBack = 365, quote = c("Open", "High", "Low", "Close", "Volume"), 
aggregation = c("d", "w", "m"), returnClass = c("timeSeries", "ts", 
"matrix", "data.frame"), getReturns = FALSE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Easily downloads time series data from Yahoo
    
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
    #   yahooSeries(aggregation = "w")
    #   yahooSeries(aggregation = "m", nDaysBack = 10*366)
    #   yahooSeries(returnSeries = TRUE)
    
    # FUNCTION:
    
    # Settings:
    returnClass = match.arg(returnClass)
    aggregation = match.arg(aggregation)
       
    # First Symbol:
    X = .yahooSeries(symbols[1], from = from, to = to, 
        nDaysBack = nDaysBack, quote = quote, 
        aggregation = aggregation, returnClass = "timeSeries")
    UNITS = paste(symbols[1], ".", quote, sep = "")
    if (aggregation == "d") X = alignDailySeries(X, ...) 
    X@units = UNITS
    colnames(X@Data) = UNITS
    # print(head(X))

    # Next Symbols:
    if (length(symbols) > 1) {
        for (i in 2:length(symbols)) { 
            Y = .yahooSeries(symbols[i], from = from, to = to, 
                nDaysBack = nDaysBack, quote = quote, 
                aggregation = aggregation, returnClass = "timeSeries")
            # print(head(Y))
            UNITS = paste(symbols[i], ".", quote, sep = "")
            if (aggregation == "d") Y = alignDailySeries(Y, ...)
            X = .mergeSeries(X, Y@Data, units = c(X@units, UNITS))
            # print(head(X))
        }
    }
    
    # Return Series ?
    if (getReturns) X = returnSeries(X, ...)  
    
    # As zoo | ts  Object ?
    if (returnClass == "matrix") {
        X = X@data
    } else if (returnClass == "data.frame") {
        X = data.frame(X@Data)
    } else if (returnClass == "ts") {
        X = as.ts(X@Data)
    }
    
    # Return Value:
    X
}


# ------------------------------------------------------------------------------


.yahooSeries = 
function(symbol = "IBM", from = NULL, to = NULL, nDaysBack = 365, 
quote = "Close", aggregation = c("d", "w", "m"), 
returnClass = c("timeSeries", "zoo", "ts"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Easily downloads univariate time series data from Yahoo
    
    # Arguments:
    #   symbol - a character string, the Yahoo Symbol name.
    #   from - ISO formatted character string of the starting date,
    #       e.g. "2005-01-01".
    #   to - ISO formatted character string of the end date,
    #       e.g. "2005-12-31".
    #   ndaysBack - an integer giving the length of the download 
    #       period in number of days starting n days back from Today.
    #       Only in use if 'from' and 'to' are not specified.
    #   quote - a character value or vector of strings giving the 
    #       column names of those instruments to be extracted from
    #       the download. 
    #   aggregation - a character string denoting the aggregation
    #       level of the downloaded date, 'd' for daily, 'w' for
    #       weekly and 'm' for monthly data records.
    #   returnClass = a character string which decides how the downloaded
    #       time series will be returned. By default an object of
    #       class 'timeSeries' will be returned .
    
    # Examples:
    #   yahooSeries(aggregation = "w")
    #   yahooSeries(aggregation = "m", nDaysBack = 10*366)
   
    # FUNCTION:
    
    # Automatic Selection of From / To: 
    if (is.null(from) & is.null(to)) {
        to = Sys.Date() 
        from = as.character(to - nDaysBack)
        to = as.character(to)
    }
    
    # Extract Atoms - From:
    yearFrom = substring(from, 1, 4)
    monthFrom = as.character(as.integer(substring(from, 6,7))-1)
    dayFrom = substring(from, 9, 10)
    
    # Exgtract Atoms- To:
    yearTo = substring(to, 1, 4)
    monthTo = as.character(as.integer(substring(to, 6,7))-1)
    dayTo = substring(to, 9, 10)
    
    # Download:
    query = paste("s=", symbol, 
        "&a=", monthFrom, "&b=", dayFrom, "&c=", yearFrom, 
        "&d=", monthTo, "&e=", dayTo, "&f=", yearTo,
        "&g=", aggregation[1], "&x=.csv", sep = "")  
    X = yahooImport(query)@data 
    
    # Extract desired columns from Time Series:
    ans = as.timeSeries(X)[, quote]
    
    # Return Value:
    ans
}
 

################################################################################
#  as.Date               S-PLUS: Converts date represenatation
#  data                  S-PLUS: Loads or lists specified data sets
#  download.file         S-PLUS: Downloads from Internet using "lynx" or "wget"
#  strsplit              S-PLUS: Splits character vector into substrings


if (!exists("as.Date")) 
{   
as.Date = 
function(x, format = "%d-%m-%y")
{   # A Function implemented by Diethelm Wuertz

    # Description:
    #   Mimics R's as.Date function
    
    # FUNCTION:
    
    # Used by yahooImport ...
    ans = timeDate(s, in.format = format, format = "%Y-%02m-%02d")  
    
    # Return Value:
    ans 
}}


# ------------------------------------------------------------------------------


if (!exists("data")) {
data = 
function(x)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # For S-Plus Compatibility:
    invisible(x)
}}


# ------------------------------------------------------------------------------


if (!exists("download.file")) {
download.file =
function(url, destfile, method, ...)
{   # A function implemented by Diethelm Wuertz
  
    # FUNCTION:
    
    # Download:
    if (method == "lynx") {
        command = paste("lynx.exe -source", url, ">", destfile, sep = " ")
        dos(command, ...)
        return(invisible())
    } 
    if (method == "wget") {
        command = paste("wget.exe", url, "-O", destfile, sep = " ")
        system(command, minimized = TRUE, ...)
        return(invisible())
    }       
}}


# ------------------------------------------------------------------------------


if (!exists("strsplit")) {
strsplit = 
function(x, split = " ") 
{   # A function implemented by Diethelm Wuertz
  
    # FUNCTION:
    
    # For S-Plus Compatibility:
    ans = lapply(lapply(X = x, FUN = unpaste, sep = split), unlist) 
    
    # Return Value:
    ans
}}
 

################################################################################

