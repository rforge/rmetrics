
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
#   1999 - 2007, Diethelm Wuertz, GPL
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
#  yahooImport           Downloads market data from Yahoo's web site  
#  yahooSeries           Easy to use download from Yahoo  
# FUNCTION:             IMPORT STATISTICS - EXPERIMENTAL:
#  keystatsImport        Downloads key statistics from Yahoo's web site
#  briefingImport        Downloads briefings from Yahoo's Internet site
################################################################################


################################################################################
# FUNCTION:             IMPORT DATA FUNCTIONS:
#  yahooImport           Downloads market data from Yahoo's web site 
#  yahooSeries           Easy to use download from Yahoo


yahooImport = 
function (query, file = "tempfile", 
source = "http://chart.yahoo.com/table.csv?", save = FALSE, sep = ";", 
swap = 20, try = TRUE)
{   
    # A function implemented by Diethelm Wuertz

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
        if (inherits(z, "try-error") || inherits(z, "Error")) {
            return("No Internet Access") 
        } else {
            return(z) 
        }
    } else {
        # For S-Plus Compatibility:
        method <- if(is.R()) NULL else "wget"
    
        # Download the file:
        url = paste(source, query, sep = "")
        download.file(url = url, destfile = file, method = method)
        
        # Read data and revert:
        x = X = read.table(file, header = TRUE, sep = ",")
        n = dim(x)[1]
        x = x[n:1, ]
        
        # Result:
        colnames(x)[1] <- "%Y-%m-%d"
        ### rownames(x) = 1:n
        z = data.frame(x)
   
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


yahooSeries = 
function(symbols = c("^DJI", "IBM"), from = NULL, to = NULL, 
nDaysBack = 365, quote = c("Open", "High", "Low", "Close", "Volume"), 
aggregation = c("d", "w", "m"), returnClass = c("timeSeries", "ts", 
"matrix", "data.frame"), getReturns = FALSE, ...)
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
    #   yahooSeries(symbols = "IBM", aggregation = "w")
    #   yahooSeries(symbols = c("^DJI", "IBM"))
    #   yahooSeries(symbols = c("^DJI", "IBM"), aggregation = "w")
    #   yahooSeries(aggregation = "m", nDaysBack = 10*366)
    #   yahooSeries(returnSeries = TRUE)
    
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
    yearFrom = substring(from, 1, 4)
    monthFrom = as.character(as.integer(substring(from, 6,7))-1)
    dayFrom = substring(from, 9, 10)
    
    # Extract Atoms - To:
    yearTo = substring(to, 1, 4)
    monthTo = as.character(as.integer(substring(to, 6,7))-1)
    dayTo = substring(to, 9, 10)
    
    # Download:
    for (i in 1:length(symbols)) {
        query = paste("s=", symbols[i], "&a=", monthFrom, "&b=", dayFrom, 
            "&c=", yearFrom, "&d=", monthTo, "&e=", dayTo, "&f=", yearTo, 
            "&g=", aggregation[1], "&x=.csv", sep = "")  
        imported = yahooImport(query)@data
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
    if (getReturns) X = returnSeries(X, ...)  
    
    # Return as Object ?
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


################################################################################
# FUNCTION:             IMPORT STATISTICS - EXPERIMENTELL:
#  keystatsImport       Downloads key statistics from Yahoo's web site 


keystatsImport =  
function (query, file = "tempfile", source = "http://finance.yahoo.com/q/ks?s=", 
save = FALSE, try = TRUE) 
{   
    # A function implemented by Diethelm Wuertz and Matthew C.Keller

    # Description:
    #   Downloads Key Statistics on shares from Yahoo's Internet site
    
    # Example:
    #   keystatsImport("YHOO")
    #   keystatsImport("IBM")
    #   DEBUG:
    #       query = "IBM"
    #       file = "tempfile"; source = "http://finance.yahoo.com/q/ks?s="
    #       save = FALSE; try = TRUE; method = NULL
    
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
        # For S-Plus Compatibility:
        if (class(version) != "Sversion") {
            method = NULL
        } else {
            method = "wget"
        }
        
        # Download and Scan:
        url = paste(source, query, sep = "")
        download.file(url = url, destfile = file, method = method)
        x = scan(file, what = "", sep = "\n")
        
        # Extract Data Records:
        x = x[grep("datamodoutline1", x)]
        
        # Clean up HTML:
        x = gsub("/", "", x, perl = TRUE)
        x = gsub(" class=.yfnc_datamodoutline1.", "", x, perl = TRUE)
        x = gsub(" colspan=.2.", "", x, perl = TRUE)
        x = gsub(" cell.......=...", "", x, perl = TRUE)
        x = gsub(" border=...", "", x, perl = TRUE)
        x = gsub(" class=.yfnc_tablehead1.", "", x, perl = TRUE)
        x = gsub(" class=.yfnc_tabledata1.", "", x, perl = TRUE)
        x = gsub(" width=.75%.>", "", x, perl = TRUE)
        x = gsub(" width=.100%.", "", x, perl = TRUE)
        x = gsub(" size=.-1.", "", x, perl = TRUE)
        x = gsub("<.>", "", x, perl = TRUE)
        x = gsub("<..>", "", x, perl = TRUE)
        x = gsub("<....>", "", x, perl = TRUE)
        x = gsub("<table>", "", x, perl = TRUE)
        x = gsub("<sup>.<sup>", "", x, perl = TRUE)
        x = gsub("&amp;", "&", x, perl = TRUE)
        x = gsub("<td", " @ ", x, perl = TRUE)
        x = gsub(",", "", x, perl = TRUE)

        # Create Matrix:
        x = unlist(strsplit(x, "@" ))
        x = x[ grep(":", x) ]
        x = gsub("^ ", "", x, perl = TRUE)
        Index = grep("^ ", x)
        if (length(Index) > 0) x = x[-Index]
        x = gsub(" $", "", x, perl = TRUE)
        x = gsub(":$", ":NA", x, perl = TRUE)
        
        # If there are two ":" in a line ...
        x = sub(":", "@", x)
        x = sub(":", "/", x)
        
        # Convert to matrix:
        x = matrix(unlist(strsplit(x, "@" )), byrow = TRUE, ncol = 2)
        
        # Add Current Date:
        stats = as.character(Sys.Date())
        x = rbind(c("Symbol", query), c("Date", stats), x)
        X = as.data.frame(x[, 2])
        rownames(X) = x[, 1] 
        colnames(X) = "Value"
    }
    
    # Return Value:
    X
}


# ------------------------------------------------------------------------------


briefingImport =  
function (query, file = "tempfile", source = "http://finance.yahoo.com/q/ud?s=", 
save = FALSE, try = TRUE) 
{   
    # A function implemented by Diethelm Wuertz and Matthew C.Keller

    # Description:
    #   Downloads Key Statistics on shares from Yahoo's Internet site
    
    # Example:
    #   .briefingImport("YHOO")
    #   .briefingImport("IBM")
    #   DEBUG:
    #       query = "IBM"
    #       file = "tempfile"; source = "http://finance.yahoo.com/q/ks?s="
    #       save = FALSE; try = TRUE; method = NULL
    
    # FUNCTION:
    
    # Download:
    if (try) {
        # First try if the Internet can be accessed:
        z = try(.briefingImport(file = file, source = source, 
            query = query, save = save, try = FALSE))
        if (class(z) == "try-error" || class(z) == "Error") {
            return("No Internet Access")
        }
        else {
            return(z)
        }
    } else {
        # For S-Plus Compatibility:
        if (class(version) != "Sversion") {
            method = NULL
        } else {
            method = "wget"
        }
        
        # Download:
        url = paste(source, query, sep = "")
        download.file(url = url, destfile = file, method = method)
        x = scan(file, what = "", sep = "\n")
        
        # Extract Data Records:
        x = x[grep("Briefing.com", x)]

        x = gsub("</", "<", x, perl = TRUE)
        x = gsub("/", " / ", x, perl = TRUE)
        x = gsub(" class=.yfnc_tabledata1.", "", x, perl = TRUE)
        x = gsub(" align=.center.", "", x, perl = TRUE)
        x = gsub(" cell.......=...", "", x, perl = TRUE)
        x = gsub(" border=...", "", x, perl = TRUE)
        x = gsub(" color=.red.", "", x, perl = TRUE)
        x = gsub(" color=.green.", "", x, perl = TRUE)
        x = gsub("<.>", "", x, perl = TRUE)
        x = gsub("<td>", "@", x, perl = TRUE)
        x = gsub("<..>", "", x, perl = TRUE)
        x = gsub("<...>", "", x, perl = TRUE)
        x = gsub("<....>", "", x, perl = TRUE)
        x = gsub("<table>", "", x, perl = TRUE)
        x = gsub("<td nowrap", "", x, perl = TRUE)
        x = gsub("<td height=....", "", x, perl = TRUE)
        x = gsub("&amp;", "&", x, perl = TRUE)
        
        x = unlist(strsplit(x, ">"))
        
        x = x[ grep("-...-[90]", x, perl = TRUE) ]
        nX = length(x)
        # The last record has an additional @, remove it ...
        x[nX] = gsub("@$", "", x[nX], perl = TRUE)
        x = unlist(strsplit(x, "@"))
        x[x == ""] = "NA"
        x = matrix(x, byrow = TRUE, ncol = 9)[, -c(2,4,6,8)]
        x[, 1] = as.character(strptime(x[, 1], format = "%d-%b-%y"))
        colnames(x) = c("Date", "ResearchFirm", "Action", "From", "To")
        x = x[nrow(x):1, ]
        X = as.data.frame(x)
    }
    
    # Return Value:
    X
}       
        

################################################################################

