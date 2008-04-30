
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
#  yahooImport           Downloads market data from chart.yahoo.com
#  yahooSeries           Easy to use download from chart.yahoo.com
################################################################################


yahooImport <-
    function (query, file = "tempfile",
    source = "http://chart.yahoo.com/table.csv?",
    save = FALSE, sep = ";", try = TRUE)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Downloads market data from Yahoo's web site

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
            title = "Web Data Import from chart.yahoo.com",
            description = as.character(date()) )
        return(ans)
    }

    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


yahooSeries <-
    function(symbols, from = NULL, to = Sys.timeDate(), nDaysBack = 366, quote=c("Open","High","Low","Close","Volume"), aggregation = c("d","w","m"), ...)
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

    # FUNCTION:

    # Match Arguments:
    aggregation = match.arg(aggregation)

    # Internal Univariate Download Function:
    # symbol = "IBM", from = NULL, to = NULL, nDaysBack = 365,
    # quote = "Close", aggregation = c("d", "w", "m"),
    # returnClass = c("timeSeries", "ts", "matrix", "data.frame")

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

    # Return Value:
    X
}


################################################################################

