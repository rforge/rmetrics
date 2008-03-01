
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
# FUNCTION:                 GENERATION OF TIME SERIES OBJECTS:
#  timeSeries                Creates a 'timeSeries' object from scratch
#  readSeries                Reads a spreadsheet and creates a 'timeSeries'
#  applySeries               Applies a function to blocks of a 'timeSeries'
#  orderStatistics           Compute order statistic of a 'timeSeries'
# FUNCTION:                 DATA SLOT AND CLASSIFICATION:
#  seriesData                Extracts data slot from 'timeSeries' object
#  isUnivariate              Tests if 'timeSeries' object is univariate
#  isMultivariate            Tests if 'timeSeries' object is multivariate
################################################################################


################################################################################
# FUNCTION:            GENERATION OF TIME SERIES OBJECTS:
#  timeSeries           Creates a 'timeSeries' object from scratch
#  readSeries           Reads from a spreadsheet and creates a 'timeSeries'
#  applySeries          Applies a function to blocks of a 'timeSeries'
#  orderStatistics      Compute order statistic of a 'timeSeries'


timeSeries =
function (data, charvec, units = NULL, format = NULL, zone = myFinCenter,
FinCenter = myFinCenter, recordIDs = data.frame(), title = NULL,
documentation = NULL, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Creates a 'timeSeries' object from scratch.

    # Arguments:
    #   data -a 'data frame or a 'matrix' object of numeric data.
    #   charvec - a character vector of dates and times.
    #   units - an optional units string, NULL defaults an empty
    #       string.
    #   format - the format specification of the input character
    #       vector.
    #   zone - the time zone or financial center where the data were
    #       recorded.
    #   FinCenter - a character with the the location of the
    #       financial center named as "continent/city".
    #   recordIDS - stores record IDs in form of a data frame
    #   title - an optional title string, if not specified the inputs
    #       data name is deparsed.
    #   documentation - optional documentation string.

    # Value:
    #   Returns a S4 object of class 'timeSeries'.
    #   positions - these are the POSIX date/time strings created
    #       by default from the dimnames of the data matrix, or
    #       alternatively if the data are read from a CSV file,
    #       the first column is expected to hold the positions,
    #       and the column name the "format" string.

    # Details:
    #    This is a minimal implementation of the SPLUS "timeSeries"
    #    object.

    # Example:
    #   data.mat = matrix(round(rnorm(30),2), 10)
    #   charvec =  paste("2004-01-", c(paste("0", 1:9, sep=""), 10:30), sep="")
    #   timeSeries(data.mat, charvec)

    # FUNCTION:

    # This allows data to be a vector as input ...
    if (is.vector(data)) data = matrix(data)

    # Trace:
    if (FinCenter == "") FinCenter = "GMT"
    trace = FALSE

    # Missing charvec:
    if (missing(charvec)) {
        N = dim(as.matrix(data))[1]
        charvec = timeSequence(from = "1970-01-01", length.out = N,
            zone = "GMT", FinCenter = "GMT")
    }

    # charvector | Time Positions:
    if (is(charvec, "timeDate")) {
        timeDates = charvec
    } else {
        if (is.null(format)) format = .whichFormat(charvec)
        timeDates = timeDate(charvec = charvec,
            format = format, zone = zone, FinCenter = FinCenter)
    }

    # Data | Dimension Names:
    if (is.timeSeries(data)) {
        recordIDs = data@recordIDs
        data = data@Data
        rownames(data) = c(as.character(timeDates))
        units = colnames(data)
    } else {
        data = as.matrix(data)
        rownames(data) = c(as.character(timeDates))
        if (is.null(units)) {
            if (is.null(colnames(data))) {
                units = paste("TS.", 1:dim(data)[2], sep = "")
            } else {
                units = colnames(data)
            }
        }
        colnames(data) = units
    }

    # Record IDs:
    # DW:
    # No double row Names in data.frames - this generates problems!
    # if (sum(dim(recordIDs)) > 0)
        # rownames(recordIDs) = c(as.character(charvec))

    # Add title and Documentation:
    if (is.null(title)) title = "Time Series Object"
    if (is.null(documentation)) documentation = as.character(date())

    # Result:
    ans = new("timeSeries",
        Data = as.matrix(data),
        positions = rownames(data),
        format = timeDates@format,
        FinCenter = timeDates@FinCenter,
        units = as.character(units),
        recordIDs = recordIDs,
        title = as.character(title),
        documentation = as.character(documentation)
        )
    attr(ans, "dimension") = c(NCOL(data), NROW(data))

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


readSeries =
function(file, header = TRUE, sep = ";", zone = myFinCenter,
FinCenter = myFinCenter, title = NULL, documentation = NULL, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Reads from a spreadsheet and creates a 'timeSeries' object

    # Arguments:
    #   file - the filename of a spreadsheet data set from which
    #       to import the data records.
    #   header -
    #   sep -
    #   zone - the time zone or financial center where the data were
    #       recorded.
    #   FinCenter - a character with the the location of the
    #       financial center named as "continent/city". By default
    #       an empty string which means that internally "GMT" will
    #       be used.
    #   title - an optional title string, if not specified the inputs
    #       data name is deparsed.
    #   documentation - optional documentation string.

    # Value:
    #   Returns a S4 object of class 'timeSeries'.

    # Notes:
    #   Note we expect that the header of the spreadsheet file in
    #   the first cell holds the time/date format specification!

    # FUNCTION:

    # Read Data:
    df = read.table(file = file, header = header, sep = ";", ...)

    # Create Time Series:
    ans = as.timeSeries(df)

    # Add title and Documentation:
    if (is.null(title)) ans@title = "Time Series Object"
    if (is.null(documentation)) ans@documentation = as.character(date())

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


applySeries =
function(x, from = NULL, to = NULL, by = c("monthly", "quarterly"),
FUN = colAvgs, units = NULL, format = x@format, zone = x@FinCenter,
FinCenter = x@FinCenter, recordIDs = data.frame(), title = x@title,
documentation = x@documentation, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Apply a function to the margins of a 'timeSeries' object

    # Details:
    #   This function can be used to aggregate and coursen a
    #   'timeSeries' object.

    # Arguments:
    #   x - a 'timeSeries' object to be aggregated
    #   from, to - two 'timeDate' position vectors which size the
    #       blocks
    #   by - calendarical block, only active when both 'from'
    #       and 'to' are NULL
    #   FUN - function to be applied, by default 'colAvgs'
    #   units - a character vector with column names, allows to
    #       overwrite the column names of the input 'timeSeries'
    #       object.

    # Value:
    #   Returns a S4 object of class 'timeSeries'.

    # Notes:
    #   The size of the 'moving' window and the selection of an
    #   'adj'-acent endpoint are not needed, all the information
    #   is kept in the 'from' and 'to' position vectors.

    # FUNCTION:

    # Check object:
    if (class(x) != "timeSeries") stop("s is not a timeSeries object")

    # Monthly and Quarterly from and to:
    if (is.null(from) & is.null(to)) {
        if (by[1] == "monthly") {
            # Use monthly blocks:
            from = unique(timeFirstDayInMonth(seriesPositions(x)))
            to = unique(timeLastDayInMonth(seriesPositions(x)))
        } else if (by[1] == "quarterly") {
            from = unique(timeFirstDayInQuarter(seriesPositions(x)))
            to = unique(timeLastDayInQuarter(seriesPositions(x)))
        } else {
            stop("by must be eiter monthly or quarterly")
        }
        from@FinCenter = to@FinCenter = FinCenter
    }

    # Column Names:
    colNames = units

    # Function:
    fun = match.fun(FUN)

    # Blocks:
    j.pos = as.POSIXct(seriesPositions(x))
    j.from = as.POSIXct(from)
    j.to = as.POSIXct(to)

    # Iterate:
    y = x@Data
    pos = seriesPositions(x)
    rowNames = rownames(x@Data)
    rowBind = NULL
    for (i in 1:from@Dim) {
        test = (j.pos >= j.from[i] & j.pos <= j.to[i])
        # make sure that cutted is a matrix ...
        cutted = as.matrix(y[test, ])
        ### if (sum(test)>0) rownames(cutted) <- rowNames[test]
        ans = fun(cutted, ...)
        rowBind = rbind(rowBind, ans)
    }
    rownames(rowBind) = as.character(to)
    if (is.null(colNames)) {
        units = x@units
    } else {
        units = colNames }

    # Return Value:
    timeSeries(data = rowBind, charvec = as.character(to), units = units,
        format = format, zone = zone, FinCenter = FinCenter, recordIDs =
        recordIDs, title = title, documentation = documentation, ...)
}


.applySeries =
function (x, from = NULL, to = NULL, by = c("monthly", "quarterly"),
FUN = colAvgs, units = NULL, ...)
{
    # Old/Alternative Version

    # Chreck for 'timeSeries' Object:
    stopifnot(is.timeSeries(x),
              is(from, "timeDate") || is.null(from),
              is(to,   "timeDate") || is.null(to))

    # Allow for colMeans:
    if (substitute(FUN) == "colMeans") FUN = "colAvgs"

    # Monthly and Quarterly from and to:
    if (is.null(from) & is.null(to)) {
        by = match.arg(by)
        if (by == "monthly") {
            from = unique(timeFirstDayInMonth(seriesPositions(x)))
            to = unique(timeLastDayInMonth(seriesPositions(x)))
        }
        else if (by == "quarterly") {
            from = unique(timeFirstDayInQuarter(seriesPositions(x)))
            to = unique(timeLastDayInQuarter(seriesPositions(x)))
        }
        from@FinCenter = to@FinCenter = x@FinCenter
    }

    # Start Cutting Process:
    fun = match.fun(FUN)
    cutted = NULL
    i = 1

    # Find First Interval which is not empty:
    while (is.null(cutted)) {
        cutted = cut(x, from[i], to[i])
        if (!is.null(cutted)) {
            # Non empty Interval:
            ans = fun(cutted, ...)
        }
        i = i + 1
    }
    # Continue up to the end:
    for (j in i:from@Dim) {
        cutted = cut(x, from[j], to[j])
        if (!is.null(cutted)) {
            # Non empty Interval:
            newAns = fun(cutted, ...)
            ans = rbind(ans, newAns)
        }
    }

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


orderStatistics =
function(x)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Compute the order statistics for a 'timeSeries object

    # Value:
    #   A named list with the order statistics for each column of
    #   the inputted series.

    # FUNCTION:

    # Order Statistics
    Units = x@units
    nUnits = length(Units)
    ans = list()
    for (i in 1:nUnits) {
        X = x[, i]
        positions = X@positions
        S = sort(X@Data, index.return = TRUE)
        X@Data = matrix(S$x, ncol = 1)
        X@positions = rownames(X@Data) = positions[S$ix]
        colnames(X@Data) = Units[i]
        TEXT = paste("ans$", Units[i], "=X", sep = "")
        eval(parse(text = TEXT))
    }

    # Return Value:
    ans

}


################################################################################
#  seriesData           Extracts data slot from 'timeSeries' object
#  isUnivariate         Tests if an object of class 'timeSeries' is univariate
#  isMultivariate       Tests if an object of class 'timeSeries' is multivariate

seriesData =
function(object)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #    Returns the series Data of an ordered data object.

    # Arguments:
    #   object - a 'timeSeries' object

    # Value:
    #    Returns an object of class 'matrix'.

    # FUNCTION:

    # Test:
    if(class(object) != "timeSeries") stop("Object is not a time Series")

    # Get Data Slot:
    ans = object@Data

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


isUnivariate =
function(x)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Tests if a time series is univariate

    # FUNCTION:

    # Return Value:
    if (NCOL(x) == 1) return(TRUE) else return(FALSE)
}


# ------------------------------------------------------------------------------


isMultivariate =
function(x)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Tests if a time series is multivariate

    # FUNCTION:

    # Examples:
    #   isMultivariate(as.timeSeries(data(daxRet)))

    # FUNCTION:

    # Return Value:
    if (NCOL(x) > 1) return(TRUE) else return(FALSE)
}



################################################################################

