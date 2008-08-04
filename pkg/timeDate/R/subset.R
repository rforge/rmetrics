
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
#   1999 - Diethelm Wuertz, GPL
#   2007 - Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@phys.ethz.ch>
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# MEHODS:                   SUBSETTING TIMEDATE OBJECTS:
#  [.timeDate                Extracts/replaces subsets from 'timeDate' objects
################################################################################


"[.timeDate" <-
    function(x, ..., drop = TRUE)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Extracts or replaces subsets from 'timeDate' objects

    # Arguments:
    #   x - a 'timeDate' object

    # Value:
    #   Returns a subset from a 'timeDate' object.

    # FUNCTION:

    # Character Type Subsetting:
    subset = I(...)
    if (is.character(subset)){
        if (timeDate:::.subsetCode(subset) == "SPAN") {
            # Subsetting by Span Indexing:
            return(timeDate:::.subsetBySpan(x, subset))
        } else {
            # Subsetting by Python Indexing:
            return(timeDate:::.subsetByPython(x, subset))
        }
    }

    # Subsets:
    z = as.POSIXlt(x@Data)
    val <- lapply(z, "[", ..., drop = drop)
    attributes(val) <- attributes(z)
    val = as.POSIXct(val)

    # Return Value:
    new("timeDate",
        Data = val,
        format = x@format,
        FinCenter = x@FinCenter)
}


#-------------------------------------------------------------------------------


"[<-.timeDate" <-
    function(x, ..., value)
{
    # A function implemented by Yohan Chalabi

    # Description:
    #   Extracts or replaces subsets from 'timeDate' objects

    # Arguments:
    #   x - a 'timeDate' object

    # Value:
    #   Returns a subset from a 'timeDate' object.

    # FUNCTION:

    if (!inherits(value, "timeDate"))
        value <- as.timeDate(value)

    # Subsets:
    z = as.POSIXlt(x@Data)
    value <- as.POSIXlt(value@Data)
    val <- "[<-"(z, ..., value)
    val = as.POSIXct(val)

    # Return Value:
    new("timeDate",
        Data = val,
        format = x@format,
        FinCenter = x@FinCenter)
}


################################################################################


.subsetCode <-
function(subset)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Defines codes for different types of subsettings

    # Details:

    # Python Like Indexing:
    #   Subset:             Code:
    #   ISO8601             00000
    #   ::                  00010
    #   ISO8601::ISO8601    00100
    #   ISO8601::           01000
    #   ::ISO8601           10000

    # Indexing by Spans:
    #   subsets = tolower(c(
    #     "last 1 Month(s)",
    #     "last 1 Week(s)",
    #     "last 1 Day(s)",
    #     "last 1 hour(s)",
    #     "last 1 minute(s)",
    #     "last 1 second(s)"))

    # Example:
    #   .subsetCode("2008-03::")
    #   .subsetCode("last 2 Weeks")

    # Code String:
    if (length(grep("last", subset)) > 0 ) {
        code = "SPAN"
    } else {
        code = paste(
            sign(regexpr("^::[0-9]", subset)[1]+1),
            sign(regexpr("[0-9]::$", subset)[1]+1),
            sign(regexpr("[0-9]::[0-9]", subset)[1]+1),
            as.integer(subset == "::"),
            length(grep("[a-Z]", subset)), sep = "")
    }

    # Return Value:
    code
}


# ------------------------------------------------------------------------------



.subsetByPython <-
function(x = timeCalendar(), subset = "::")
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Subsets a timeDate object by python like indexing

    # Arguments:
    #   x - a timeDate object
    #   subset - a python like subset string

    # Example:
    #   .subsetByPython(subset = "2008")
    #   .subsetByPython(subset = "2008-07")
    #   .subsetByPython(subset = "::")
    #   .subsetByPython(subset = "2008-07::2008-09")
    #   .subsetByPython(subset = "2008-07::")
    #   .subsetByPython(subset = "::2008-06")

    # FUNCTION:

    # Subset Code:
    code = .subsetCode(subset)

    # Full Vector:
    ans = x

    # Date String:
    date = strsplit(subset, "::")[[1]]

    # 1. DATE
    if(code == "00000") ans = x[grep(date, format(x))]

    # 2. ::
    if(code == "00010") ans = x

    # Internal Functions:
    .completeStart = function(date) {
        substr(paste(date, "-01-01", sep = ""), 1, 10) }
    .completeEnd = function(date) {
        if (nchar(date) == 4)
            paste(date, "-12-31", sep = "") else
        if (nchar(date) == 7)
            format(timeLastDayInMonth(paste(date, "-01", sep = ""))) else
        if (nchar(date) == 10)
            date }

    # 3. DATE::DATE:
    if(code == "00100")
        ans = window(x, .completeStart(date[1]), .completeEnd(date[2]))

    # 4. DATE::
    if(code == "01000")
        ans = window(x, .completeStart(date[1]), end(x))

    # 5. ::DATE
    if(code == "10000")
        ans = window(x, start(x), .completeEnd(date[2]))

    # Return Value
    ans
}


# ------------------------------------------------------------------------------


.subsetBySpan  <-
function(x = timeCalendar(), subset = "last 3 Months")
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Subsets a timeDate object by span indexing

    # Arguments:
    #   x - a timeDate object
    #   subset - a span like subset string

    # Note:
    #   ye[ars]
    #   mo[nths]
    #   da[ys]
    #   ho[urs]
    #   mi[nutes]
    #   se[conds]
    #       ... only "last" spans are implemented

    # Example:
    #   .subsetBySpan(timeCalendar(), "last 2 months")
    #   .subsetBySpan(timeCalendar(), "last 62 days")

    # FUNCTION:

    # Get Code:
    code = .subsetCode(subset)
    stopifnot(code == "SPAN")

    # Settings:
    duration = as.numeric(strsplit(subset, " ")[[1]][2])
    len = c(ye = 31622400, mo = 2678400, da = 86400, ho = 3600, mi = 60, se = 1)
    unit = tolower(substr(strsplit(subset, " ")[[1]][3], 1, 2))
    offset = len[unit]*duration

    # Return Value:
    window(x, start = end(x) - offset, end(x))
}


################################################################################

