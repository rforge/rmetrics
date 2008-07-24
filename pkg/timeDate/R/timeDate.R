
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
# FUNCTION:              DESCRIPTION:
#  timeDate               Creates a 'timeDate' object from given dates
#  .formatFinCenter       Internal called by timeDate
#  strptimeDate           Creates for character time stamps a 'timeDate' object
################################################################################


timeDate <-
    function(charvec = Sys.timeDate(), format = NULL, zone = "",
    FinCenter = "")
{
    # A function implemented by Yohan Chalabi and Diethelm Wuertz

    # Description:
    #   Creates a "timeDate' object from a character vector

    # Arguments:
    #   charvec - a character vector of dates and times. Alternatively
    #       it may be a 'timeDate', a 'Date', or a 'POSIXt' object. In
    #       these cases the argument will be coerced into a character
    #       string or character vector.
    #   format - the format specification of the input character
    #       vector. If set to NULL autodetection will be tried.
    #   zone - the time zone or financial center where the data
    #       were recorded.
    #   FinCenter - a character string with the the location of
    #       the financial center named as "continent/city" where the
    #       data will be used.

    # Value:
    #   Returns a S4 object of class 'timeDate'.

    # Note:
    #   Changeover DST not yet fully implemented!

    # Examples:
    #   timeDate("2004-01-01")
    #   timeDate("2004-01-01 00:00:00")
    #   timeDate("20040101")
    #   timeDate("200401011600")
    #   timeDate("20040101000000")
    #   timeDate("1/1/2004") # American format
    #   timeDate("2004-01-01", FinCenter = "GMT")
    #   timeDate("20040101", FinCenter = "GMT")
    #   td = timeDate("2004-01-01", FinCenter = "GMT"); timeDate(td)
    #   td = timeDate("20040101", FinCenter = "GMT"); timeDate(td)

    # FUNCTION:

    # Settings and Checks:
    if (zone == "")
        zone = getRmetricsOptions("myFinCenter")
    if (FinCenter == "")
        FinCenter = getRmetricsOptions("myFinCenter")

    # ISO Date/Time Format:
    isoDate   <- "%Y-%m-%d"
    isoFormat <- "%Y-%m-%d %H:%M:%S"

    if (inherits(charvec, "character")) {
        if (is.null(format))
            # Autodetect Format :
            format <- whichFormat(charvec)
    } else { ## convert from known classes to ISO :
        format <- isoFormat
        charvec <- if (is(charvec, "timeDate")) {
            format(charvec, format)
        }
        else if (inherits(charvec, "Date")) {
            zone <- FinCenter
            format <- isoDate
            format(charvec, format)
        }
        else if (inherits(charvec, "POSIXt")) {
            format(charvec, format)
        }
    }

    # Midnight Standard & conversion to isoFormat:
    charvec <- midnightStandard(charvec, format)

    ## Convert:
    charvec = .formatFinCenter(charvec, zone, type = "any2gmt")
    # it is important to set manually the tzone flag,
    # otherwise tzone "GMT" is used globally on the system.
    # NO : ct <- as.character(strptime(charvec, isoFormat, tz = "GMT"))
    Data <- strptime(charvec, isoFormat)
    attr(Data, "tzone") <- "GMT"

    # check isoFormat is appropriate to show time
#    if (!length(grep("%H|%M|%S", format))) {
    gmt2any <- .formatFinCenter(charvec, FinCenter, type = "gmt2any")
    lt <- strptime(gmt2any, isoFormat)
    time <- unlist(unclass(lt)[1:3])
    format <-
        if (any(time[!is.na(time)] != 0))
            isoFormat
        else
            isoDate
#    }

    new("timeDate",
        Data = as.POSIXct(Data),
        format = format,
        FinCenter = as.character(FinCenter))
}

# ------------------------------------------------------------------------------


.formatFinCenter <-
    function(charvec, FinCenter, type = c("gmt2any", "any2gmt"))
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Internal function used by function timeDate()

    if (FinCenter == "GMT" || FinCenter == "UTC")
        return(charvec)

    ## else start working:

    type <- match.arg(type)
    signum <- switch(type,
                     "gmt2any" = +1,
                     "any2gmt" = -1)
    ##  otherwise give error


    # Get the DST list from the database:
    try <- try(dst.list <- rulesFinCenter(FinCenter), silent = TRUE)
    if (inherits(try, "try-error"))
        stop(gettextf("'%s' is not a valid FinCenter.", FinCenter))
    # Update list with last entry:
    z = as.matrix(dst.list)
    z[dim(z)[1], ]
    vec1 = as.vector(c(z[, 1], "2099-01-01 00:00:00"))
    vec2 = as.vector(c(z[, 2], rev(z[, 2])[1]))
    dst.list = data.frame(ruleChanges = as.character(vec1),
    offSet = as.integer(vec2))
    # Extract the dates when DST was changed:
    dst.dates = as.character(dst.list[, 1])
    # Extract the Offsets to GMT
    dst.offsets = as.character(dst.list[, 2])
    # The new dates ar the charvec's:
    new.dates = charvec
    # The new offsets are still unknown:
    new.offsets = rep(NA, length(charvec))
    # Combine all Dates and Offsets:
    dates = c(dst.dates, new.dates)
    offsets = c(dst.offsets, new.offsets)
    # The number of Offsets:
    n = length(dates)
    # Order the Dates:
    o = order(dates)
    # Dates and Offsets in the right order:
    o.dates = dates[o]
    o.offsets = offsets[o]
    # The points at which we have to determine the offsets
    xout = (1:n)[is.na(o.offsets)]
    # The date indexes:
    x = (1:n)[-xout]
    # The corresponding offsets
    y = o.offsets[x]
    # The new offsets:
    yout = approx(x, y , xout, method = "constant")$y
    # All dates:
    m = length(dst.dates)
    # Put them in the right order:
    # Added DW: 2005-05-27
    idx = order(o[which(o>m)])
    offSets = yout[idx]
    dt = strptime(charvec, "%Y-%m-%d %H:%M:%S")

    ## Return Value:
    format(dt + signum * offSets, format="%Y-%m-%d %H:%M:%S")
}


# ------------------------------------------------------------------------------


strptimeDate <- 
    function(x, format = whichFormat(x), tz = "")
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Creates for character time stamps a 'timeDate' object
    
    # Example:
    #   timeDate(); strptimeDate(as.character(Sys.timeDate()))
    
    # Note:
    #   This function works like strptime.
    
    # FUNCTION:
    
    # Check Arguments:
    stopifnot(is.character(x))
    
    # Settings and Checks:
    if (tz == "")
        tz = getRmetricsOptions("myFinCenter")
    
    # Create 'timeDate':
    ans = timeDate(x, format, zone = tz, FinCenter = tz)
    
    # Return Value:
    ans
}


################################################################################

