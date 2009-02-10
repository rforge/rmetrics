
# This R package is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This R package is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this R package; if not, write to the
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

setGeneric("timeDate",
           function(charvec, format = NULL, zone = "", FinCenter = "")
           standardGeneric("timeDate"))

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
    #   timeDate(c("2004-01-01", "2004-01-01"))
    #   timeDate("2004-01-01 00:00:00")
    #   timeDate("20040101")
    #   timeDate("200401011600")
    #   timeDate("20040101000000")
    #   timeDate("1/1/2004") # American format
    #   timeDate("2004-01-01", FinCenter = "GMT")
    #   timeDate("20040101", FinCenter = "GMT")
    #   td = timeDate("2004-01-01", FinCenter = "GMT"); timeDate(td)
    #   td = timeDate("20040101", FinCenter = "GMT"); timeDate(td)

# ------------------------------------------------------------------------------

setMethod("timeDate", "character",
          function(charvec, format = NULL, zone = "", FinCenter = "")
{

    # Settings and Checks:
    if (zone == "")
        zone = getRmetricsOptions("myFinCenter")
    if (FinCenter == "")
        FinCenter = getRmetricsOptions("myFinCenter")

    # ISO Date/Time Format:
    isoDate   <- "%Y-%m-%d"
    isoFormat <- "%Y-%m-%d %H:%M:%S"

    # Autodetect Format :
    if (is.null(format))
        format <- whichFormat(charvec[1])
    if (identical(format,"unknown"))
        return(timeDate(NA, zone = zone, FinCenter = FinCenter))

    # if entries of charvec are not of same length, replace them with NA's
    if (any(nc <- !(nchar(charvec) == nchar(charvec[1])))) {
        is.na(charvec) <- nc
        warning("'charvec' entries of different number of characters are replaced by NA's")
    }

    # Midnight Standard & conversion to isoFormat:
    charvec <- midnightStandard(charvec, format)

    # convert to POSIXct as it is
    ct <- as.POSIXct(charvec, format = isoFormat, tz="GMT")

    ## Do conversion
    ## YC: .formatFinCenterNum faster than .formatFinCenter
    num = .formatFinCenterNum(c(unclass(ct)), zone, type = "any2gmt")
    # it is important to set manually the tzone flag,
    Data <- as.POSIXct(num, origin = "1970-01-01", tz = "GMT")
    attr(Data, "tzone") <- "GMT"

    new("timeDate",
        Data = as.POSIXct(Data),
        # Note format is automatically created in
        # initialize,timeDate-method
        FinCenter = as.character(FinCenter))
})

# ------------------------------------------------------------------------------

## timeDate
setMethod("timeDate", "timeDate",
          function(charvec, format = NULL, zone = "", FinCenter = "")
{
    # if zone not provided, change only the FinCenter in charvec (timeDate)
    if (zone == "") {
        if (FinCenter != "")
            finCenter(charvec) <- FinCenter
        charvec
    } else {
        callGeneric(format(charvec), zone = zone, FinCenter = FinCenter)
    }
})

# ------------------------------------------------------------------------------

## POSIXt
setMethod("timeDate", "POSIXt",
          function(charvec, format = NULL, zone = "", FinCenter = "")
{



    if (!(zone %in% c("", "GMT", "UTC"))) {
        callGeneric(format(charvec), zone = zone, FinCenter = FinCenter)
    } else {

        # Since zone is not provided consider that charvec is in GMT
        charvec <- as.POSIXct(charvec)
        attr(charvec, "tzone") <- "GMT"

        # FinCenter
        if (FinCenter == "")
            FinCenter = getRmetricsOptions("myFinCenter")

        new("timeDate",
            Data = charvec,
            # Note format is automatically created in
            # initialize,timeDate-method
            FinCenter = as.character(FinCenter))
    }
})


# ------------------------------------------------------------------------------

## Date
setMethod("timeDate", "Date",
          function(charvec, format = NULL, zone = "", FinCenter = "")
{
    charvec <- format(charvec)
    format <- "%Y-%m-%d"
    callGeneric()
})

# ------------------------------------------------------------------------------

## numeric
setMethod("timeDate", "numeric",
          function(charvec, format = NULL, zone = "", FinCenter = "")
{
      charvec <- as.POSIXct(as.numeric(charvec),
                            origin = "1970-01-01", tz = "GMT")
      callGeneric()
})

# ------------------------------------------------------------------------------

## missing
setMethod("timeDate", "missing",
          function(charvec, format = NULL, zone = "", FinCenter = "")
    callGeneric(Sys.time(), format, zone, FinCenter))

# ------------------------------------------------------------------------------

## ANY
setMethod("timeDate", "ANY",
          function(charvec, format = NULL, zone = "", FinCenter = "")
    callGeneric(as.character(charvec), format, zone, FinCenter))

################################################################################

.formatFinCenterNum <-
    function(num, FinCenter, type = c("gmt2any", "any2gmt"))
{
    # A function implemented by Diethelm Wuertz and Yohan Chalabi

    # Description:
    #   Internal function used by function timeDate()

    if (FinCenter == "GMT" || FinCenter == "UTC")
        return(num)

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

    num + signum * dst.list$offSet[ findInterval(num, dst.list$numeric)]
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
    dt = strptime(charvec, "%Y-%m-%d %H:%M:%S", tz = "GMT")

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
