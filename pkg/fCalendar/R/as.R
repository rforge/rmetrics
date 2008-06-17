
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
# METHOD:                   DESCRIPTION:
#  as.character.timeDate     Returns a 'timeDate' object as 'character' string
#  as.double.timeDate        Returns a 'timeDate' object as 'numeric' object
#  as.data.frame.timeDate    Returns a 'timeDate' object as 'data.frame' object
#  as.list.timeDate          Returns a 'timeDate' object as 'list' object
#  as.POSIXct.timeDate       Returns a 'timeDate' object as 'POSIXct' object
#  as.POSIXlt.timeDate       Returns a 'timeDate' object as 'POSIXlt' object
#  as.Date.timeDate          Returns a 'timeDate' object as 'Date' object
################################################################################


as.character.timeDate <-
    function(x, ...)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns a 'timeDate' object as character string

    # Arguments:
    #   x - a 'timeDate' object
    #   ... - arguments passed to other methods.

    # Value:
    #   Returns 'x' as a character vector.

    # FUNCTION:

    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")
    Sys.setenv(TZ = "GMT")

    # Check Class Type:
    if (!inherits(x, "timeDate")) stop("Wrong class type")

    # Format:
    ans = format.POSIXct(x@Data)
    attr(ans, "control") = c(FinCenter = x@FinCenter)

    # Reset Time Zone:
    Sys.setenv(TZ = myTZ)

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


as.double.timeDate <-
    function(x,
    units = c("auto", "secs", "mins", "hours", "days", "weeks"), ...)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns a 'timeDate' object as 'numeric' vector

    # Arguments:
    #   x - a 'timeDate' object
    #   units - a character string denoting in which units the
    #       elements of the numeric vector are measured

    # Value:
    #   Returns 'x' as a numeric vector.

    # FUNCTION:

    # Set time zone to GMT:
    myTZ = Sys.getenv("TZ")
    Sys.setenv(TZ = "GMT")

    # as double:
    ct = timeDate(x, zone = x@FinCenter, FinCenter = "GMT")@Data
    origin = as.POSIXct("1970-01-01", tz = "GMT")
    dt = difftime(ct, origin, units = units)
    units = attr(dt, "units")
    ans = as.double(difftime(ct, origin, units = units))
    attr(ans, "FinCenter")<-"GMT"
    attr(ans, "units")<-units
    if (units == "secs")
        attr(ans, "origin")<-"1970-01-01 00:00:00 GMT"
    if (units == "mins" | units == "hours")
        attr(ans, "origin")<-"1970-01-01 00:00 GMT"
    if (units == "days" | units == "weeks")
        attr(ans, "origin")<-"1970-01-01 GMT"

    # Reset Time Zone:
    Sys.setenv(TZ = myTZ)

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


as.data.frame.timeDate <-
    function(x, ...)
{
    # A function implemented by Diethelm Wuertz and Yohan Chalabi

    # Description:
    #   Returns a 'timeDate' object as data frame

    # Arguments:
    #   x - a 'timeDate' object

    # Value:
    #   Returns 'x' as a data frame.

    # FUNCTION:

    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")
    Sys.setenv(TZ = "GMT")

    # Check Class Type:
    stopifnot(inherits(x, "timeDate"))

    # Data Frame:
    ans <- as.data.frame.POSIXlt(x@Data, ...)
    nm <- paste(deparse(substitute(x), width.cutoff = 500), collapse = " ")
    colnames(ans) <- paste(x@FinCenter, ":", nm, sep = "")
    attr(ans, "control") <- c(FinCenter = x@FinCenter)

    # Reset Time Zone:
    Sys.setenv(TZ = myTZ)

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------

as.list.timeDate <-
    function(x, ...)
{
    # A function implemented by Yohan Chalabi and Diethelm Wuertz

    # Description:
    #   Returns a 'timeDate' object as list
    # important for functions like sapply and lapply

    # Arguments:
    #   x - a 'timeDate' object

    # Value:
    #   Returns 'x' as a data frame.

    # FUNCTION:

    # Check Class Type:
    stopifnot(inherits(x, "timeDate"))

    ans <- vector("list", length(x))

    for (i in seq(length(x)))
        ans[i] <- x[i]

    # Return Value:
    ans
}

# ------------------------------------------------------------------------------

as.POSIXct.timeDate <-
    function(x, tz = "", ...)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns a 'timeDate' object as POSIXct object

    # Arguments:
    #   x - a 'timeDate' object
    #   tz - a timezone specification to be used for the conversion.
    #       (Not needed when used for 'timeDate' conversions.)

    # Value:
    #   Returns 'x' as an object of class 'POSIXct'.

    # FUNCTION:

    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")
    Sys.setenv(TZ = "GMT")

    # Check Class Type:
    if (!inherits(x, "timeDate")) stop("Wrong class type")

    # POSIXlt:
    ans = x@Data
    attr(ans, "control") = c(FinCenter = x@FinCenter)

    # Reset Time Zone:
    Sys.setenv(TZ = myTZ)

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


as.POSIXlt.timeDate <-
    function(x, tz = "")
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns a 'timeDate' object as 'POSIXlt' object

    # Arguments:
    #   x - a 'timeDate' object
    #   tz - a timezone specification to be used for the conversion.
    #       (Not needed when used for 'timeDate' conversions.)

    # Value:
    #   Returns 'x' as an object of class 'POSIXct'.

    # FUNCTION:

    # Set Timezone to GMT:
    ans = as.POSIXlt(as.POSIXct(x = x, tz = tz))

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


as.Date.timeDate <-
    function(x, method = c("trunc", "round", "next"), ...)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns a 'timeDate' object as 'Date' object

    # Arguments:
    #   x - a 'timeDate' object
    #   method - a character string denoting the method how to
    #       compute the 'Date' object.

    # Value:
    #   Returns 'x' as an object of class 'POSIXct'.

    # FUNCTION:

    # as Date:
    method = match.arg(method)
    if (method == "trunc") {
        ans = as.Date(as.POSIXct(trunc(x)), ...)
    } else if (method == "round") {
        ans = as.Date(as.POSIXct(round(x)), ...)
    } else if (method  == "next") {
        ans = as.Date(as.POSIXct(trunc(x)), ...) + 1
    }

    # Add Attribute:
    attr(ans, "control")<-c(method = method, FinCenter = x@FinCenter)

    # Return Value:
    ans
}


################################################################################

