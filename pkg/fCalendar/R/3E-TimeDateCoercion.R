
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
# S3 MEHOD:              COERCION AND OBJECT TRANSFORMATIONS:
#  as.timeDate            Implements Use Method
#  as.timeDate.default    Default Method
#  as.timeDate.POSIXt     Returns a 'POSIX' object as 'timeDate' object
#  as.timeDate.Date       Returns a 'POSIX' object as 'timeDate' object
# S3 METHOD:             DESCRIPTION:
#  as.character.timeDate  Returns a 'timeDate' object as 'character' string
#  as.double.timeDate     Returns a 'timeDate' object as 'numeric' object
#  as.data.frame.timeDate Returns a 'timeDate' object as 'data.frame' object
#  as.POSIXct.timeDate    Returns a 'timeDate' object as 'POSIXct' object
#  as.POSIXlt.timeDate    Returns a 'timeDate' object as 'POSIXlt' object
#  as.Date.timeDate       Returns a 'timeDate' object as 'Date' object
# S3 METHOD:             DESCRIPTION:
#  julian.timeDate        Returns Julian day counts since 1970-01-01
#  atoms.timeDate         Returns date/time atoms from a 'timeDate' object
#  months.timeDate        Extracts months atom from a 'timeDate' object
################################################################################


################################################################################
#  as.timeDate            Use Method
#  as.timeDate.POSIXt     Returns a 'POSIX' object as 'timeDate' object
#  as.timeDate.Date       Returns a 'POSIX' object as 'timeDate' object


as.timeDate = 
function(x, zone = NULL, FinCenter = NULL) 
{
    UseMethod("as.timeDate")
}


# ------------------------------------------------------------------------------


as.timeDate.default = 
function(x, zone = myFinCenter, FinCenter = myFinCenter) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns default object as 'timeDate' object
    
    # Arguments:
    #   x - a 'timeDate' object
    
    # Value:
    #   Returns 'x' as a 'timeDate' object. 

    # Changes:
    #
    
    # FUNCTION:
    
    # as timeDate:
    ans = timeDate(charvec = as.character(x), 
        zone = zone, FinCenter = FinCenter)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


as.timeDate.POSIXt = 
function(x, zone = myFinCenter, FinCenter = myFinCenter) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns a 'POSIXt' object as 'timeDate' object
    
    # Arguments:
    #   x - a 'timeDate' object
    
    # Value:
    #   Returns 'x' as a 'timeDate' object. 

    # Changes:
    #
    
    # FUNCTION:
    
    # as timeDate:
    ans = timeDate(charvec = x, zone = zone, FinCenter = FinCenter)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


 
as.timeDate.Date = 
function(x, zone = myFinCenter, FinCenter = myFinCenter) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns a 'Date' object as 'timeDate' object
    
    # Arguments:
    #   x - a 'timeDate' object
    #   ... - arguments passed to other methods.
    
    # Value:
    #   Returns 'x' as a character vector. 

    # Changes:
    #
    
    # FUNCTION:
    
    # as timeDate:
    ans = timeDate(charvec = x, zone = zone, FinCenter = FinCenter)
    
    # Return Value:
    ans
}


################################################################################
#  as.character.timeDate  Returns a 'timeDate' object as 'character' string
#  as.double.timeDate     Returns a 'timeDate' object as 'numeric' object
#  as.data.frame.timeDate Returns a 'timeDate' object as 'data.frame' object
#  as.POSIXct.timeDate    Returns a 'timeDate' object as 'POSIXct' object
#  as.POSIXlt.timeDate    Returns a 'timeDate' object as 'POSIXlt' object
#  as.Date.timeDate       Returns a 'timeDate' object as 'Date' object


as.character.timeDate =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns a 'timeDate' object as character string
    
    # Arguments:
    #   x - a 'timeDate' object
    #   ... - arguments passed to other methods.
    
    # Value:
    #   Returns 'x' as a character vector. 
    
    # Changes:
    #

    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
    # Check Class Type:
    if (!inherits(x, "timeDate")) stop("Wrong class type")
    
    # Format:
    ans = format.POSIXct(x@Data)
    attr(ans, "control") = c(FinCenter = x@FinCenter)
    
    # Reset Time Zone: 
    Sys.putenv(TZ = myTZ)
    
    # Return Value: 
    ans
}


# ------------------------------------------------------------------------------


as.double.timeDate = 
function(x, units = c("auto", "secs", "mins", "hours", "days", "weeks"), ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns a 'timeDate' object as 'numeric' vector
    
    # Arguments:
    #   x - a 'timeDate' object
    #   units - a character string denoting in which units the
    #       elements of the numeric vector are measured
    
    # Value:
    #   Returns 'x' as a numeric vector. 

    # Changes:
    #
    
    # FUNCTION:
    
    # Set time zone to GMT:
    myTZ = Sys.getenv("TZ")
    Sys.putenv(TZ = "GMT")
    
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
    Sys.putenv(TZ = myTZ)
    
    # Return Value:
    ans   
}


# ------------------------------------------------------------------------------


as.data.frame.timeDate =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns a 'timeDate' object as data frame
    
    # Arguments:
    #   x - a 'timeDate' object
    
    # Value:
    #   Returns 'x' as a data frame.
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
    # Check Class Type:
    stopifnot(inherits(x, "timeDate"))
    
    # Data Frame:
    ans = as.data.frame.POSIXlt(x@Data, ...)
    colnames(ans) = paste(x@FinCenter, ":", substitute(x), sep = "")
    attr(ans, "control") = c(FinCenter = x@FinCenter)
    
    # Reset Time Zone: 
    Sys.putenv(TZ = myTZ)
    
    # Return Value: 
    ans
}


# ------------------------------------------------------------------------------


as.POSIXct.timeDate =
function(x, tz = "")
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns a 'timeDate' object as POSIXct object
    
    # Arguments:
    #   x - a 'timeDate' object
    #   tz - a timezone specification to be used for the conversion.
    #       (Not needed when used for 'timeDate' conversions.)
    
    # Value:
    #   Returns 'x' as an object of class 'POSIXct'.
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
    # Check Class Type:
    if (!inherits(x, "timeDate")) stop("Wrong class type")
    
    # POSIXlt:
    ans = x@Data
    attr(ans, "control") = c(FinCenter = x@FinCenter)
    
    # Reset Time Zone: 
    Sys.putenv(TZ = myTZ)
    
    # Return Value: 
    ans
}


# ------------------------------------------------------------------------------


as.POSIXlt.timeDate =
function(x, tz = "")
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns a 'timeDate' object as 'POSIXlt' object
    
    # Arguments:
    #   x - a 'timeDate' object
    #   tz - a timezone specification to be used for the conversion.
    #       (Not needed when used for 'timeDate' conversions.)
    
    # Value:
    #   Returns 'x' as an object of class 'POSIXct'.
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Set Timezone to GMT:
    ans = as.POSIXlt(as.POSIXct(x = x, tz = tz))
    
    # Return Value: 
    ans
}


# ------------------------------------------------------------------------------


as.Date.timeDate =
function(x, method = c("trunc", "round", "next"), ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns a 'timeDate' object as 'Date' object
    
    # Arguments:
    #   x - a 'timeDate' object
    #   method - a character string denoting the method how to 
    #       compute the 'Date' object.
    
    # Value:
    #   Returns 'x' as an object of class 'POSIXct'.
    
    # Changes:
    #
    
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
#  julian.timeDate        Returns Julian day counts since 1970-01-01
#  atoms.timeDate         Returns date/time atoms from a 'timeDate' object
#  months.timeDate        Extract months atom from a 'timeDate' object

 
julian.timeDate = 
function(x, FinCenter = myFinCenter, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Extracts Julian time in days since 1970-01-01
    
    # Arguments:
    #   x - a 'timeDate' object
    #   units - a character string, one of the units listed, 
    #       by default "secs".
    
    # Value:
    #   Returns the number of days (possibly fractional) since
    #   the origin.
    
    # Details:
    #   The origin is "1970-01-01 00:00:00 GMT"

    # Changes:
    #
    
    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
    # Check Class Type:
    if (!inherits(x, "timeDate")) stop("Wrong class type")
    
    # Fixed Units:
    if (!exists("myUnits")) units = "secs" else units = myUnits
    
    # POSIX:
    ct = timeDate(x, zone = x@FinCenter, FinCenter = FinCenter)@Data

    # Difftime:  
    origin = as.POSIXlt("1970-01-02", tz = "GMT") - 24 * 3600
    res = difftime(ct, origin, units = units[1])
    ans = structure(res, origin = origin)
        
    # Reset Time Zone: 
    Sys.putenv(TZ = myTZ)
    
    # Return Value: 
    ans
}
    

# ------------------------------------------------------------------------------


atoms.timeDate = 
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Extracts atoms from a 'timeDate' object.
    
    # Arguments:
    #   x - a 'timeDate' object from which to extract the
    #       calendar "atoms".
    
    # Value:
    #   Returns a data.frame with the following calendar atoms:
    #   Y(ear), m(onth), d(ay), H(our), M(inutes), S(econds).
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
    # Check Class Type:
    if (!inherits(x, "timeDate")) stop("Wrong class type")
    
    # mdy:
    X = as.POSIXlt(x@Data)
    Y = X$year + 1900
    m = X$mon + 1
    d = X$mday
    H = X$hour
    M = X$min
    S = X$sec
    
    # Data Frame:
    ans = data.frame(Y = Y, m = m, d = d, H = H, M = M, S = S)
    attr(ans, "control") = c(FinCenter = x@FinCenter)
    
    # Reset Time Zone: 
    Sys.putenv(TZ = myTZ)
    
    # Return Value: 
    ans
}


# ------------------------------------------------------------------------------


months.timeDate =
function(x, abbreviate = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Extracts months atom from a timeDate object

    # Arguments:
    #   x - a 'timeDate' object from which to extract the
    #       month "atom".
    
    # Value:
    #   Returns the month from a 'timeDate' object as an integer
    #   value or vector with elements ranging between 1 and 12,
    #   numbering the months from January to December.
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
    # Check Class Type:
    if (!inherits(x, "timeDate")) stop("Wrong class type")
    
    # Month:
    ans = as.POSIXlt(x@Data)$mon+1
    attr(ans, "control") = c(FinCenter = x@FinCenter)
    
    # Reset Time Zone: 
    Sys.putenv(TZ = myTZ)
    
    # Return Value: 
    ans
}
    

################################################################################

