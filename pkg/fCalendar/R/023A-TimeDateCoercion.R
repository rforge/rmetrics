
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
#  as.character.timeDate  Returns a 'timeDate' object as character string
#  as.data.frame.timeDate Returns a 'timeDate' object as data frame
#  as.POSIXct.timeDate    Returns a 'timeDate' object as POSIXct object
#
#  julian.timeDate        Returns Julian day counts since 1970-01-01
#
#  atoms.timeDate         Returns date/time atoms from a 'timeDate' object
#  months.timeDate        Extract months atom from a 'timeDate' object
################################################################################


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

    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
    # Check Class Type:
    if (!inherits(x, "timeDate")) stop("Wrong class type")
    
    # Format:
    ans = format.POSIXlt(x@Data)
    attr(ans, "control") = c(FinCenter = x@FinCenter)
    
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
{# A function implemented by Diethelm Wuertz

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
    Sys.putenv(TZ = "GMT")
    
    # Check Class Type:
    if (!inherits(x, "timeDate")) stop("Wrong class type")
    
    # POSIXlt:
    ans = as.POSIXct.POSIXlt(x@Data)
    attr(ans, "control") = c(FinCenter = x@FinCenter)
    
    # Reset Time Zone: 
    Sys.putenv(TZ = myTZ)
    
    # Return Value: 
    ans
}


# ------------------------------------------------------------------------------

 
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

    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
    # Check Class Type:
    if (!inherits(x, "timeDate")) stop("Wrong class type")
    
    # Fixed Units:
    if (!exists("myUnits")) units = "secs" else units = myUnits
    
    # POSIX:
    lt = timeDate(x, zone = x@FinCenter, FinCenter = FinCenter)@Data

    # Difftime:  
    origin = as.POSIXlt("1970-01-02", tz = "GMT") - 24 * 3600
    res = difftime(as.POSIXct(lt), origin, units = units[1])
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
    
    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
    # Check Class Type:
    if (!inherits(x, "timeDate")) stop("Wrong class type")
    
    # mdy:
    X = x@Data
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
    
    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
    # Check Class Type:
    if (!inherits(x, "timeDate")) stop("Wrong class type")
    
    # Month:
    ans = x@Data$mon+1
    attr(ans, "control") = c(FinCenter = x@FinCenter)
    
    # Reset Time Zone: 
    Sys.putenv(TZ = myTZ)
    
    # Return Value: 
    ans
}
    

################################################################################

