
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
#  weekdays.timeDate
#  months.timeDate           Extracts months atom from a 'timeDate' object
#  quarters.timeDate
#  julian.timeDate           Returns Julian day counts since 1970-01-01
# FUNCTION:
#  atoms.timeDate            Returns date/time atoms from a 'timeDate' object
################################################################################


## DW:
## R(base) has S3 weekdays(), months(), quarters(), and julian()
## for POSIXt and Date objects, see help weekdays(base)


# ------------------------------------------------------------------------------
 

weekdays.timeDate <- 
function(x, abbreviate = FALSE) {
    ans = weekdays(as.POSIXlt(x), abbreviate)
    attr(ans, "control") <- x@FinCenter
    ans
}


# ------------------------------------------------------------------------------


months.timeDate <- 
function(x, abbreviate = FALSE) 
{
    ans = months(as.POSIXlt(x), abbreviate)
    attr(ans, "control") <- x@FinCenter 
    ans
}


# ------------------------------------------------------------------------------


quarters.timeDate <- 
function(x, ...) 
{
    ans = quarters(as.POSIXlt(x), ...)
    attr(ans, "control") <- x@FinCenter
    ans
}


# ------------------------------------------------------------------------------


julian.timeDate <- 
function(x, 
    origin = timeDate("1970-01-01", zone = "GMT", FinCenter = "GMT"), ...) 
{
    # origin ignored always using 1970-01-01 GMT
    ans = julian(as.POSIXlt(x), origin = as.POSIXct(origin), ...)
    attr(ans, "control") <- x@FinCenter
    ans
}


# ------------------------------------------------------------------------------
# move to unit tests ...


.test.weekdays <- 
function() 
{
    charvec = "2008-03-31 22:00:00"
    print(charvec)
    NYC = timeDate(charvec, zone = "NewYork", FinCenter = "NewYork")
    ZRH = timeDate(charvec, zone = "NewYork", FinCenter = "Zurich")
    print(NYC)
    print(weekdays(NYC, TRUE))
    print(months(NYC, TRUE))
    print(quarters(NYC))
    print(julian(NYC))
    print(ZRH)
    print(weekdays(ZRH, TRUE))
    print(months(ZRH, TRUE))
    print(quarters(ZRH))
    print(julian(ZRH))
    print(julian(ZRH)-julian(NYC))
    # Example: .test.weekdays()
    invisible()
}



################################################################################
# OLD VERSION


.months.timeDate <- 
    function(x, abbreviate = NULL)
{   
    # A function implemented by Diethelm Wuertz

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
    Sys.setenv(TZ = "GMT")
    
    # Check Class Type:
    if (!inherits(x, "timeDate")) stop("Wrong class type")
    
    # Month:
    ans = as.POSIXlt(x@Data)$mon+1
    attr(ans, "control") = c(FinCenter = x@FinCenter)
    
    # Reset Time Zone: 
    Sys.setenv(TZ = myTZ)
    
    # Return Value: 
    ans
}
    

# ------------------------------------------------------------------------------
# OLD VERSION:


.julian.timeDate <- 
    function(x, origin = timeDate("1970-01-01"), 
    units = c("auto", "secs", "mins", "hours", "days", "weeks"), 
    zone = NULL, FinCenter = NULL, ...)
{   
    # A function implemented by Diethelm Wuertz

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

    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.setenv(TZ = "GMT")
    
    # Check Class Type:
    stopifnot(is(x, "timeDate"))
    units = match.arg(units)
    
    # POSIX:
    if (is.null(zone)) zone = x@FinCenter
    if (is.null(FinCenter)) FinCenter = x@FinCenter
    ct = timeDate(x, zone = zone, FinCenter = FinCenter) 

    # Difftime:  
    if (is.null(origin))
        origin = timeDate("1970-01-01", zone = "GMT", FinCenter = "GMT")
    res = difftimeDate(ct, origin, units = units)
        
    # Reset Time Zone: 
    Sys.setenv(TZ = myTZ)
    
    # Return Value: 
    structure(res, origin = origin)
}
    

################################################################################
# Do we need this function ?


atoms.timeDate <- 
    function(x, ...)
{   
    # A function implemented by Diethelm Wuertz

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
    Sys.setenv(TZ = "GMT")
    
    # Check Class Type:
    if (!inherits(x, "timeDate")) stop("Wrong class type")
    
    # mdy:
    X = as.POSIXlt(x)
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
    Sys.setenv(TZ = myTZ)
    
    # Return Value: 
    ans
}


################################################################################

