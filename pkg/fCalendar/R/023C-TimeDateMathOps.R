
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
# S3 MEHOD:              MATHEMATICAL OPERATIONS:
#  Ops.timeDate           Group 'Ops' generic functions for 'timeDate' objects
#  +.timeDate             Performs arithmetic + operation on 'timeDate' objects
#  -.timeDate             Performs arithmetic - operation on 'timeDate' objects
#  diff.timeDate          Returns suitably lagged and iterated differences
#  difftimeDate           Returns a difference of two 'timeDate' objects
#  round.timeDate         Rounds objects of class 'timeDate'
#  trunc.timeDate         Truncates objects of class 'timeDate' 
# S3 MEHOD:              CONCATENATION, ORDERING AND SORTING:
#  c.timeDate             Concatenates 'timeDate' objects
#  rep.timeDate           Replicates a 'timeDate' object
#  sort.timeDate          Sorts a 'timeDate' object
#  sample.timeDate        Resamples a 'timeDate' object
#  unique.timeDate        Makes a 'timeDate' object unique
#  rev.timeDate           Reverts a 'timeDate' object
################################################################################


################################################################################
# S3 MEHOD:              MATHEMATICAL OPERATIONS:
#  Ops.timeDate           Group 'Ops' generic functions for 'timeDate' objects
#  +.timeDate             Performs arithmetic + operation on 'timeDate' objects
#  -.timeDate             Performs arithmetic - operation on 'timeDate' objects
#  diff.timeDate          Returns suitably lagged and iterated differences
#  difftimeDate           Returns a difference of two 'timeDate' objects
#  round.timeDate         Rounds objects of class 'timeDate'
#  trunc.timeDate         Truncates objects of class 'timeDate'


Ops.timeDate = 
function(e1, e2)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Uses group 'Ops' generic functions for 'timeDate' objects

    # Arguments:
    #   e1 - an object of class 'timeDate'
    #   e2 - an object of class 'timeDate' 
    
    # Value:
    #   Returns the 'Ops' grouped object.
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
    # Check Logical Operators:
    if (nargs() == 1)
        stop(paste("unary", .Generic, "not defined for timeDate objects"))
    boolean <- switch(.Generic, "<" = , ">" = , "==" = ,
        "!=" = , "<=" = , ">=" = TRUE, FALSE)
    if (!boolean) 
        stop(paste(.Generic, "not defined for timeDate XXX objects"))   
        
    # Convert to GMT:
    e1GMT = timeDate(e1, zone = e1@FinCenter, FinCenter = "GMT")@Data
    e2GMT = timeDate(e2, zone = e2@FinCenter, FinCenter = "GMT")@Data
    
    # Convert to Julian:
    if (inherits(e1GMT, "POSIXct")) e1 <- as.POSIXct(e1GMT)
    if (inherits(e2GMT, "POSIXct")) e2 <- as.POSIXct(e2GMT)
    
    # Return Value:
    Sys.putenv(TZ = myTZ)
    NextMethod(.Generic)
}


# ------------------------------------------------------------------------------


"+.timeDate" =
function(e1, e2)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Performs arithmetic "+" operation on 'timeDate' objects.
    
    # Arguments:
    #   e1 - an object of class 'timeDate'
    #   e2 - an object of class 'numeric'
    
    # Value:
    #   Returns a 'timeDate' object "e2" seconds later than the
    #   'timeDate' object "e1". 
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
    # Check Class Types:
    test1 = test2 = 1
    if (inherits(e1, "timeDate")) test1 = 0
    if (inherits(e2, "numeric"))  test2 = 0
    if (test1 + test2 != 0) stop("Wrong class types") 
    
    # Convert to GMT:
    e1GMT = timeDate(e1, zone = e1@FinCenter, FinCenter = "GMT")@Data
    
    # Add and Convert back to FinCenter:
    ans = timeDate(e1GMT+e2, zone = "GMT", FinCenter = e1@FinCenter)
    
    # Return Value:
    Sys.putenv(TZ = myTZ)
    ans
}


# ------------------------------------------------------------------------------


"-.timeDate" = 
function(e1, e2)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Performs arithmetic "-" operation on 'timeDate' objects
    
    # Arguments:
    #   e1 - an object of class 'timeDate'
    #   e2 - an object of class 'timeDate' or of class 'numeric'
    
    # Value:
    #   Returns a 'difftime' object if both "e1" and "e2" are
    #   'timeDate' objects, or returns a 'timeDate' object "e2"
    #   seconds earlier than "e1".
    
    # Example:
    #   charvec = c("2004-01-01 16:00:00", "2004-01-01 18:00:00")
    #   x = timeDate(charvec, zone = "GMT", FinCenter = "Europe/Zurich")
    
    # Changes:
    #
    
    # FUNCTION:
     
    ## Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
    # Check Class Types:
    test1 = test2 = 1
    if (inherits(e1, "timeDate")) test1 = 0
    if (inherits(e2, "timeDate")) test2 = 0
    if (inherits(e2, "numeric"))  test2 = 0
    if (test1 + test2 != 0) stop("Wrong class types") 
    
    # First Object:
    e1GMT = timeDate(e1, zone = e1@FinCenter, FinCenter = "GMT")@Data
    if (inherits(e2, "timeDate")) {
        e2 = timeDate(e2, zone = e2@FinCenter, FinCenter = "GMT")@Data
        # Returns difftime:
        return(e1GMT-e2) }
    if (inherits(e2, "numeric")) {
        # Returns timeDate:
        return(timeDate(e1GMT-e2, zone = "GMT", FinCenter = e1@FinCenter)) }
        
    # Return Value:
    Sys.putenv(TZ = myTZ)
    invisible()         
}


# ------------------------------------------------------------------------------


diff.timeDate =
function (x, lag = 1, differences = 1, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns suitably lagged and iterated differences
    
    # Arguments:
    #   x - a 'timeDate' object.
    #   lag - an integer indicating which lag to use, by 
    #       default 1.
    #   differences - an integer indicating the order of the 
    #       difference, by default 1.
    #   ... - further arguments to be passed to or from methods.
  
    # Value:
    #   If 'x' is a vector of length 'n' and 'differences=1', then 
    #   the computed result is equal to the successive differences
    #   'x[(1+lag):n] - x[1:(n-lag)]'.
    #   If 'difference' is larger than one this algorithm is applied
    #   recursively to 'x'. Note that the returned value is a vector 
    #   which is shorter than 'x'.

    # Changes:
    #
    
    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
    # Convert to GMT:
    GMT = timeDate(x, zone = x@FinCenter, FinCenter = "GMT") 
    ans = diff.POSIXt(as.POSIXct(GMT@Data), 
        lag = lag, differences = differences, ...) 
        
    # Reset Timezone:
    Sys.putenv(TZ = myTZ)

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


difftimeDate = 
function(time1, time2, 
units = c("auto", "secs", "mins", "hours", "days", "weeks")) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Takes a difference of two 'timeDate' objects
    
    # Arguments:
    #   time1, time2 - 'timeDate' objects.
    #   units - a character string. The units in which the results 
    #       are desired, one of the following: "auto", "secs", 
    #       "mins", "hours", "days", "weeks"
    
    # Value:
    #   'difftimeDate' takes a difference of two 'timeDate' 
    #   objects and returns an object of class 'difftime' with
    #   an attribute indicating the units. 

    # Changes:
    #
    
    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
    
    # Convert to GMT:
    time1GMT = timeDate(time1, zone = time1@FinCenter, 
        FinCenter = "GMT") 
    time2GMT = timeDate(time2, zone = time2@FinCenter, 
        FinCenter = "GMT") 

    # Return Value:
    Sys.putenv(TZ = myTZ)
    difftime(time1GMT@Data, time2GMT@Data, tz = "GMT", units = units[1]) 
}


# ------------------------------------------------------------------------------


round.timeDate =
function(x, units = c("days", "hours", "mins"), ...)     
{   # A function implemented by Diethelm Wuertz

    # Changes:
    #
    
    # FUNCTION:
    
    # Get Units:
    units = match.arg(units)
    
    # Sorting under GMT is not what we want!
    # GMT = timeDate(x, zone = x@FinCenter, FinCenter = "GMT")
    # lt = round.POSIXt(GMT@Data, units = units, ...)
    # ans = timeDate(lt, zone = "GMT", FinCenter = x@FinCenter)
    
    # Use:
    lt = round.POSIXt(x@Data, units = units)
    ans = timeDate(lt, zone = x@FinCenter, FinCenter = x@FinCenter)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


trunc.timeDate =
function(x, units = c("days", "hours", "mins"), ...) 
{   # A function implemented by Diethelm Wuertz

    # Changes:
    #
    
    # FUNCTION:
    
    # Get Units:
    units = match.arg(units)
    
    # Sorting under GMT is not what we want!
    # GMT = timeDate(x, zone = x@FinCenter, FinCenter = "GMT")
    # lt = trunc.POSIXt(GMT@Data, units = units)
    # ans = timeDate(lt, zone = "GMT", FinCenter = x@FinCenter)
    
    # Use:
    lt = trunc.POSIXt(x@Data, units = units)
    ans = timeDate(lt, zone = x@FinCenter, FinCenter = x@FinCenter)
    
    # Return Value:
    ans
}


################################################################################
# S3 MEHOD:              CONCATENATION, ORDERING AND SORTING:
#  c.timeDate             Concatenates objects of class 'timeDate'
#  rep.timeDate           Replicates objects of class 'timeDate'
#  sample.timeDate        Resamples objects of class 'timeDate'
#  sort.timeDate          Sorts, Rounds or truncates a 'timeDate' vector
#  unique.timeDate        Remove duplicates from a 'timeDate' vector
#  rev.timeDate           Reverts  a 'timeDate' vector object


c.timeDate = 
function(..., recursive = FALSE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Concatenates objects of class 'timeDate'
    
    # Arguments:
    #   ... - objects to be concatenated.
    #   recursive - a logical. If 'recursive=TRUE', the function 
    #       recursively descends through lists combining all their 
    #       elements into a vector.
    
    # Value:
    #   Returns all arguments to be coerced to a 'timeDate' object  
    #   which is the type of the returned value.
    
    # Example:
    #   timeCalendar()[0]; c(timeCalendar()[0], timeCalendar())

    # Details:
    #   This is a generic function which combines its arguments.

    # Changes:
    #
    
    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.putenv(TZ = "GMT")
        
    # List all:
    z = list(...)
    
    # Convert to GMT character vectors:
    all = NULL
    for (i in 1:length(z)) {
        # DW added if:
        if (z[[i]]@Dim > 0) {
            new = format(timeDate(z[[i]], zone = z[[i]]@FinCenter, 
                FinCenter = "GMT")@Data, "%Y-%m-%d %H:%M:%S")
            all = c(all, new)
        }
    }
    
    # Convert to Financial Center of the first element:
    ans = timeDate(all, zone = "GMT", FinCenter = z[[1]]@FinCenter)
    
    # Return Value:
    Sys.putenv(TZ = myTZ)
    ans
}

    
# ------------------------------------------------------------------------------


rep.timeDate =
function(x, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Replicates objects of class 'timeDate'
    
    # Arguments:
    #   x - a 'timeDate' object
    #   times - a non-negative integer.  A vector giving the number 
    #       of times to repeat each element if of length 'length(x)', 
    #       or to repeat the whole vector if of length 1.
    
    # Value:
    #   Returns a vector of repeated elements belonging to the same 
    #   class as 'x'.
    
    # Changes:
    #
    
    # FUNCTION:

    # Replicate: 
    GMT = timeDate(x, zone = x@FinCenter, FinCenter = "GMT")
    charvec = rep(as.character(GMT@Data), ...)
    ans = timeDate(charvec, zone = "GMT", FinCenter = x@FinCenter)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


sample.timeDate =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Changes:
    #
    
    # FUNCTION:
    
    # Sample:
    GMT = timeDate(x, zone = x@FinCenter, FinCenter = "GMT")
    charvec = sample(as.character(GMT@Data), ...)
    ans = timeDate(charvec, zone = "GMT", FinCenter = x@FinCenter)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


sort.timeDate =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Changes:
    #
    
    # FUNCTION:
    
    # Sort:
    GMT = timeDate(x, zone = x@FinCenter, FinCenter = "GMT")
    charvec = sort(as.character(GMT@Data), ...)
    ans = timeDate(charvec, zone = "GMT", FinCenter = x@FinCenter)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


unique.timeDate =
function(x, ...) 
{   # A function Implemented by Diethelm Wuertz

    # Description:
    #   Returns a timeDate object with duplicate entries removed
    
    # Arguments:
    #   x - an object of class timeDate
    #   incomparables - not used
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Remove Duplicates:    
    GMT = timeDate(x, zone = x@FinCenter, FinCenter = "GMT")
    charvec = unique(as.character(GMT@Data), ...)
    ans = timeDate(charvec, zone = "GMT", FinCenter = x@FinCenter)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


rev.timeDate =
function(x)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Reverts  a 'timeDate' vector object.
    
    # Arguments:
    #   x - a 'timeDate' object
    
    # Value:
    #   Returns 'x' as a 'timeDate' object in reversed order.
    
    # Changes:
    #
    
    # FUNCTION:
    
    # Remove Duplicates:    
    GMT = timeDate(x, zone = x@FinCenter, FinCenter = "GMT")
    charvec = rev(as.character(GMT@Data))
    ans = timeDate(charvec, zone = "GMT", FinCenter = x@FinCenter)
    
    # Return Value:
    ans
}

    
################################################################################

