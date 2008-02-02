
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
# MEHODS:                   CONCATENATION, ORDERING AND SORTING:
#  c.timeDate                Concatenates 'timeDate' objects
#  rep.timeDate              Replicates a 'timeDate' object
#  sort.timeDate             Sorts a 'timeDate' object
#  sample.timeDate           Resamples a 'timeDate' object
#  unique.timeDate           Makes a 'timeDate' object unique
#  rev.timeDate              Reverts a 'timeDate' object
################################################################################


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

    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.setenv(TZ = "GMT")
        
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
    Sys.setenv(TZ = myTZ)
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

    # FUNCTION:
    
    # Remove Duplicates:    
    GMT = timeDate(x, zone = x@FinCenter, FinCenter = "GMT")
    charvec = rev(as.character(GMT@Data))
    ans = timeDate(charvec, zone = "GMT", FinCenter = x@FinCenter)
    
    # Return Value:
    ans
}

    
################################################################################

