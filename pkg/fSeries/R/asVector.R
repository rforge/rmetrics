
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
# METHOD:                   HANDLING OTHER TIME SERIES OBJECTS:
#  asVector                  Converts a time series into a numeric vector
#  asVector.default          Defaul Method
#  asVector.timeSeries       Converts a 'timeSeries' object into a vector
#  asVector.zoo              Converts a 'zoo' object into a vector
#  asVector.ts               Converts a 'ts' object into a vector
################################################################################


asVector <-
    function (x) 
{
    # Description:
    #   Converts a time series into a numeric vector
    
    UseMethod("asVector")
}


# ------------------------------------------------------------------------------


asVector.default <-
    function (x) 
{
    # Description:
    #   Converts a time series into a numeric vector
    
    as.vector(x, mode = "any")
}


# ------------------------------------------------------------------------------


asVector.timeSeries <- 
    function(x) 
{
    # A function implemented by Diethelm Wuertz and Yohan Chalabi
    
    # Description:
    #   Converts a timeSeries object into a numeric vector
    
    # Arguments:
    #   x - an univariate time series object of class 'timeSeries'
    
    # FUNCTION:
    
    # Check:
    stopifnot(isUnivariate(x))
    
    # Convert to Vector:
    ans = as.vector(x@Data, mode = "any")
    positions = x@positions
    attr(positions, "control") <- NULL
    names(ans) = positions
    
    # Add attributes:
    control = list(class = "timeSeries", units = x@units, 
        FinCenter = x@FinCenter)
    class(control) = "control"
    attr(ans, "control") <- control
        
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


asVector.zoo = 
    function(x) 
{
    # A function implemented by Diethelm Wuertz and Yohan Chalabi
    
    # Description:
    #   Converts a zoo object into a numeric vector
    
    # Arguments:
    #   x - an univariate time series object of class 'zoo'
    
    # FUNCTION:
    
    # Note: as.vector.zoo is not an exported object from namespace:zoo
    stopifnot(isUnivariate(x))
    
    # Convert to Vector:
    ans = as.vector(x, mode = "any")
    names(ans) = attr(x, "index")
    
    # Add attributes:
    control = list(class = "zoo", units = NULL)
    class(control) = "control"
    attr(ans, "control") <- control
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


asVector.ts <- 
    function(x) 
{
    # A function implemented by Diethelm Wuertz and Yohan Chalabi
    
    # Description:
    #   Converts a ts object into a numeric vector
    
    # Arguments:
    #   x - an univariate time series object of class 'ts'
    
    # FUNCTION:
    
    # Check:
    stopifnot(isUnivariate(x))
    
    # Convert to Vector:
    ans = as.vector(x, mode = "any")
    
    # Add Attributes:
    control = list(class = "ts", tsp = attr(x, "tsp"), units = NULL)
    class(control) = "control"
    attr(ans, "control") <- control
    
    # Return Value:
    ans
}


################################################################################

