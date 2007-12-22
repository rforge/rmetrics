
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
#  asMatrix                  Converts a time series into a numeric matrix
#  asMatrix.default          Defaul Method
#  asMatrix.timeSeries       Converts a 'timeSeries' object into a matrix
#  asMatrix.zoo              Converts a 'zoo' object into a matrix
#  asMatrix.ts               Converts a 'ts' object into a matrix
################################################################################


asMatrix <-
    function (x) 
{
    # Description:
    #   Converts a time series into a numeric matrix
    
    UseMethod("asMatrix")
}


# ------------------------------------------------------------------------------


asMatrix.default <-
    function (x) 
{
    # Description:
    #   Converts a time series into a numeric matrix
    
    as.vector(x, mode = "any")
}


# ------------------------------------------------------------------------------


asMatrix.timeSeries <- 
    function(x) 
{
    # A function implemented by Diethelm Wuertz and Yohan Chalabi
    
    # Description:
    #   Converts a timeSeries object into a numeric matrix
    
    # Arguments:
    #   x - an univariate time series object of class 'timeSeries'
    
    # FUNCTION:
    
    # Check:
    stopifnot(isMultivariate(x))
    
    # Convert to Vector:
    ans = x@Data
    positions = x@positions
    attr(positions, "control")<-NULL
    
    # Add attributes:
    attr(ans, "control")<-
        list(class = "timeSeries", positions = positions,
            units = x@units, FinCenter = x@FinCenter)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


asMatrix.zoo = 
    function(x) 
{
    # A function implemented by Diethelm Wuertz and Yohan Chalabi
    
    # Description:
    #   Converts a zoo object into a numeric matrix
    
    # Arguments:
    #   x - an univariate time series object of class 'zoo'
    
    # FUNCTION:
    
    # Check:
    stopifnot(isMultivariate(x))
    
    # Convert to Matrix:
    ans = as.matrix(x)
    colnames(ans) = colnames(x)
    
    # Add Attributes:
    attr(ans, "control")<-list(class = "zoo", index = attr(x, "index"),
        units = colnames(x))
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


asMatrix.ts <- 
    function(x) 
{
    # A function implemented by Diethelm Wuertz and Yohan Chalabi
    
    # Description:
    #   Converts a ts object into a numeric matrix
    
    # Arguments:
    #   x - an univariate time series object of class 'ts'
    
    # FUNCTION:
    
    # Check:
    stopifnot(isMultivariate(x))
    
    # Convert to Matrix:
    ans = as.matrix(x)
    colnames(ans) = colnames(x)
    
    # Add Attributes:
    attr(ans, "control")<-list(class = "ts", tsp = attr(x, "tsp"),
        units = colnames(x))
    
    # Return Value:
    ans
}


################################################################################

