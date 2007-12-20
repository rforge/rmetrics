
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
#   1999 - 2008, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                 FINANCIAL TIME SERIES:
#  cumulated                 Computes cumulated series from financial returns
#  cumulated.default         Computes cumulated series from returns
#                             supports 'matrix', 'timeSeries', and 'zoo' objects
################################################################################


cumulated <-  
function(x, ...)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes cumulated series from financial returns
    
    # Return Value:
    UseMethod("cumulated")
}


# ------------------------------------------------------------------------------


cumulated.default <-
    function(x, method = c("continuous", "discrete", "compound", "simple"), 
    percentage = FALSE, ...)
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes cumulated series from financial returns
    #   supports 'matrix', 'timeSeries', and 'zoo' objects
    
    # Arguments:
    #   x - data object containing ordered price observations
    #   method - "continuous == "compound" and "discrete" == "simple"
    
    # Example:
    #   X = as.timeSeries(data(msft.dat))[1:10, "Close"]; X = X/X@Data[1, 1]
    #   x = returns(X, "continuous"); x;  X; cumulated(x, "continuous")  
    #   x = returns(X, "discrete"); x;  X; cumulated(x, "discrete")
    
    # Note:
    #   To make it conform with PortfolioAnalytics:
    #   "compound" == "continuous", and "simple" == "discrete"

    # FUNCTION:

    # Settings:
    method = match.arg(method)
    
    # Handle Missing Values:
    # if (na.rm) x = na.omit(x, ...) 
    
    # Transform data:
    if (percentage) x = x/100
    positions = time(x)
    
    # Calculate Returns:
    # ... colCumsums and colCumprods are generic functions with
    #     methods for 'matrix', 'timeSeries', and 'zoo' objects
    if(method == "geometric") {
        ans = colCumsums(x)      
    }
    if(method == "compound" || method == "continuous") {
        ans = exp(colCumsums(x))      
    }
    if(method == "simple" || method == "discrete") {
        ans = colCumprods(1+x) 
    }
    
    # Return Value:
    ans
}


################################################################################

