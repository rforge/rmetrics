
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR Description. See the 
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA 02111-1307 USA

# Copyrights (C)
# for this R-port: 
#   1999 - 2007, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                     PORTFOLIO S4 EXTRACTORS:
#  getAssets                     Extracts assets series data, if available
#  getStatistics                 Extracts assets statistics, mean and covariance
#  getNumberOfAssets             Extracts number of assets from statistics
#  getSpecification              Extracts @specification Slot
#  getPortfolio                  Extracts @portfolio Slot
#  getFrontier                   Extracts the efficient frontier
#  getWeights                    Extracts weights from a fPORTFOLIO object
#  getTargetReturn               Extracts target return from a portfolio
#  getTargetRisk                 Extracts target riks from a portfolio
#  getTargetStdev                Extracts target standard deviations from a PF
################################################################################


getAssets =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the efficient frontier from a 'fPORTFOLO' object
    
    # FUNCTION:
    
    # Get Series of Assets
    ans = object@data$series
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getStatistics =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the efficient frontier from a 'fPORTFOLO' object
    
    # FUNCTION:
    
    # Get Series of Assets
    ans = object@data$statistics
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getNumberOfAssets =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the efficient frontier from a 'fPORTFOLO' object
    
    # FUNCTION:
    
    # Get Series of Assets
    ans = length(object@data$statistics$mu)
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getSpecification =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the statistics from a 'fPORTFOLIO' object
    
    # FUNCTION:
    
    # Get Portfolio:
    ans = object@specification
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getPortfolio =
function(object, doplot = FALSE, ...)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the statistics from a 'fPORTFOLIO' object
    
    # FUNCTION:
    
    # Get Portfolio:
    ans = object@portfolio
    
    # Plot:
    if (doplot) plot(object, which = c(1, 3, 4, 5, 6), ...)
  
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getFrontier =
function(object, frontier = c("both", "lower", "upper"), doplot = FALSE, ...)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the efficient frontier from a 'fPORTFOLO' object
    
    # FUNCTION:
    
    # Settings:
    frontier = match.arg(frontier)
    
    # Get Efficient Frontier:
    
    ans = cbind(
        Risk = object@portfolio$targetRisk, 
        Return = object@portfolio$targetReturn)

    # Extract upper part of frontier
    if(frontier == "upper"){
        index = 1:length(ans[, 1])
        test = c(-1, diff(ans[, 1]))
        index = index[test > 0]
        ans = ans[index, ]
    } else if(frontier == "lower"){
        index = 1:length(ans[, 1])
        test = c(-1, diff(ans[, 1]))
        index = index[test < 0]
        ans = ans[index, ]
    }
  
    # Plot:
    if(doplot) plot(ans, ...)
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getWeights =
function(object, doplot = FALSE, ...)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the weights from a 'fPORTFOLIO' object
    
    # FUNCTION:
    
    # Get Weights:
    ans = object@portfolio$weights
    
    # Plot:
    if (doplot) .weightsPlot(object, ...)
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getTargetReturn =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the target return from a 'fPORTFOLIO' object
    
    # Example:
    #   targetReturn()
    
    # FUNCTION:
    
    # Target MV Return:
    ans = object@portfolio$targetReturn
    if (length(ans) == 1) names(ans) = "targetReturn"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


getTargetRisk =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the target risk from a 'fPORTFOLIO' object
   
    # FUNCTION:
    
    # Target MV Risk:
    ans = object@portfolio$targetRisk
    if (length(ans) == 1) names(ans) = "targetRisk"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


getTargetStdev =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   Extracts the target standard deviation from a 'fPORTFOLIO' object
   
    # FUNCTION:
    
    # Target Standard Deviation:
    ans = object@portfolio$targetStdev
    if (length(ans) == 1) names(ans) = "targetStdev"
    
    # Return Value:
    ans
}


################################################################################

