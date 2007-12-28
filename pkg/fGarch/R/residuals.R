
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
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
# METHOD:                 EXTRACTORS:
#  residuals.fGARCH        S3 residuals method for an object of class 'fGARCH'
#  .residuals.fGARCH       Internal Function to standardize residuals
################################################################################


residuals.fGARCH = 
function(object, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   S3 Residuals method for an object of class fGARCH

    # Arguments:
    #   object - an object of class fGarch as returned by the  
    #       function garchFit
    #   ... - optional argument to be passed, this may be standardize=FALSE
    #       to return the -non-standardized values of the residuals. 
    #       By default standardized residuals will be returned.
    
    # FUNCTION:
    
    # Get numeric vector of residuals, optionally standardized
    residuals = .residuals.fGARCH(object = object, ...) 
    
    # Get original time series class:
    data = object@data$data
    dataClass = class(data)[1]
    
    if (dataClass == "timeSeries") {
        ans = data
        data.mat = matrix(residuals)
        rownames(data.mat) = rownames(data)
        colnames(data.mat) = object@data$unit
        ans@Data = data.mat
    } else if (dataClass == "zoo") {
        ans = residuals
        attr(ans, "index") = attr(data, "index")
        class(ans) = "zoo"
    else if (dataClass == "ts" | dataClass == "mts") {
        ans = residuals
        attr(ans, "tsp") = attr(data, "tsp")
        class(ans) = "ts"
    } else {
        ans = data
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.residuals.fGARCH = 
function(object, standardize = FALSE) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   S3 Residuals method for an object of class fGARCH
    
    # Arguments:
    #   object - an object of class fGarch as returned by the 
    #       function garchFit
    #   standardize - a logial flag, should the residuals be
    #       standardized    

    # FUNCTION:
    
    # Residuals:
    if (standardize) {
        ans = object@residuals/object@sigma.t
    } else {
        ans = object@residuals
    }
    
    # Return Value:
    ans
    
}


################################################################################

