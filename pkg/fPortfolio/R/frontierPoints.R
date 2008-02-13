
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
#   1999 - Diethelm Wuertz, GPL
#   2007 - Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
# for code accessed (or partly included) from other sources:
#   see Rmetric's copyright and license files


################################################################################
# FUNCTION:                     DESCRIPTION:
#  frontierPoints                Extracts frontier line
################################################################################


frontierPoints <-
    function(object, frontier = c("both", "lower", "upper"), 
        return = "mean", risk = "cov")
{   
    # A function implemented by Rmetrics

    # Description:
    #   Extracts the efficient frontier from a 'fPORTFOLO' object
    
    # Arguments:
    #   object - an object of S4 class fPORTFOLIO as returned by the
    #       functions *Portfolio().
    
    # FUNCTION:
    
    # Settings:
    frontier = match.arg(frontier)
    
    # Get Efficient Frontier:
    Type = getType(object)
    targetRisk = getTargetRisk(object)[, risk] 
    targetReturn = getTargetReturn(object)[, return]
   
    # Whole Frontier
    ans = cbind(Risk = targetRisk, Return = targetReturn)

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
        if (length(index) == 1) {
            ans = matrix(ans[index, ], ncol = 2)
        } else {
            ans = ans[index, ]
        }         
    }
    
    # Add colnames:
    colnames(ans) = c("targetRisk", "targetReturn")
    
    # Return Value:
    ans  
}


################################################################################

