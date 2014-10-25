
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


################################################################################
# METHOD:                 EXTRACTORS:
#  coef.fGARCH             Extracts 'fGarch' Model Coefficients
################################################################################


setMethod(f = "coef", signature(object = "fGARCH"), definition = 
    function(object) 
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:  
    #   Extracts 'fGarch' Model Coefficients
    
    # Arguments:
    #   object - an object of class fGarch as returned by the function
    #       garchFit
    
    # FUNCTION:
    
    # Numeric vector of fitted values:
    ans = slot(object, "fit")$coef
    
    # Return Value:
    ans
})


# ------------------------------------------------------------------------------


setMethod(f = "coef", signature(object = "fGARCHSPEC"), definition = 
    function(object) 
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:  
    #   Extracts 'fGarch' Model Coefficients
    
    # Arguments:
    #   object - an object of class fGarch as returned by the function
    #       garchFit
    
    # FUNCTION:
    
    # Numeric vector of fitted values:
    ans = unlist(slot(object, "model"))
    attr(ans, "distribution") <- slot(object, "distribution")
    
    # Return Value:
    ans
})


################################################################################

