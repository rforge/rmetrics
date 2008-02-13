
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
# FUNCTION:                     PORTFOLIO DATA CLASS:
#  'fPFOLIODATA'                 S4 Portfolio Data Class
#  show.fPFOLIODATA              Print method for 'fPFOLIODATA' objects
################################################################################


setClass("fPFOLIODATA", 
    representation(
        data = "list",
        statistics = "list",
        tailRisk = "list")  
)


# ------------------------------------------------------------------------------


show.fPFOLIODATA <- 
    function(object)
{   
    # A function implemented by Rmetrics

    # Description:
    #   S4 Print Method for an object of class "fPFOLIODATA"
    
    # Arguments:
    #   object - an object of class "fPFOLIOSPEC"
    
    # FUNCTION:
    
    # Series:
    cat("\nSeries Data:\n\n")
    print(object@data$series)
    
    # Statistics:
    cat("\nStatistics:\n\n")
    print(object@statistics)
    
    # Tailrisk:
    # NYI
        
    # Return Value: 
    invisible(object)
}


# ------------------------------------------------------------------------------


setMethod("show", "fPFOLIODATA", show.fPFOLIODATA)


################################################################################

