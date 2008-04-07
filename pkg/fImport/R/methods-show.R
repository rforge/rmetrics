
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
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:             DESCRIPTION:
#  show.fWEBDATA         S4 Show Method for WEB downloaded data
################################################################################


setMethod("show", signature(object = "fWEBDATA"), definition = 
    function(object)
{   
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
       
    # Unlike print the argument for show is 'object'.
    x = object
    
    # Title:
    cat("\nTitle:\n", object@title, "\n", sep = "")
    
    # Parameter:
    cat("\nParameter:\n")
    param = cbind(object@param)
    colnames(param) = "Value:"
    print(param, quotes = FALSE) 
    
    # Description:
    cat("\nDescription:\n", object@description, sep = "")   
    cat("\n\n")
    
    # Return Value:
    invisible()
})


################################################################################

