
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
# METHOD:                 DESCRIPTION:
#  'fGARCH'                S4: fGARCH Class representation
# METHOD:                 DESCRIPTION:
#  show                    Print method for an object of class 'fGARCH'
#  plot                    Plot method for an object of class 'fGARCH'
#  .interactiveGarchPlot    IUnternal Utility Function
#  summary                 Summary method for an object of class 'fGARCH'
################################################################################


setMethod(f = "show", signature = "fGARCH", description = 
    function(object) 
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Print method for an object of class "fGARCH"
    
    # Arguments:
    #   object - an object of class 'fGARCH'
    
    # FUNCTION:
     
    # Title:
    cat("\nTitle:\n ")
    cat(object@title, "\n")
    
    # Call:
    cat("\nCall:\n ")
    cat(paste(deparse(object@call), sep = "\n", collapse = "\n"), "\n")
    
    # Mean Equation:
    cat("\nMean and Variance Equation:\n ")
    cat(as.character(object@formula[1]), "+", 
        as.character(object@formula[2]), "\n")
        
    # Conditional Distribution:
    cat("\nConditional Distribution:\n ")
    cat(object@fit$params$cond.dist, "\n")
  
    # Coefficients:
    cat("\nCoefficient(s):\n")
    digits = max(6, getOption("digits") - 4)
    print.default(format(object@fit$par, digits = digits), print.gap = 2, 
         quote = FALSE)    
    
    # Error Analysis:
    digits = max(4, getOption("digits") - 5)
    fit = object@fit 
    signif.stars = getOption("show.signif.stars")
    cat("\nError Analysis:\n")
    printCoefmat(fit$matcoef, digits = digits, signif.stars = signif.stars) 
    
    # Log Likelihood:
    cat("\nLog Likelihood:\n ")
    LLH = object@fit$value
    N = length(object@data$x)
    cat(LLH, "   normalized: ", LLH/N, "\n")
        
    # Description:
    cat("\nDescription:\n ")
    cat(object@description, "\n")

    # Return Value:
    cat("\n")
    invisible()
})


################################################################################

