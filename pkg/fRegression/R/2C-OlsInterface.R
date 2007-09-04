
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
# FINMETRICS-LIKE:      OLS DESCRIPTION:
#  OLS                   Fit an OLS regression model - SPlus like Call
#  print.OLS             S3 Print method for an OLS regression model
#  plot.OLS              S3 Plot method for an OLS regression model
#  summary.OLS           S3 Summary method for an OLS regression model
################################################################################


OLS = 
function(formula, data, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   OLS Estimator
    
    # Notes:
    #   A Finmetrics S-Plus like implementation
    
    # FUNCTION:
    
    # Estimate:
    fit = lm(formula = formula, data = data, ...)
    fit$call = match.call()
    fit$formula = formula
    fit$data = data
    class(fit) = "OLS"
    
    # Return Value:
    fit 
}

# ------------------------------------------------------------------------------


print.OLS = 
function(x, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   S3 Print Method
    
    # FUNCTION:
    
    # Print:
    class(x) = "lm"
    print.lm(x, ...) 
    
    # Return Value:
    invisible(x)
}
    

# ------------------------------------------------------------------------------


plot.OLS = 
function(x, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   S3 Plot Method
    
    # FUNCTION:
    
    # Plot:
    class(x) = "lm"
    plot.lm(x, ...)
    
    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------

    
summary.OLS = 
function(object, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   S3 Summary Method
    
    # FUNCTION:
    
    # Summary:
    class(object) = "lm"
    summary.lm(object, ...) 
    
    # Return Value:
    invisible(object)
}


################################################################################

