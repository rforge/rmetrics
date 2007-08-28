
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
# FUNCTION:                 DESCRIPTION:
#  garchOxFit                Fits parameters of a garch model           
#  print.garchOx             S3 Print Method
#  summary.garchOx           S3 Summary Method
#  plot.garchOx              S3 Plot Method
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(GarchOxInterface, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.garchOxFit = 
function()
{  
    # Tested for MS Windows only ...
    
    # garchOxFit(formula, data, 
    #   cond.dist = c("gaussian", "t", "ged", "skewed-t"), 
    #   include.mean = TRUE, trace = TRUE, control = list(), 
    #   title = NULL, description = NULL)
    
    # Load Data, convert to numeric Vector:
    data(dem2gbp)  
    x = dem2gbp[, 1]
    
    # Fit:
    OXPATH <<- "C:\\Ox\\Ox3"
    # fit = garchOxFit(~garch(1,1), data = x, trace = FALSE) # CHECK !!!

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.print = 
function()
{  
    #
    
    NA  

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.summary = 
function()
{  
    #
    
    NA  

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.plot = 
function()
{  
    #
    
    NA  

    # Return Value:
    return()    
}


################################################################################
    
