
# Rmetrics is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# Rmetrics is distributed in the hope that it will be useful,
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
# FUNCTION:             DESCRIPTION:
#  forecastsCategories   Lists categries of symbols www.forecasts.org
#  forecastsListing      Lists symbols from www.forecasts.org
#  forecastsImport       Downloads monthly data from www.forecasts.org 
#  forecastsSeries       Easy to use download from www.forecasts.org   
################################################################################
       
        
test.forecastsCategories <- 
    function()
{     
    NA
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.forecastsListing = 
function()
{     
    NA
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.forecastsSeries = 
function()
{     
    if (FALSE) {
        
        X = forecastsSeries("MPRIME")
        print(head(X)) 
        
        X = forecastsSeries("GOLD")
        print(head(X))
        
        X = forecastsSeries(c("MPRIME", "GOLD"))
        print(head(X)) 
        
    }
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.forecastsImport = 
function()
{     
    if (FALSE) {
        
        X = forecastsImport("MPRIME")
        print(X)
        
        X = forecastsImport("GOLD")
        print(X)
        
    }
    
    # Return Value:
    return()
}


################################################################################

  