
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
#  forecastsImport       Downloads monthly data from www.forecasts.org 
#  forecastsSeries       Easy to use download from www.forecasts.org   
################################################################################
       
        
test.forecastsImport = 
function()
{     
    if (FALSE) {
        
        X = forecastsImport("MPRIME")
        print(X)
        print(head(X@data)) 
        print(class(X@data))
        
        X = forecastsImport("GOLD")
        print(X)
        print(head(X@data)) 
        print(class(X@data))
        
    }
    
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
        print(tail(X))
        print(class(X))
        
        X = forecastsSeries("GOLD")
        print(head(X))
        print(tail(X)) 
        print(class(X))
        
        X = forecastsSeries(c("MPRIME", "GOLD"))
        print(head(X)) 
        print(tail(X)) 
        print(class(X))
        
    }
    
    # Return Value:
    return()
}

    
# ------------------------------------------------------------------------------


test.forecastsListing = 
function()
{     
    if (FALSE) {
        
        list = c(
            "business", "banking", "cpi", "interest", "exchange", "price", 
            "employment", "monetary", "reserves", "gdp", "government", "ppi",
            "regional", "bop")
        
        for (List in list) print(forecastsListing(List))
    
    }
 
    
    # Return Value:
    return()
}


################################################################################





forecastsImport <-  
    function(query, file = "tempfile", 
    source = "http://www.forecasts.org/data/data/", save = FALSE, try = TRUE) 
{   