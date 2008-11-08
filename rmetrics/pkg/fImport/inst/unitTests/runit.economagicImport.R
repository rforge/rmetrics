
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
#  economagicImport      Downloads market data from www.economagic.com
#  economagicSeries      Easy to use download from www.economagic.com
################################################################################

      
test.economagicSeries <- 
    function()
{  
    if (FALSE) {
        
        # Daily Rates:
        X = economagicSeries(symbols = "fedny/day-fxus2eu")
        print(head(X))
        
        # Monthly Rates:
        X = economagicSeries(symbols = "fedstl/fedfunds+2")
        print(head(X))
        
        # Quarterly Rates:
        X = economagicSeries(symbols = "fedstl/gnp")
        print(head(X))
        
        # USDEUR Daily Foreign Exchange Rate:
        X = economagicSeries(symbols = "fedny/day-fxus2eu")
        print(head(X))
    }
         
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------
    
       
test.economagicImport <- 
    function()
{  
    if (FALSE) {
        
        # Daily Rates:
        X = economagicImport(query = "fedny/day-fxus2eu")
        print(X)
    
        # Monthly Rates:
        X = economagicImport(query = "fedstl/fedfunds+2")
        print(X)
        
        # Quarterly Rates:
        X = economagicImport(query = "fedstl/gnp")
        print(X)
            
        # USDEUR Daily Foreign Exchange Rate:
        X = economagicImport("fedny/day-fxus2eu")
        print(X)
        
    }
         
    # Return Value:
    return()
}

    
################################################################################\

