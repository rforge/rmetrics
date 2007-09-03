
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
#  economagicImport      Downloads market data from EconoMagic's web site
#  economagicSeries      Easy to use download from EconoMagic
################################################################################

   
test.economagic = 
function()
{  
    # economagicImport() - 
    # Daily Rates:
    X = economagicImport(query = "fedny/day-fxus2eu", 
        frequency = "daily", colname = "USDEUR")
    checkIdentical(
        target = X@data[6, 1], 
        current = "1999-01-13")
    checkIdentical(
        target = X@data[6, 2], 
        current = 1.1698)

    # economagicImport() - 
    # Monthly Rates:
    X = economagicImport(query = "fedstl/fedfunds+2", 
         frequency = "monthly", colname = "USFEDFUNDS")
    checkIdentical(
        target = X@data[6, 1], 
        current = "1955-02-01")
    checkIdentical(
        target = X@data[6, 2], 
        current = 1.29)
    
    # economagicImport() - 
    # Quarterly Rates:
    X = economagicImport(query = "fedstl/gnp", 
        frequency = "quarterly", colname = "USGNP")
    checkIdentical(
        target = X@data[6, 1], 
        current = "1948-12-01")
    checkIdentical(
        target = X@data[6, 2], 
        current = 276.6)
        
    # economagicSeries() - 
    # USDEUR Foreign Exchange Rate:
    economagicSeries("fedny/day-fxus2eu", frequency = "daily")
         
    # Return Value:
    return()
}

    
################################################################################\

