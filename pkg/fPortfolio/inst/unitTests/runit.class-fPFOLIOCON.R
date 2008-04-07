
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
#   1999 - Diethelm Wuertz, GPL
#   2007 - Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
# for code accessed (or partly included) from other sources:
#   see Rmetric's copyright and license files


################################################################################
# FUNCTION:
#  test.fPFOLIOCON
#  test.show.fPFOLIOCON.Short
#  test.show.fPFOLIOCON.LongOnly
################################################################################


## DW
## Note, there is not yet a print function in the show-methods.R


# ------------------------------------------------------------------------------


test.fPFOLIOCON <- 
    function()
{  
    # Class:
    getClass("fPFOLIOCON")
   
    # Slots:
    getSlots("fPFOLIOCON")
   
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.show.fPFOLIOCON.Short <- 
    function()
{  
    # Constraints:
    constraints = "Short"
   
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    ## DW
    ## Although this is not neede in general, we should set the
    ## box and group constraints properly to +/- Inf !
    
    # As Object:
    portfolioConstraints(data, spec = portfolioSpec(), constraints)
   
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.show.fPFOLIOCON.LongOnly <-  
    function()
{  
    # Constraints:
    constraints = "LongOnly"
   
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # As Object:
    portfolioConstraints(data, spec = portfolioSpec(), constraints)
   
    # Return Value:
    return()
}


################################################################################

