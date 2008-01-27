
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
# FUNCTION:                     PORTFOLIO S4 EXTRACTORS FROM DATA SLOT:
#  getData                       Extracts data slot
#   getSeries                     Extracts assets series data 
#   getNumberOfAssets             Extracts number of assets from statistics
#   getNames                      Extracts assets names
#  getStatistics                 Extracts statistics slot
#   getMu                         Extracs mean  mu from statistics
#   getSigma                      Extracs covariance Sigma from statistics
#  getTailRisk                   Extracts tail risk slot
################################################################################


################################################################################
# fPFOLIODATA - S4

    # Slots:
    # data = list(
    #   series
    #   nAssets)
    # statistics = list(
    #   mu,
    #   Sigma) 
    # tailRisk = list()
    
    
# ------------------------------------------------------------------------------
    
    
getData.fPFOLIODATA <- 
    function(object)
{   
    # A function implemented by Rmetrics

    # Description:
    #   Extracts the series from fPOLIODATA
    
    # Arguments:
    #   object - an object of S4 class fPFOLIODATA
    
    # FUNCTION:
    
    # Get Series:
    ans = object@data
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------
    
    
getSeries.fPFOLIODATA <- 
    function(object)
{   
    # A function implemented by Rmetrics

    # Description:
    #   Extracts the series from fPOLIODATA
    
    # Arguments:
    #   object - an object of S4 class fPFOLIODATA
    
    # FUNCTION:
    
    # Get Series:
    ans = object@data$series
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------
    
    
getNumberOfAssets.fPFOLIODATA <- 
    function(object)
{   
    # A function implemented by Rmetrics

    # Description:
    #   Extracts the series from fPOLIODATA
    
    # Arguments:
    #   object - an object of S4 class fPFOLIODATA
    
    # FUNCTION:
    
    # Get Series:
    ans = object@data$nAssets
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getNames.fPFOLIODATA <- 
    function(object)
{   
    # A function implemented by Rmetrics

    # Description:
    #   Extracts the asset names from a 'fPORTFOLIO' object
     
    # FUNCTION:
      
    # Get Names of Assets:
    ans = names(getMu(object))
    if(is.null(ans)){
        counter = seq(1, getNumberOfAssets(object), 1)
        ans = paste("A", counter, sep = "")
    }
      
    # Return Value:
    ans  
} 



# ------------------------------------------------------------------------------


getStatistics.fPFOLIODATA <- 
    function(object)
{   
    # A function implemented by Rmetrics

    # Description:
    #   Extracts the statistics from fPOLIODATA 
    
    # Arguments:
    #   object - an object of S4 class fPFOLIODATA
    
    # FUNCTION:
    
    # Get Statistics 
    ans = object@statistics
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getMu.fPFOLIODATA <- 
    function(object)
{   
    # A function implemented by Rmetrics

    # Description:
    #   Extracts the statistics$mu from fPOLIODATA 
    
    # Arguments:
    #   object - an object of S4 class fPFOLIODATA
    
    # FUNCTION:
    
    # Get Statistics 
    ans = object@statistics$mu
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getSigma.fPFOLIODATA <- 
    function(object)
{   
    # A function implemented by Rmetrics

    # Description:
    #   Extracts the statistics$Sigma from fPOLIODATA 
    
    # Arguments:
    #   object - an object of S4 class fPFOLIODATA
    
    # FUNCTION:
    
    # Get Statistics 
    ans = object@statistics$Sigma
    
    # Return Value:
    ans  
}


# ------------------------------------------------------------------------------


getTailRisk.fPFOLIODATA <- 
    function(object)
{   
    # A function implemented by Rmetrics

    # Description:
    #   Extracts the tailRisk from fPOLIODATA 
    
    # Arguments:
    #   object - an object of S4 class fPFOLIODATA
    
    # FUNCTION:
    
    # Get Statistics 
    ans = object@tailRisk
    
    # Return Value:
    ans  
}
    

################################################################################

