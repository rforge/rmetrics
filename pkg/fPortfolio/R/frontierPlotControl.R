
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
# FUNCTION:                     PRINT AND PLOT METHODS:            
#  frontierPlotControl
################################################################################


frontierPlotControl <- 
    function( 
    
    # Colors:
    sharpeRatio.col   = "blue",
    minvariance.col   = "red",
    tangency.col      = "steelblue",
    cml.col           = "green",
    equalWeights.col  = "blue",
    singleAsset.col   = NULL,
    twoAssets.col     = "grey",
    monteCarlo.col    = "black",
 
    # Point Sizes:
    minvariance.cex   = 1.25,
    tangency.cex      = 1.25,
    cml.cex           = 1.25,
    equalWeights.cex  = 1.25,
    singleAsset.cex   = 1.25,
    twoAssets.cex     = 0.01,
    monteCarlo.cex    = 0.01,
    sharpeRatio.cex   = 0.1,
    
    # Limits:
    xlim              = NULL,
    ylim              = NULL,
    
    # MC Steps:
    mcSteps           = 5000,
    
    # Pie Settings:
    pieR              = NULL, 
    piePos            = NULL, 
    pieOffset         = NULL
    )   
{
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Return Value:
    list(
    
        # Colors:
        sharpeRatio.col  = sharpeRatio.col,
        minvariance.col  = minvariance.col,
        tangency.col     = tangency.col,
        cml.col          = cml.col,
        equalWeights.col = equalWeights.col,
        singleAsset.col  = singleAsset.col,
        twoAssets.col    = twoAssets.col,
        monteCarlo.col   = monteCarlo.col,
     
        # Point Sizes:
        minvariance.cex  = minvariance.cex,
        tangency.cex     = tangency.cex,
        cml.cex          = cml.cex,
        equalWeights.cex = equalWeights.cex,
        singleAsset.cex  = singleAsset.cex ,
        twoAssets.cex    = twoAssets.cex,
        monteCarlo.cex   = monteCarlo.cex,
        sharpeRatio.cex  = sharpeRatio.cex,
        
        # Limits:
        xlim             = xlim,
        ylim             = ylim,
        
        # MC Steps:
        mcSteps          = 5000,
        
        # Pie Settings:
        pieR             = pieR, 
        piePos           = piePos, 
        pieOffset        = pieOffset
        
        )
        
}


################################################################################

