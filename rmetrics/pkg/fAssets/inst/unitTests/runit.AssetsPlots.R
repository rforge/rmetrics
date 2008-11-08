
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
# FUNCTION:                 TIME SERIES ASSETS PLOTS:
#  assetsSeriesPlot          Displays time series of individual assets
#  assetsHistPlot            Displays histograms of individual assets
#  assetsDensityPlot         Displays density plots of individual assets 
#  assetsQQNormPlot          Displays normal qq-plots of individual assets
# FUNCTION:                 DENSITY BOX PLOTS:
#  assetsBoxPlot             Producess standard box plots
#  assetsBoxPercentilePlot   Producess side-by-side box-percentile plots
# FUNCTION:                 BIVARIATE ASSETS PLOTS:                           
#  assetsPairsPlot           Displays pairs of scatterplots of assets         
#  assetsCorgramPlot         Displays correlations between assets             
#  assetsCorTestPlot         Displays and tests pairwise correlations         
# FUNCTION:                 BIVARIATE CORRELATION PLOTS:                      
#  assetsCorEigenPlot        Displays ratio of the largest two eigenvalues                    
#  *assetsTreePlot            Displays minimum spanning tree of assets         
#  assetsDendogramPlot       Displays hierarchical clustering dendogram       
#  .assetsStarPlot           Draws segment diagrams of a multivariate data set
################################################################################


# *moved to Rmetrics addon Package


test.assetsSeriesPlot =
function()
{ 
    LPP = as.timeSeries(data(LPP2005REC))
    par(mfrow = c(3, 3))
    par(ask = FALSE)
    
    assetsSeriesPlot(LPP)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.assetsHistPlot =
function()
{ 
    LPP = as.timeSeries(data(LPP2005REC))
    par(mfrow = c(3, 3))
    par(ask = FALSE)
    
    assetsHistPlot(LPP)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.assetsQQNormPlot =
function()
{ 
    LPP = as.timeSeries(data(LPP2005REC))
    par(mfrow = c(3, 3))
    par(ask = FALSE)
    
    assetsQQNormPlot(LPP)          
    
    # Return Value:
    return()
}


################################################################################


test.assetsBoxPlot =
function()
{ 
    LPP = as.timeSeries(data(LPP2005REC))
    par(mfrow = c(3, 3))
    par(ask = FALSE)
    
    # Plot:
    assetsBoxPlot(LPP)   
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.assetsQQNormPlot =
function()
{ 
    LPP = as.timeSeries(data(LPP2005REC))
    par(mfrow = c(3, 3))
    par(ask = FALSE)
    
  
    # Plot:
    assetsBoxPercentilePlot(LPP)
    
    # Return Value:
    return()
}


################################################################################


test.assetsPairsPlot =
function()
{ 
    LPP = as.timeSeries(data(LPP2005REC))[, 1:6]
    par(mfrow = c(1, 1))
    par(ask = FALSE)
    
    # Plot:
    assetsPairsPlot(LPP)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.assetsCorTestPlot =
function()
{ 
    LPP = as.timeSeries(data(LPP2005REC))[, 1:6]
    par(mfrow = c(1, 1))
    par(ask = FALSE)
    
    # Plot:
    assetsCorTestPlot(LPP)
    
    # Return Value:
    return()
}


################################################################################


test.assetsCorgramPlot =
function()
{ 
    LPP = as.timeSeries(data(LPP2005REC))[, 1:6]
    par(mfrow = c(1, 1))
    par(ask = FALSE)
    
    # Plot:
    assetsCorgramPlot(LPP) 
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.assetsCorEigenPlot =
function()
{ 
    LPP = as.timeSeries(data(LPP2005REC))[, 1:6]
    par(mfrow = c(1, 1))
    par(ask = FALSE)
    
    # Plot:
    assetsCorEigenPlot(LPP) 
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.assetsTreePlot =
function()
{ 
    LPP = as.timeSeries(data(LPP2005REC))[, 1:6]
    par(mfrow = c(1, 1))
    par(ask = FALSE)
    
    # Plot:
    assetsTreePlot(LPP)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


assetsDendogramPlot =
function()
{ 
    LPP = as.timeSeries(data(LPP2005REC))[, 1:6]
    par(mfrow = c(1, 1))
    par(ask = FALSE)

    # Plot:   
    assetsDendrogramPlot(LPP)                      
    
    # Return Value:
    return()
}
   

################################################################################

