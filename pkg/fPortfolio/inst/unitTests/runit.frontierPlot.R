
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
#  test.frontierPlot
################################################################################


test.frontierPlot.MV.LongOnly <-  
    function()
{  
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    
    # Portfolio:
    portfolio = portfolioFrontier(data)
    portfolio
    
    # Plot:
    par(mfrow = c(2, 2))
    frontierPlot(portfolio, frontier = "both")
    frontierPlot(portfolio, frontier = "lower")
    frontierPlot(portfolio, frontier = "upper")
    frontierPlot(portfolio)
    frontierPlot(portfolio, col = c("darkgreen", "red"), add = TRUE)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


#   minvariancePoints             Adds minimum variance point
#   cmlPoints                     Adds market portfolio 
#   cmlLines                      Adds capital market Line
#   tangencyPoints                Adds tangency portfolio point 
#   tangencyLines                 Adds tangency line
#   equalWeightsPoints            Adds point of equal weights portfolio
#   singleAssetPoints             Adds points of single asset portfolios
#   twoAssetsLines                Adds EF for all combinations of two assets
#   sharpeRatioLines              Adds Sharpe ratio line
#   monteCarloPoints              Adds randomly produced feasible portfolios


frontierPlot(portfolio, pch = 19)
minvariancePoints(portfolio)

################################################################################
