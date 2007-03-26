
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
# FUNCTION:             ASSETS STATISTICS:
#  assetsStats           Computes basic statistics of a set of assets  
# FUNCTION:             MEAN-COVARIANCE ESTIMATION:
#  assetsMeanCov         Estimates mean and variance for a set of assets
#   method = "cov"        uses standard covariance estimation
#   method = "mve"        uses "mve" from [MASS]
#   method = "mcd"        uses "mcd" from [MASS]
#   method = "nnve"       uses "nnve" from [covRobust]
#   method = "shrink"     uses "shrinkage" from [corpcor]
#   method = "bagged"     uses "bagging" [corpcor]
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(AssetsMeanCovariance, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.assetsStats =
function()
{
    # Settings:
    Data = usPortfolioData()
    class(Data)
    head(Data)
    
    # Statistics:
    assetsStats(as.matrix(Data))          # CHECK - extend to timeSeries Objects
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.assetsMeanCov =
function()
{
    # Settings:
    Data = usPortfolioData()
    class(Data)
    
    # Test Standard Mean-Covariance:
    args(assetsMeanCov)
    assetsMeanCov(Data, method = "cov")
   
    
    # uses "mve" from [MASS]
    assetsMeanCov(Data, method = "mve")
    
    # uses "mcd" from [MASS]
    assetsMeanCov(Data, method = "mcd")
    
    # uses "nnve" from [covRobust]
    assetsMeanCov(Data, method = "nnve")                       # CHECK control
    
    # uses "shrinkage" from [corpcor]
    assetsMeanCov(Data, method = "shrink")
    
    # uses "bagging" [corpcor]
    assetsMeanCov(Data, method = "bagg")
    
    # checkEqualsNumeric(target, current)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fPortfolio/test/runit1E.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}
   

################################################################################