
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
# FUNCTION:                     Classical and Robust Estimators
#  portfolioData                 Creates portfolio data list
#  portfolioStatistics           Estimates mu and Sigma statistics
################################################################################

################################################################################
# FUNCTION:                     PORTFOLIO S4 EXTRACTORS FROM DATA SLOT:
#  getData
#   getSeries                    Extracts assets series data 
#   getNumberOfAssets            Extracts number of assets from statistics
#  getStatistics                 Extracts assets statistics, mean and covariance
#   getMu
#   getSigma
#  getTailrisk                   Extracts tail risk
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(PortfolioData, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.portfolioData =
function()
{
    # Load Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
   
    # Set Default Specifications:
    spec = portfolioSpec()
    spec
    
    # PortfolioData:
    portfolioData(data, spec)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.portfolioStatistics =
function()
{
    # Load Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
   
    # Set Default Specifications:
    spec = portfolioSpec()
    spec
    
    # PortfolioStatistics:
    portfolioStatistics(data, spec)
         
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.Extractors =
function()
{
    # Load Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    if (!inherits(data, "fPFOLIODATA")) data = portfolioData(data, spec)
     
    getData(data)
    getSeries(data)
    getNumberOfAssets(data)
     
    getStatistics(data) 
    getMu(data)
    getSigma(data)
    
    getTailrisk(data)
         
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fPortfolio/test/runit2B.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}
   

################################################################################

