
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
# FUNCTION:                     PORTFOLIO S4 EXTRACTORS:
#  getAssets                     Extracts assets series data, if available
#  getStatistics                 Extracts assets statistics, mean and covariance
#  getNumberOfAssets             Extracts number of assets from statistics
#  getSpecification              Extracts @specification Slot
#  getPortfolio                  Extracts @portfolio Slot
#  getFrontier                   Extracts the efficient frontier
#  getWeights                    Extracts weights from a fPORTFOLIO object
#  getTargetReturn               Extracts target return from a portfolio
#  getTargetRisk                 Extracts target riks from a portfolio
#  getTargetStdev                Extracts target standard deviations from a PF
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(PortfolioExtractors, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.getAssets =
function()
{
    # Settings:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    myPF = tangencyPortfolio(Data)
    
    # Test:
    myAssets = getAssets(myPF) 
    myAssets
    target = round(mean(colAvgs(myAssets)), 3)
    current = 1.112
    checkEqualsNumeric(target, current)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.getStatistics =
function()
{
    # Settings:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    myPF = tangencyPortfolio(Data)
    
    # Test:
    myStatistics = getStatistics(myPF)
    myStatistics
    target = 
        round(mean(myStatistics$mu), 3) + round(mean(myStatistics$Sigma), 3)
    current = 1.122
    checkEqualsNumeric(target, current)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.getNumberOfAssets =
function()
{ 
    # Settings:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    myPF = tangencyPortfolio(Data)
    
    # Test:
    getNumberOfAssets(myPF)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.getSpecification =
function()
{ 
    # Settings:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    myPF = tangencyPortfolio(Data)
    
    # Test:
    getSpecification(myPF)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.getPortfolio =
function()
{ 
    # Settings:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    myPF = tangencyPortfolio(Data)
    
    # Test:
    getPortfolio(myPF)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.getFrontier =
function()
{ 
    # Settings:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    myPF = tangencyPortfolio(Data)
    
    # Test:
    getFrontier(myPF)                                           # CHECK Colnames
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.getWeights =
function()
{ 
    # Settings:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    myPF = tangencyPortfolio(Data)
    
    # Test:
    getWeights(myPF) 
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.getTargetReturn =
function()
{ 
    # Settings:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    myPF = tangencyPortfolio(Data)
    
    # Test:
    getTargetReturn(myPF)  
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.getTargetRisk =
function()
{ 
    # Settings:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    myPF = tangencyPortfolio(Data)
    
    # Test:
    getTargetRisk(myPF)                                          # CHECK numeric
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.getTargetStdev =
function()
{ 
    # Settings:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    myPF = tangencyPortfolio(Data)
    
    # Test:
    getTargetStdev(myPF)                                         # CHECK numeric
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fPortfolio/test/runit2E.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}
   

################################################################################

