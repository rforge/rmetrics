
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
# FUNCTION:                     PORTFOLIO SPECIFICATION CLASS:
#  'fPFOLIOSPEC'                 S4 Portfolio Specification Class
#  portfolioSpec                 Specifies a portfolio
#  show.fPFOLIOSPEC              Print method for 'fPFOLIOSPEC' objects
# FUNCTION:                     MODEL SLOT:
#  setType<-                     Sets type of portfolio Optimization
#  setEstimator<-                Sets name of mean-covariance estimator
#  setParams<-                   Sets optional model parameters
# FUNCTION:                     PORTFOLIO SLOT:
#  setWeights<-                  Sets weights vector
#  setTargetReturn<-             Sets target return value
#  setTargetAlpha<-              Sets CVaR target alpha value
#  setRiskFreeRate<-             Sets risk-free rate value
#  setNFrontierPoints<-          Sets number of frontier points
# FUNCTION:                     SOLVER SLOT:
#  setSolver<-                   Sets name of desired solver
#  setTrace<-                    Sets solver's trace flag
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(PortfolioSpec, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.portfolioSpec =
function()
{ 
    # Arguments:
    # portfolioSpec(model = list(type = "MV", estimator = c("mean", "cov"), 
    #   params = list()), portfolio = list(weights = NULL, targetReturn = NULL, 
    #   riskFreeRate = 0, nFrontierPoints = 50, returnRange = NULL, 
    #   riskRange = NULL), solver = list(type = c("RQuadprog", "RDonlp2"), 
    #   trace = FALSE), title = NULL, description = NULL) 

    # Default Specs:
    Spec = portfolioSpec()
    print(Spec)
    unclass(Spec)
        
    # Return Value:
    return()
}


################################################################################


test.setType =
function()
{ 
    # Modify Model Type:
    Spec = portfolioSpec()
    setType(Spec) = "CVaR"
    Spec

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.setEstimator =
function()
{ 
    # Modify Model Estimator:
    Spec = portfolioSpec()
    setEstimator(Spec) = c("mean", "shrink")
    Spec
    
    # Return Value:
    return()
}


################################################################################


test.setWeights =
function()
{ 
    # Modify portfolio weights:
    Spec = portfolioSpec()
    setWeights(Spec) = rep(1/8, 8)
    Spec
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.setTargetReturn =
function()
{ 
    # Modify portfolio target return:
    Spec = portfolioSpec()
    setTargetReturn(Spec) = 0.20
    Spec
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.setRiskFreeRate =
function()
{ 
    # Modify portfolio target return:
    Spec = portfolioSpec()
    setRiskFreeRate(Spec) = 0.03
    Spec
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.setNFrontierPoints =
function()
{ 
    # Modify portfolio number of frontier points:
    Spec = portfolioSpec()
    setNFrontierPoints(Spec) = 10
    Spec
    
    # Return Value:
    return()
}


################################################################################


test.setSolver =
function()
{ 
    NA
    
    # Return Value:
    return()
}


################################################################################



test.portfolioStatistics =
function()
{ 
    # Arguments:
    # portfolioStatistics = (data, spec = portfolioSpec())
    
    # Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    Data
    
    # Specification:
    Spec = portfolioSpec()
    Spec
    
    # Portfolio Statistics:
    portfolioStatistics(Data, Spec)
    
    # mean-mcd Portfolio Statistics:
    setEstimator(Spec) <- c("mean", "mcd")
    portfolioStatistics(Data, Spec)
    
    # mean-shrink Portfolio Statistics:
    setEstimator(Spec) <- c("mean", "shrink")
    portfolioStatistics(Data, Spec)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fPortfolio/test/runit2C.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}
   

################################################################################

