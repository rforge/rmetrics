
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
#  test.efficientPortfolio.MV.Short
#  test.efficientPortfolio.MV.LongOnly
#  test.efficientPortfolio.MV.LongOnly.Rdonlp2
#  test.efficientPortfolio.BoxConstraints.RDonlp2
#  test.efficientPortfolio.MV.LongOnly.twoAssets
################################################################################


test.efficientPortfolio.MV.Short <- 
    function()
{  
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    setTargetReturn(spec) <- mean(seriesData(data))
    setOptimSolver(spec) <- "solveShortExact"
    setTrace(spec) <- TRUE
    spec
 
    # Constraints:
    constraints = "Short"
    constraints
    
    # Optimization:
    portfolio = efficientPortfolio(data, spec, constraints)
    portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.efficientPortfolio.MV.LongOnly <- 
    function()
{  
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    setTargetReturn(spec) <- mean(data@Data)
    setTrace(spec) <- TRUE
    spec
 
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # Optimization:
    portfolio = efficientPortfolio(data, spec, constraints)
    portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.efficientPortfolio.MV.LongOnly.Rdonlp2 <- 
    function()
{
    if (require(Rdonlp2))
        
        # Data:
        data = as.timeSeries(data(smallcap.ts))
        data = data[, c("BKE", "GG", "GYMB", "KRON")]
        head(data)
        
        # Specification:
        spec = portfolioSpec()
        setTargetReturn(spec) <- mean(seriesData(data))
        setSolver(spec) = "Rdonlp2"
        spec
        
        # Constraints:
        constraints = "LongOnly"
        constraints
        
        # Efficient Portfolio:
        portfolio = efficientPortfolio(data, spec, constraints)
        portfolio
        
    }
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.efficientPortfolio.BoxConstraints.RDonlp2 = 
    function()
{
    if (FALSE) {
        
        require(Rdonlp2)
        
        # Data:
        data = as.timeSeries(data(smallcap.ts))
        data = data[, c("BKE", "GG", "GYMB", "KRON")]
        head(data)
        
        # Specification:
        spec = portfolioSpec()
        setTargetReturn(spec) = mean(seriesData(data))
        setSolver(spec) = "Rdonlp2"
        spec
        
        # Optimization:
        efficientPortfolio(data, spec, "maxW[1:nAssets]=0.6")
    }
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.efficientPortfolio.MV.LongOnly.twoAssets <- 
    function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    setTargetReturn(spec) = mean(data@Data)
    spec
    
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # Efficient Portfolio:
    Portfolio = efficientPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.efficientPortfolio.MV.Short <- 
    function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    setTargetReturn(spec) = 0.15
    spec
    
    # Constraints - Efficient Portfolio:
    constraints = "Short" 
    constraints
    
    # Portfolio:
    Portfolio = efficientPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.efficientPortfolio.MV.BoxConstraints <- 
    function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    setTargetReturn(spec) <- mean(seriesData(data))
    spec
    
    # Consgtraints:
    constraints = "maxW[1:nAssets]=0.6"
    constraints
    
    # Efficient Portfolio:
    Portfolio = efficientPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


################################################################################

   
test.efficientPortfolio.LPP.LongOnly <- 
    function()
{   
    # Second Example:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # CVaR Specification:
    spec = portfolioSpec()
    setType(spec) = "LPP"
    setEstimator(spec) <- "assetsLPM"
    setTargetReturn(spec) = mean(data@Data)
    spec
    
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # Optimization:
    Portfolio = efficientPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()    
}


################################################################################


test.efficientPortfolio.CVaR.LongOnly = 
function()
{  
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    setType(spec) <- "CVaR"
    setTargetReturn(spec) <- mean(data@Data)
    setTrace(spec) <- TRUE
    spec
 
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # Optimization:
    Portfolio = efficientPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.efficientPortfolio.CVaR.LongOnly.TwoAssets <- 
    function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG")]
    head(data)
    
    # CVaR Specification:
    spec = portfolioSpec()
    setType(spec) = "CVaR"
    setTargetReturn(spec) = mean(data@Data)
    spec
    
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # CVaR Portfolio:
    Portfolio = efficientPortfolio(data, spec, constraints)
    Portfolio 
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.efficientPortfolio.CVaR.LongOnly.Alpha <- 
    function()
{  
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # CVaR Specification:
    spec = portfolioSpec()
    setType(spec) = "CVaR"
    setTargetReturn(spec) = mean(data@Data)
    setAlpha(spec) = 0.10
    spec
    
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # CVaR Portfolio Optimization:
    Portfolio = efficientPortfolio(data, spec, constraints)
    Portfolio 
    
    # Return Value:
    return()    
}


################################################################################
# Tangency Portfolio:


test.tangencyPortfolio.MV.Short <- 
    function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    setRiskFreeRate(spec) = 0.01
    spec
    
    # Constraints - Capital Market Line:
    constraints = "Short"
    constraints
    
    # Portfolio:
    Portfolio = cmlPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.tangencyPortfolio.MV.LongOnly <- 
    function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    spec
    
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # Optimization:
    Portfolio = cmlPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.tangencyPortfolio.MV.LongOnly <- 
    function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    spec
    
    # CML Portfolio - Equals Tangency Portfolio:
    constraints = "LongOnly"
    constraints
    
    # Portfolio - Equals tangency Portfolio:
    Portfolio = cmlPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.tangencyPortfolio.CVaR.LongOnly <- 
    function()
{
    # Linear Programming - CVaR Portfolio:
    #   the return is fixed, we minimie the CVaR
    
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # CVaR Specification:
    spec = portfolioSpec()
    setType(spec) = "CVaR"
    setTargetReturn(spec) = mean(data@Data)
    setAlpha(spec) = 0.05
    spec
    
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # CVaR Portfolio:
    Portfolio = cmlPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


################################################################################
# Minvariance Portfolio


test.minvariancePortfolio.MV.Short <- 
    function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    spec
    
    # Constraints - Minimum Variance Portfolio:
    constraints = "Short"
    constraints
    
    # Portfolio:
    Portfolio = minvariancePortfolio(data, spec, constraints)
    Portfolio 
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.minvariancePortfolio.MV.LongOnly <-
    function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    spec
    
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # Minimum Variance Portfolio:
    Portfolio = minvariancePortfolio(data, spec, constraints)
    Portfolio 
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.minvariancePortfolio.MV.BoxConstrained = 
function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    spec
    
    # Constraints:
    constraints = "maxW[1:nAssets]=0.6"
    constraints
    
    # Minimum Variance Portfolio:
    Portfolio = minvariancePortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.minvariancePortfolio.CVaR.LongOnly =
function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # CVaR Specification:
    spec = portfolioSpec()
    setType(spec) = "CVaR"
    setAlpha(spec) = 0.05
    spec
    
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # CVaR Portfolio Optimization:
    Portfolio = minvariancePortfolio(data, spec, constraints)
    Portfolio 
    
    # Return Value:
    return()
}


################################################################################

