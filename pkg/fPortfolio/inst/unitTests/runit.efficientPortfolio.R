
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
#  test.efficientPortfolio
#  test.efficientPortfolio.MV.Short
#  test.efficientPortfolio.MV.LongOnly
#  test.efficientPortfolio.MV.LongOnly.Rdonlp2
#  test.efficientPortfolio.BoxConstraints.RDonlp2
#  test.efficientPortfolio.MV.LongOnly.twoAssets
################################################################################


test.efficientPortfolio.Default <-
    function()
{
    # The default returns the MV long only tangency portfolio ...

    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    print(head(data))
    
    # Specification
    #   ... use default
    spec = portfolioSpec()
    print(spec)
    
    # Constraints:
    #   ... use default, long only
    constraints = "LongOnly"
    print(constraints)

    # Optimization:
    portfolio = efficientPortfolio(data)
    print(portfolio)

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
    print(head(data))

    # Specification:
    #   ... use default, long only
    spec = portfolioSpec()
    print(spec)

    # Constraints:
    #   ... allow for unlimited short selling
    constraints = "Short"
    print(constraints)

    # Optimization:
    getWeights(spec)
    getTargetReturn(spec)
    getTargetRisk(spec)
    # ... since all three are NULL, the tangency portfolio will be returned:
    try(efficientPortfolio(data, spec, constraints))
    # Fails since ...
    #   Either target return or target risk must be specified!

    # Specify Target Return to minimize risk ...
    setTargetReturn(spec) = mean(as.matrix(data))
    getWeights(spec)
    getTargetReturn(spec)
    getTargetRisk(spec)
    portfolio = efficientPortfolio(data, spec, constraints)
    print(portfolio)

    # Specify Target Risk to maximize the return ...
    setTargetRisk(spec) = getTargetRisk(portfolio)[,"cov"]
    setOptimize(spec) = "maxReturn"
    getWeights(spec)
    getTargetReturn(spec)
    getTargetRisk(spec)
    portfolio = efficientPortfolio(data, spec, constraints)
    print(portfolio)
    
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
    print(head(data))

    # Specification:
    spec = portfolioSpec()
    setTrace(spec) <- TRUE
    print(spec)

    # Constraints:
    constraints = "LongOnly"
    print(constraints)

    # Risk Minimized Optimization:
    setTargetReturn(spec) <- mean(as.matrix(data))
    portfolio = efficientPortfolio(data, spec, constraints)
    print(portfolio)
    getSolver(portfolio)

    # Return Maximized Optimization:
    setTargetRisk(spec) <- getTargetRisk(portfolio)[, "cov"]
    portfolio = efficientPortfolio(data, spec, constraints)
    print(portfolio)
    getSolver(portfolio)

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.efficientPortfolio.MV.LongOnly.Rdonlp2 <-
    function()
{
    # This requires the Rdonlp2 Package:
    if (require(Rdonlp2)) {

        # Data:
        data = as.timeSeries(data(smallcap.ts))
        data = data[, c("BKE", "GG", "GYMB", "KRON")]
        print(head(data))

        # Specification:
        spec = portfolioSpec()
        setTrace(spec) <- TRUE
        print(spec)

        # Constraints:
        constraints = "LongOnly"
        print(constraints)

        # Risk Minimized Optimization:
        setSolver = "solveRdonlp2"
        setTargetReturn(spec) <- mean(as.matrix(data))
        portfolio = efficientPortfolio(data, spec, constraints)
        print(portfolio)
        getSolver(portfolio)

        # Return Maximized Optimization:
        setTargetRisk(spec) <- getTargetRisk(portfolio)[, "cov"]
        portfolio = efficientPortfolio(data, spec, constraints)
        print(portfolio)
        getSolver(portfolio)

    }

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.efficientPortfolio.BoxConstraints.RDonlp2 =
    function()
{
    # This requires the Rdonlp2 Package:
    if (require(Rdonlp2)) 

        # Data:
        data = as.timeSeries(data(smallcap.ts))
        data = data[, c("BKE", "GG", "GYMB", "KRON")]
        print(head(data))

        # Specification:
        spec = portfolioSpec()
        setTargetReturn(spec) = mean(series(data))
        setSolver(spec) = "Rdonlp2"
        print(spec)
        
        # Constraints:
        constraints = "maxW[1:nAssets]=0.6"
        print(constraints)
        
        # Optimization:
        portfolio = efficientPortfolio(data, spec, constraints)
        print(portfolio)
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
    print(head(data))

    # Specification:
    spec = portfolioSpec()
    setTargetReturn(spec) = mean(data@Data)
    print(spec)

    # Constraints:
    constraints = "LongOnly"
    print(constraints)

    # Efficient Portfolio:
    portfolio = efficientPortfolio(data, spec, constraints)
    print(portfolio)

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
    print(head(data))

    # Specification:
    spec = portfolioSpec()
    setTargetReturn(spec) = 0.15
    print(spec)

    # Constraints - Efficient Portfolio:
    constraints = "Short"
    print(constraints)

    # Portfolio:
    portfolio = efficientPortfolio(data, spec, constraints)
    print(portfolio)

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
    print(head(data))

    # Specification:
    spec = portfolioSpec()
    setTargetReturn(spec) <- mean(series(data))
    print(spec)

    # Consgtraints:
    constraints = "maxW[1:nAssets]=0.6"
    print(constraints)

    # Efficient Portfolio:
    portfolio = efficientPortfolio(data, spec, constraints)
    print(portfolio)

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
    print(head(data))

    # CVaR Specification:
    spec = portfolioSpec()
    setType(spec) = "LPP"
    setEstimator(spec) <- "assetsLPM"
    setTargetReturn(spec) = mean(data@Data)
    print(spec)

    # Constraints:
    constraints = "LongOnly"
    print(constraints)

    # Optimization:
    portfolio = efficientPortfolio(data, spec, constraints)
    print(portfolio)

    # Return Value:
    return()
}


################################################################################


test.efficientPortfolio.CVaR.LongOnly <- 
    function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    print(head(data))

    # Specification:
    spec = portfolioSpec()
    setType(spec) <- "CVaR"
    setTargetReturn(spec) <- mean(data@Data)
    setTrace(spec) <- TRUE
    print(spec)

    # Constraints:
    constraints = "LongOnly"
    print(constraints)

    # Optimization:
    portfolio = efficientPortfolio(data, spec, constraints)
    print(portfolio)

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
    print(spec)

    # Constraints:
    constraints = "LongOnly"
    print(constraints)

    # CVaR Portfolio:
    portfolio = efficientPortfolio(data, spec, constraints)
    print(portfolio)

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
    print(spec)

    # Constraints:
    constraints = "LongOnly"
    print(constraints)

    # CVaR Portfolio Optimization:
    portfolio = efficientPortfolio(data, spec, constraints)
    print(portfolio)

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
    print(spec)

    # Constraints - Capital Market Line:
    constraints = "Short"
    print(constraints)

    # Portfolio:
    portfolio = cmlPortfolio(data, spec, constraints)
    print(portfolio)

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
    print(spec)

    # Constraints:
    constraints = "LongOnly"
    print(constraints)

    # Optimization:
    portfolio = cmlPortfolio(data, spec, constraints)
    print(portfolio)

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
    print(spec)

    # CML Portfolio - Equals Tangency Portfolio:
    constraints = "LongOnly"
    print(constraints)

    # Portfolio - Equals tangency Portfolio:
    portfolio = cmlPortfolio(data, spec, constraints)
    print(portfolio)

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
    print(spec)

    # Constraints:
    constraints = "LongOnly"
    print(constraints)

    # CVaR Portfolio:
    portfolio = cmlPortfolio(data, spec, constraints)
    print(portfolio)

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
    print(spec)

    # Constraints - Minimum Variance Portfolio:
    constraints = "Short"
    print(constraints)

    # Portfolio:
    portfolio = minvariancePortfolio(data, spec, constraints)
    print(portfolio)

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
    print(spec)

    # Constraints:
    constraints = "LongOnly"
    print(constraints)

    # Minimum Variance Portfolio:
    portfolio = minvariancePortfolio(data, spec, constraints)
    print(portfolio)

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.minvariancePortfolio.MV.BoxConstrained <- 
    function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)

    # Specification:
    spec = portfolioSpec()
    print(spec)

    # Constraints:
    constraints = "maxW[1:nAssets]=0.6"
    print(constraints)

    # Minimum Variance Portfolio:
    portfolio = minvariancePortfolio(data, spec, constraints)
    print(portfolio)

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.minvariancePortfolio.CVaR.LongOnly <- 
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
    print(spec)

    # Constraints:
    constraints = "LongOnly"
    print(constraints)

    # CVaR Portfolio Optimization:
    portfolio = minvariancePortfolio(data, spec, constraints)
    print(portfolio)

    # Return Value:
    return()
}


################################################################################

