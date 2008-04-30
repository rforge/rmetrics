
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
#  test.efficientPortfolio.MV.BoxConstraints.RDonlp2
#  test.efficientPortfolio.MV.LongOnly.twoAssets
#  test.efficientPortfolio.LPP.LongOnly
#  test.efficientPortfolio.CVaR.LongOnly
#  test.efficientPortfolio.CVaR.LongOnly.TwoAssets
################################################################################


test.efficientPortfolio.MV.Short <-
    function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    print(head(data))

    # Specification:
    spec = portfolioSpec()
    setTargetReturn(spec) = mean(colMeans(data))
    print(spec)

    # Constraints:
    constraints = "Short"
    print(constraints)

    # Specify Target Return to minimize risk ...
    portfolio = efficientPortfolio(data, spec, constraints)
    print(portfolio)
    print(getSolver(portfolio))
    
    # Specify Target Risk to maximize the return ...
    setTargetRisk(spec) <- getTargetRisk(portfolio)[,"cov"]
    setOptimize(spec) = "maxReturn"
    ## portfolio = efficientPortfolio(data, spec, constraints)
    ## print(portfolio)
    ## print(getSolver(portfolio))
    
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
    setTargetReturn(spec) <- mean(colMeans(data))
    portfolio = efficientPortfolio(data, spec, constraints)
    print(portfolio)
    print(getSolver(portfolio))

    # Return Maximized Optimization:
    setTargetRisk(spec) <- getTargetRisk(portfolio)[, "cov"]
    setOptimize(spec) = "maxReturn"
    ## portfolio = efficientPortfolio(data, spec, constraints)
    ## print(portfolio)
    ## print(getSolver(portfolio))

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
        setSolver(spec) = "solveRdonlp2"
        setTargetReturn(spec) <- mean(colMeans(data))
        portfolio = efficientPortfolio(data, spec, constraints)
        print(portfolio)
        print(getSolver(portfolio))

        # Return Maximized Optimization:
        setTargetRisk(spec) <- getTargetRisk(portfolio)[, "cov"]
        setOptimize(spec) = "maxReturn"
        ## portfolio = efficientPortfolio(data, spec, constraints)
        ## print(portfolio)
        ## print(getSolver(portfolio))

    }

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.efficientPortfolio.MV.BoxConstraints.RDonlp2 =
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
        setTargetReturn(spec) = mean(series(data))
        setSolver(spec) = "solveRdonlp2"
        print(spec)
        
        # Constraints:
        constraints = "maxW[1:nAssets]=0.6"
        print(constraints)
        
        # Optimization:
        ## portfolio = efficientPortfolio(data, spec, constraints)
        ## print(portfolio)
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
# LPP

test.efficientPortfolio.LPP.LongOnly <-
    function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    print(head(data))
    
    if (FALSE) {
        
        # Estimator:
        lpmEstimator <-
        function(data, spec) {
            mu <- colMeans(data)
            Sigma <- assetsLPM(data, tau = colMeans(data), a = 1.5)$Sigma
            list(mu = mu, Sigma = Sigma)
        }
    
        # CVaR Specification:
        spec = portfolioSpec()
        setType(spec) = "LPP"
        setEstimator(spec) <- "lpmEstimator"
        setTargetReturn(spec) = mean(data@Data)
        print(spec)
    
        # Constraints:
        constraints = "LongOnly"
        print(constraints)
    
        # Optimization:
        portfolio = efficientPortfolio(data, spec, constraints)
        print(portfolio)
        
    }

    # Return Value:
    return()
}


################################################################################
# CVAR


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

