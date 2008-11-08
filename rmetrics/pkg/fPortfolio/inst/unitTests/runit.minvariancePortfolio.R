
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
#  test.minvariancePortfolio.MV.Short
#  test.minvariancePortfolio.MV.LongOnly
#  test.minvariancePortfolio.MV.BoxConstrained
#  test.minvariancePortfolio.CVaR.LongOnly
################################################################################


test.minvariancePortfolio.MV.Short <-
    function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)

    # Specification:
    spec = portfolioSpec()
    setSolver(spec) = "solveRshortExact"
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


################################################################################


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

