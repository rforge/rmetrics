
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
#  test.tangencyPortfolio.MV.LongOnly
#  test.tangencyPortfolio.CVaR.LongOnly
################################################################################



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
    portfolio = tangencyPortfolio(data, spec, constraints)
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
    setTargetReturn(spec) = mean(colMeans(data))
    setAlpha(spec) = 0.05
    print(spec)

    # Constraints:
    constraints = "LongOnly"
    print(constraints)

    # CVaR Portfolio:
    portfolio = tangencyPortfolio(data, spec, constraints)
    print(portfolio)

    # Return Value:
    return()
}


################################################################################

