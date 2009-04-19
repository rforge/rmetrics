
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


################################################################################


test.backtesting <-
    function()
{
    # Specifications - 
    swxData <- 100 * SWX.RET 
    swxSpec <- portfolioSpec()
    swxBacktest <- portfolioBacktest()
    setWindowsHorizon(swxBacktest) = "18m"
    swxFormula <- LP40 ~ SBI + SPI + SII

    # Portfolio Backtesting -
    portfolios <- portfolioBacktesting(
        formula = swxFormula, 
        data = swxData, 
        spec = swxSpec, 
        constraints = "LongOnly", 
        backtest = swxBacktest, 
        trace = TRUE)

    # Smoothed Rebalancing -   
    smoothedPortfolios <- portfolioSmoothing(
        object = portfolios, 
        backtest = swxBacktest,
        trace = TRUE)  
     
    # Rolling Variance Sigma - 
    series <- rollingSigma(smoothedPortfolios)
    plot(series, main = "Rolling Variance")

    # Rolling Value at Risk -
    series <- rollingVaR(smoothedPortfolios)
    plot(series, main = "Rolling VaR")

    # Rolling Conditional Value at Risk -
    series <- rollingCVaR(smoothedPortfolios)
    plot(series, main = "Rolling CVaR")

    # Rolling Drawdown at Risk -
    series <- rollingDaR(smoothedPortfolios)
    plot(series, main = "Rolling DaR")

    # Rolling Conditional Drawdown at Risk -
    series <- rollingCDaR(smoothedPortfolios)
    plot(series, main = "Rolling CDaR")
   
    # Rolling Conditional Drawdown at Risk -
    series <- backtestStats(smoothedPortfolios, "rollingCDaR")
    plot(series, main = "Rolling CDaR")

    # Return Value:
    return()
}


################################################################################

