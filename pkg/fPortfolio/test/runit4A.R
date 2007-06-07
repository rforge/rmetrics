
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
# FUNCTION:                         DESCRIPTION:
#  rollingWindows                    Returns a list of rolling window frames
# FUNCTION:                         DESCRIPTION:
#  rollingCmlPortfolio               Rolls a CML portfolio
#  rollingTangencyPortfolio          Rolls a tangency portfolio
#  rollingMinvariancePortfolio       Rolls a minimum risk portfolio
# FUNCTION:                         DESCRIPTION:
#  rollingPortfolioFrontier          Rolls a portfolio frontier
# FUNCTION:                         DESCRIPTION:
#  portfolioBacktesting              Does portfolio backtesting
#  .rollingBacktestPortfolio          Rolls a backtesting portfolio
#  .portfolioBacktestingStats         Computes monthly portfolio statistics
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(RollingFrontier, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


################################################################################


test.rollingWindows = 
function()
{
    # Load Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    head(Data)
    dim(Data)
    
    # Rolling Windows:
    windows = rollingWindows(x = Data, period = "12m", by = "1m")
    windows
    
    # Return Value:
    return()  
}


################################################################################


test.rollingCmlPortfolio =
function()
{
    # Load Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    
    # Rolling Windows:
    windows = rollingWindows(x = Data, period = "12m", by = "1m")
    
    # Rolling CML Portfolio:
    ans = rollingCmlPortfolio(data = Data, spec = portfolioSpec(),
        constraints = NULL, from = windows$from, to = windows$to)
    ans[[5]]
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.rollingTangencyPortfolio =
function()
{
    # Load Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    
    # Rolling Windows:
    windows = rollingWindows(x = Data, period = "12m", by = "1m")
    
    # Rolling Tangency Portfolio:
    ans = rollingTangencyPortfolio(data = Data, spec = portfolioSpec(),
        constraints = NULL, from = windows$from, to = windows$to)
    ans[[5]]
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------

   
test.rollingMinvariancePortfolio =
function()
{
    # Load Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    
    # Rolling Windows:
    windows = rollingWindows(x = Data, period = "12m", by = "1m")
    
    # Rolling Portfolio:
    ans = rollingMinvariancePortfolio(data = Data, spec = portfolioSpec(),
        constraints = NULL, from = windows$from, to = windows$to)
    ans[[5]]
    
    # Return Value:
    return()
}


################################################################################


test.rollingPortfolioFrontier =
function()
{
    # Load Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    
    # Rolling Windows:
    windows = rollingWindows(x = Data, period = "12m", by = "1m")
    
    # Rolling Portfolio:
    ans = rollingPortfolioFrontier(data = Data, spec = portfolioSpec(),
        constraints = NULL, from = windows$from, to = windows$to)
    ans[[5]]
    
    # Return Value:
    return()
}


################################################################################


test.rollingBacktestPortfolio =
function()
{
    # Rolls a backtesting portfolio - Internal Function:
    # rollingBacktestPortfolio(data, spec, constraints, from, to, benchmark, 
    #   portfolio = "minvariancePortfolio", action = NULL, trace = TRUE, 
    #   title = NULL, description = NULL, ...)
    
    # Load Data:
    SWXLP = as.timeSeries(data(SWXLP))
    Data = returnSeries(SWXLP, percentage = TRUE)
    head(Data)
    colnames(Data)
    
    # Rolling Windows:
    windows = rollingWindows(x = Data, period = "12m", by = "1m")
    
    # Portfolio Backtesting:
    ans = .rollingBacktestPortfolio(
        data = Data[, c("SBI", "SPI", "SWIIT")], 
        spec = portfolioSpec(), 
        constraints = NULL, 
        from = windows$from, 
        to = windows$to, 
        benchmark = Data[, "LP40"], 
        portfolio = "minvariancePortfolio")
    
    # Return Value:
    return()
}



# ------------------------------------------------------------------------------


test.portfolioBacktesting.MeanVariance = 
function()
{   
    # Does portfolio backtesting ...
    # portfolioBacktesting(formula, data, spec = portfolioSpec(), 
    #   constraints = NULL, portfolio = "minvariancePortfolio", 
    #   horizon = "12m", smoothing = "6m", trace = TRUE)   
    
    # Load Data:
    SWXLP = as.timeSeries(data(SWXLP))
    Data = returnSeries(SWXLP, percentage = TRUE)
    head(Data)
    colnames(Data)
    
    # Rolling Windows:
    windows = rollingWindows(x = Data, period = "12m", by = "1m")
    
    # Mean-Variance Backtesting:
    par(mfrow = c(2, 2), cex = 0.7)
    portfolioBacktesting(
        formula = LP60 ~ SBI + SPI + SWIIT, 
        data = Data, 
        spec = portfolioSpec(), 
        constraints = NULL, 
        portfolio = "minvariancePortfolio", 
        horizon = "12m", 
        smoothing = "6m", 
        trace = TRUE)   
        
    # Mean Variance Result:
    #                        Portfolio Benchmark
    # Total Return               31.21     24.24
    # Mean Return                 0.41      0.31
    # StandardDev Return          0.99      1.73
    # VaR 5% Quantile            -1.24     -3.58
    # Var 10% Quantile           -0.72     -1.96
    # 5% Expected Shortfall      -1.63     -3.90
    # Minimum Monthly Return     -2.07     -4.71
        
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.portfolioBacktesting.MeanVariance.myPortfolio = 
function()
{   
    if (FALSE)  {
    
        myPortfolio = 
        function(data, spec, constraints)
        {
            strategyPortfolio = tangencyPortfolio(data, spec, constraints)
            Status = strategyPortfolio@portfolio$status
            if(Status == 1)  
                strategyPortfolio = minvariancePortfolio(data, spec, contraints)
            strategyPortfolio
        }
        
        # Mean-Variance Backtesting:
            par(mfrow = c(2, 2), cex = 0.7)
            portfolioBacktesting(
                formula = LP60 ~ SBI + SPI + SWIIT, 
                data = Data, 
                spec = portfolioSpec(), 
                constraints = NULL, 
                portfolio = "myPortfolio", 
                horizon = "24m", 
                smoothing = "24m", 
                trace = TRUE)   
                
                
        # CHECK DOESN't WORK !!!
    
    }
            
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.portfolioBacktesting.MeanCVaR = 
function()
{   
    # Does portfolio backtesting ...
    # portfolioBacktesting(formula, data, spec = portfolioSpec(), 
    #   constraints = NULL, portfolio = "minvariancePortfolio", 
    #   horizon = "12m", smoothing = "6m", trace = TRUE)   
    
    # Load Data:
    SWXLP = as.timeSeries(data(SWXLP))
    Data = returnSeries(SWXLP, percentage = TRUE)
    head(Data)
    colnames(Data)
    
    # Specifications:
    Spec = portfolioSpec()
    setType(Spec) = "CVaR"
    setSolver(Spec) = "RlpSolve"
        
    # Rolling Windows:
    windows = rollingWindows(x = Data, period = "12m", by = "1m")
    
    # Mean-CVaR Backtesting:
    par(mfrow = c(2,2), cex = 0.7)
    portfolioBacktesting(
        formula = LP40 ~ SBI + SPI + SWIIT, 
        data = Data, 
        spec = Spec,
        constraints = NULL, 
        portfolio = "minvariancePortfolio", 
        horizon = "12m", 
        smoothing = "6m", 
        trace = TRUE) 
        
    # Mean-CVaR Result:
    #                        Portfolio Benchmark  
    # Total Return               32.33     24.24
    # Mean Return                 0.42      0.31
    # StandardDev Return          1.01      1.73
    # VaR 5% Quantile            -1.22     -3.58
    # Var 10% Quantile           -0.71     -1.96
    # 5% Expected Shortfall      -1.62     -3.90
    # Minimum Monthly Return     -1.98     -4.71


    # Return Value:
    return()
}


################################################################################


if (FALSE) {
    require(RUnit)
    require(lpSolve)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fPortfolio/test/runit4A.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}
   

################################################################################

    