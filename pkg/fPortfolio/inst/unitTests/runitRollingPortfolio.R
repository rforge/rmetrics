
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
        data = Data[, c("SBI", "SPI", "SII")], 
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


test.portfolioBacktesting.MeanVariance.LP60 = 
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
    
    # Graph Frame:
    par(mfrow = c(2, 2))
    
    # Mean-Variance Backtesting:
    ans = portfolioBacktesting(
        formula = LP60 ~ SBI + SPI + SII, 
        data = Data, 
        spec = portfolioSpec(), 
        constraints = NULL, 
        portfolio = "minvariancePortfolio", 
        horizon = "12m", 
        smoothing = "12m", 
        trace = TRUE)  
        
    #                        Portfolio Benchmark
    # Total Return               34.81     22.53
    # Mean Return                 0.45      0.29
    # StandardDev Return          1.10      2.67
    # VaR 5% Quantile            -1.26     -5.87
    # Var 10% Quantile           -0.76     -3.23
    # 5% Expected Shortfall      -1.83     -6.47
    # Minimum Monthly Return     -2.30     -7.26
 
    # Return Value:
    return()
}        
  
      
# ------------------------------------------------------------------------------       
 

test.portfolioBacktesting.MeanVariance.myPortfolio = 
function()
{    
    # Load Data:
    SWXLP = as.timeSeries(data(SWXLP))
    Data = returnSeries(SWXLP, percentage = TRUE)
    head(Data)
    colnames(Data)
    
    # Strategy Portfolio:
    myPortfolio <<-  
    function(data, spec, constraints)
    {
        strategyPortfolio = tangencyPortfolio(data, spec, constraints)
        Status = strategyPortfolio@portfolio$status
        if(Status == 1)  
            strategyPortfolio = minvariancePortfolio(data, spec, contraints)
        strategyPortfolio
    }
    
    # Mean-Variance Backtesting:
    par(mfrow = c(2, 2))
    portfolioBacktesting(
        formula = LP60 ~ SBI + SPI + SII, 
        data = Data, 
        spec = portfolioSpec(), 
        constraints = NULL, 
        portfolio = "myPortfolio", 
        horizon = "12m", 
        smoothing = "12m", 
        trace = TRUE)  
     
    #                        Portfolio Benchmark
    # Total Return               34.83     22.53
    # Mean Return                 0.45      0.29
    # StandardDev Return          1.28      2.67
    # VaR 5% Quantile            -1.45     -5.87
    # Var 10% Quantile           -0.95     -3.23
    # 5% Expected Shortfall      -2.09     -6.47
    # Minimum Monthly Return     -2.50     -7.26

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
    
    # Strategy Portfolio:
    myPortfolio = 
    function(data, spec, constraints)
    {
        strategyPortfolio = tangencyPortfolio(data, spec, constraints)
        Status = strategyPortfolio@portfolio$status
        if(Status == 1)  
            strategyPortfolio = minvariancePortfolio(data, spec, contraints)
        strategyPortfolio
    }
    
    # Specifications:
    Spec = portfolioSpec()
    setType(Spec) = "CVaR"
    setSolver(Spec) = "lpSolve"
    
    if (FALSE) {
        # Mean-CVaR Backtesting - Please be pagtient ...
        par(mfrow = c(2,2))
        portfolioBacktesting(
            formula = LP60 ~ SBI + SPI + SII, 
            data = Data, 
            spec = Spec,
            constraints = NULL, 
            # portfolio = "minvariancePortfolio", 
            portfolio = "myPortfolio", 
            horizon = "12m", 
            smoothing = "12m", 
            trace = TRUE) 
    }
        
    #                        Portfolio Benchmark
    # Total Return               35.69     22.53
    # Mean Return                 0.46      0.29
    # StandardDev Return          1.29      2.67
    # VaR 5% Quantile            -1.46     -5.87
    # Var 10% Quantile           -0.96     -3.23
    # 5% Expected Shortfall      -2.09     -6.47
    # Minimum Monthly Return     -2.53     -7.26

    # Return Value:
    return()
}

          
# ------------------------------------------------------------------------------

 
test.XXX = 
function() 
{
    # Load Data:
    SWXLP = as.timeSeries(data(SWXLP))
    Data = returnSeries(SWXLP, percentage = TRUE)
    head(Data)
    colnames(Data)
    
    # Graph Frame:
    par(mfrow = c(2, 2))
    
    # Mean-Variance Backtesting:
    ans = portfolioBacktesting(
        formula = LP60 ~ SBI + SPI + SII, 
        data = Data, 
        spec = portfolioSpec(), 
        constraints = NULL, 
        portfolio = "minvariancePortfolio", 
        horizon = "12m", 
        smoothing = "12m", 
        trace = TRUE)  
                
    N = ema = 12
    
    par(mfrow = c(3, 2))
    
    
    # Extrakt Shrinkage Lambda:
    Lambda = attr(getStatistics(ans$tg[[1]])$Sigma, "lambda")
    for (i in 2:length(ans$tg)) {
        lambda = attr(getStatistics(ans$tg[[i]])$Sigma, "lambda")
        Lambda = rbind(Lambda, lambda)      
    }
    plot(x = (1:length(Lambda)) + N, emaTA(Lambda, N), pch = 19, 
        type = "o", xlim = c(0, length(Lambda) + N), main = "lambda" )
    grid()
    
    
    # Extrakt Eigenvalue Ratio:
    Values = eigen(getStatistics(ans$tg[[1]])$Sigma)$values
    Eigen = Values[1]/rev(Values)[1]
    for (i in 2:length(ans$tg)) {
        Values = eigen(getStatistics(ans$tg[[i]])$Sigma)$values
        Eigen = rbind(Eigen, Values[1]/rev(Values)[1])    
    }
    plot(x = (1:length(Eigen)) + N, emaTA(-log(Eigen), N), pch = 19, 
        type = "o", xlim = c(0, length(Eigen) + N), main = "eigen" )
    grid()
    
    
    # Condition Number:
    Kappa = kappa(getStatistics(ans$tg[[1]])$Sigma)
    for (i in 2:length(ans$tg)) {
        kappa = kappa(getStatistics(ans$tg[[i]])$Sigma) 
        Kappa = rbind(Kappa, kappa)    
    }
    plot(x = (1:length(Kappa)) + N, emaTA(-log(Kappa), N), pch = 19, 
        type = "o", xlim = c(0, length(Kappa) + N), main = "kappa" )
    grid()
    
    
    # Collect Portfolio Risk:
    monthlyAssets = applySeries(Data[, 1:3], FUN = colSums)
    pfRisk = getTargetRisk(ans$tg[[1]])
    for (i in 2:length(ans$tg)) {  
        pfRisk = rbind(pfRisk, getTargetRisk(ans$tg[[i]]))
    }
    pfRisk = matrix(pfRisk, ncol = 3)
    pfRisk1 = pfRisk[, 1]
    plot(x = (1:length(pfRisk1)) + N, emaTA(pfRisk1, N), pch = 19, 
        type = "o", xlim = c(0, length(pfRisk1) + N), main = "Covariance Risk" )
    grid()
    pfRisk2 = pfRisk[, 2]
    plot(x = (1:length(pfRisk2)) + N, -emaTA(pfRisk2, N), pch = 19, 
        type = "o", xlim = c(0, length(pfRisk2) + N), main = "-CVaR" )
    grid()
    
     
    # Extrakt Covariance Norm:
    Norm = norm(getStatistics(ans$tg[[1]])$Sigma)
    for (i in 2:length(ans$tg)) {
        norm = norm(getStatistics(ans$tg[[i]])$Sigma)
        Norm = rbind(Norm, norm)      
    }
    plot(x = (1:length(Norm)) + N, emaTA(Norm, N), pch = 19, 
        type = "o", xlim = c(0, length(Norm) + N), main = "Covariance Norm" )  
    grid()
    
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


test.portfolioBacktesting.MeanVariance.LPP = 
function()
{    
    
    # Load Data:
    Data = 100 * as.timeSeries(data(LPP2005REC))
    
    # Spec:
    Spec= portfolioSpec()
    setType(Spec) = "CVaR"
    
    # Strategy Portfolio:
    myPortfolio <<- 
    function(data, spec, constraints)
    {
        strategyPortfolio = tangencyPortfolio(data, spec, constraints)
        Status = strategyPortfolio@portfolio$status
        if(Status == 1)  
            strategyPortfolio = minvariancePortfolio(data, spec, contraints)
        strategyPortfolio
    }
    
    # Graph Frame:
    par(mfrow = c(2, 2))
    
    # Mean-Variance Backtesting:
    ans = portfolioBacktesting(
        formula = LPP60 ~ SBI + SPI + SII + LMI + MPI + ALT,
        data = Data, 
        spec = Spec, 
        constraints = NULL, 
        portfolio = "myPortfolio", 
        horizon = "6m", 
        smoothing = "6m", 
        trace = TRUE)  
        
   
 
    # Return Value:
    return()
}        


################################################################################

    