
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR Description. See the 
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA 02111-1307 USA

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
#  portfolioBacktesting              Does portfolio backtesting
#  plot.portfolioBacktest             S3 Plot Method
#  print.portfolioBacktes             S3 Print Method
#  summary.portfolioBacktest          S3 Summary Method
#  .rollingBacktestPortfolio         Rolls a backtesting portfolio
#  .portfolioBacktestingStats         Computes monthly portfolio statistics
################################################################################

    
portfolioBacktesting =   
function(formula, data, spec = portfolioSpec(), constraints = NULL, 
portfolio = "minvariancePortfolio", horizon = "12m", smoothing = "6m", 
trace = TRUE)   
{   # A function implemented by Rmetrics

    # Description:
    #   Does backtesting on a simple rolling portfolio strategy
    
    # Arguments:
    #   formula - a formula expression which tells which assets from the
    #       data set have to be analyzed against a given Benchmark, e.g.
    #       LP40 ~ SBI + SPI + SWIIT backtests a portfolio composed of 
    #       the Swiss Bond Index SBI, the Swiss Performance Index SPI, and
    #       the Swiss Immofunds Index, against the Pictet Benhmark Index 
    #       LP40
    #   data - a multivariate 'timeSeries' object which at least contains
    #       the columns refefrenced in the formula expression.
    #   horizon - the historical investment horizon given in multiples of
    #       months. This is the size of the rolling window on which 
    #       perfolio optimization will be performed.
    #   smoothing - the smoothing period of weights. Weights are exponentially
    #       smoothed given by this period measured in multiples of months.
    #   trace - a logical value. Should the backtesting procedure be traced?
    
    # Details:
    #   The rolling backtesting strategy is the following. 
    #       1.  Consider a rolling window of financial returns of length 
    #           'horizon'. 
    #       2.  Compute the target Return for the "safe" portfolio based
    #           on this window and the 'data' listed in the 'formula' 
    #           expression.
    #       3.  If the benchmark return is higher than the target return
    #           of the "safe" portfolio, then replace the "safe"
    #           portfolio by the efficient portfolio with a target return
    #           given by the benchmark. We call the resulting portfolio
    #           the "optimal" portfolio
    #       4.  Extract the weights from the optimal portfolio and
    #           perform an exponential moving average with a smoothing 
    #           period defined by the value 'smoothing'.
    #       5.  Do an investiment with the obtained weights for the next
    #           month and calculate in the next time step the resulting
    #           gain or loss.
    
    # FUNCTION:

    # Get Horizon Window Parameter:
    Horizon = horizon
    horizonLength = as.numeric(substr(horizon, 1, nchar(horizon)-1))
    horizonUnit = substr(horizon, nchar(horizon), nchar(horizon))
    stopifnot(horizonUnit == "m")
    horizon = horizonLength
    
    # Get Smoothing Window Parameter:
    Smoothing = smoothing
    smoothingLength = as.numeric(substr(smoothing, 1, nchar(smoothing)-1))
    smoothingUnit = substr(smoothing, nchar(smoothing), nchar(smoothing))
    stopifnot(smoothingUnit == "m")
    smoothing = smoothingLength
    
    # The Benchmark Label:
    benchmark = as.character(formula)[2]
    
    # The Asset Labels:
    assets = strsplit(gsub(" ", "", as.character(formula)[3]), "\\+")[[1]]
    
    # Trace the Specifications:
    if(trace) {
        cat("\nPortfolio Backtesting:\n")
        cat("\nPortfolio Strategy: ", portfolio)
        cat("\nPortfolio Type:     ", getType(spec))
        cat("\nPortfolio Solver:   ", getSolver(spec))
        cat("\nAssets:             ", assets)
        cat("\nBenchmark:          ", benchmark)
        cat("\nInvestment Horizon: ", horizon, horizonUnit)
        cat("\nUpdate Period:      ", "1 m")
    }
    
    # The Data:
    x = data
        
    # Create Rolling Windows:
    rW = rollingWindows(x, paste(horizon, "m", sep = ""), "1m")
    from = rW$from 
    to = rW$to 
    if(trace) {
        cat("\nStart Series:       ", as.character(start(x)))
        cat("\nEnd Series:         ", as.character(end(x)))
    }   
 
    # We invest in the "safe" or Efficient Portfolio:
    if(trace) {
        cat("\n\nPortfolio Optimization:")
        cat("\nOptimization Period\tTarget\tBenchmark\t Weights\n")
    }
    Spec = spec
    setEstimator(Spec) = c("mean", "shrink")
    Constraints = constraints
    tg = .rollingBacktestPortfolio(
        data = x[, assets], 
        spec = Spec, 
        constraints = Constraints,
        from = from, 
        to = to,
        benchmark = x[, benchmark],
        portfolio = portfolio,
        action = NULL,
        trace = trace)
        
    # Add Additional Info from Plot Function:    
    attr(tg, "control") = list(
        formula = formula,
        data = data,
        spec = spec,
        constraints = constraints,
        portfolio = portfolio,
        horizon = Horizon,
        smoothing = Smoothing, 
        trace = trace)
    object = list(tg = tg) 
    class(object) = "portfolioBacktest"
    ans = plot(object, which = 0)  
 
    # Trae Statistics:
    if (trace) {
        cat("\n")
        print(.portfolioBacktestingStats(ans))
    }
    
    # Return Value:
    invisible(ans)
}


# ------------------------------------------------------------------------------


plot.portfolioBacktest =   
function(x, labels = TRUE, which = "all", ...)   
{   # A function implemented by Rmetrics

    # Description:
    #   Plots an object of class 'portfolioBacktesting'
   
    # FUNCTION:
    
    # Settings:
    object = x
    object = object$tg
    control = attr(object, "control") 
    formula = control$formula
    data = control$data
    spec = control$spec
    constraints = control$constraints
    portfolio = control$portfolio
    horizon = control$horizon
    smoothing = control$smoothing 
    trace = control$trace
    
    # Get Horizon Window Parameter:
    horizonLength = as.numeric(substr(horizon, 1, nchar(horizon)-1))
    horizonUnit = substr(horizon, nchar(horizon), nchar(horizon))
    stopifnot(horizonUnit == "m")
    horizon = horizonLength
    
    # Get Smoothing Window Parameter:
    smoothingLength = as.numeric(substr(smoothing, 1, nchar(smoothing)-1))
    smoothingUnit = substr(smoothing, nchar(smoothing), nchar(smoothing))
    stopifnot(smoothingUnit == "m")
    smoothing = smoothingLength
    
    # The Benchmark Label:
    benchmark = as.character(formula)[2]
    
    # The Asset Labels:
    assets = strsplit(gsub(" ", "", as.character(formula)[3]), "\\+")[[1]]
    
    # The Data:
    x = data
    
    # Prepare Asset Series and Benchmark Series:
    nAssets = length(assets)
    MAX = -1e99
    for (i in 1:nAssets) MAX = max(c(MAX, cumsum(x[, assets[i]]@Data)) )
    MAX = max(MAX, cumsum(x[, benchmark]@Data))
    MIN = 1e99
    for (i in 1:nAssets) MIN = min(MIN, cumsum(x[, assets[i]]@Data))
    MIN = min(MIN, cumsum(x[, benchmark]@Data))
    
    # Plot 1:
    plot.1 = function(bechmark, assets)
    {
        if (labels) {
            ylab = "Series" 
        } else {
            ylab = ""
        }
        plot(cumsum(x[, benchmark]), type = "l", ylim = c(MIN, MAX), 
            ylab = ylab, col = "black")  
        for (i in 1:nAssets) 
            lines( cumsum(x[, assets[i]]), type = "l", col = i+1)
        if (labels) {
            assetsTitle = paste(assets, collapse = " - ", sep = "")
            title(main = paste(benchmark, "~", assetsTitle))
            grid()
            legend("topleft", legend = c(benchmark, assets), bty = "n", 
                text.col = 1:(nAssets+1))
        }
    }
    if (which == 1 || which == "all") plot.1(benchmark, assets)
           
    # Create Rolling Windows:
    rW = rollingWindows(x, paste(horizon, "m", sep = ""), "1m")
    from = rW$from 
    to = rW$to   
 
    # We invest in the "safe" or Efficient Portfolio:
    Spec = spec
    setEstimator(Spec) = c("mean", "shrink")
    Constraints = constraints
    tg = object
            
    # Extract Portfolio Status:
    Status = (tg[[1]])@portfolio$status
    for (i in 2:length(tg)) {
        status = (tg[[i]])@portfolio$status
        Status = rbind(Status, status)      
    }
    rownames(Status) = as.character(to)
    colnames(Status) = "Status"
      
    # Extract Portfolio Investment Weights:
    #   Performance will be measured at time "i"
    #   Note, we have invested at period i-1 with the weights from period i-1
    Weights = getWeights(tg[[1]])
    for (i in 2:length(tg)) {
        weights = getWeights(tg[[i-1]])
        Weights = rbind(Weights, weights)      
    }
    rownames(Weights) = as.character(to)
    colnames(Weights) = assets
    
    # Extract RiskBudgets:
    RiskBudgets = getCovRiskBudgets(tg[[1]])
    for (i in 2:length(tg)) {
        riskBudgets = getCovRiskBudgets(tg[[i-1]])
        RiskBudgets = rbind(RiskBudgets, riskBudgets)      
    }
    rownames(RiskBudgets) = as.character(to)
    colnames(RiskBudgets) = assets
    
    # Extrakt Shrinkage Lambda:
    Lambda = attr(getStatistics(tg[[1]])$Sigma, "lambda")
    for (i in 2:length(tg)) {
        lambda = attr(getStatistics(tg[[i]])$Sigma, "lambda")
        Lambda = rbind(Lambda, lambda)      
    }
    rownames(Lambda) = as.character(to)
    colnames(Lambda) = "Lambda"
    
    # Extrakt Eigenvalue Ratio:
    Values = eigen(getStatistics(tg[[1]])$Sigma)$values
    Eigen = Values[1]/Values[length(assets)]
    for (i in 2:length(tg)) {
        Values = eigen(getStatistics(tg[[i]])$Sigma)$values
        Eigen = rbind(Eigen, Values[1]/Values[length(assets)])    
    }
    rownames(Eigen) = as.character(to)
    colnames(Eigen) = "Eigen"
     
    # Exponentially Smoothed Weights:
    emaWeights = NULL
    naWeights = matrix(rep(NA, times = horizon*nAssets), ncol = nAssets)
    for (i in 1:nAssets) {
        emaWeights = cbind(emaWeights, emaTA(Weights[,i], lambda = smoothing))
    }
    # Just to be sure that we are fully invested ...
    emaWeights = emaWeights / apply(emaWeights, 1, sum)
    rownames(emaWeights) = as.character(to)
    colnames(emaWeights) = assets
    
    # Plot 2 - Plot Weights:    
    plot.2 = 
    function(emaWeights, naWeights, nAssets, horizon, smoothing)
    {
        # Plot 2:
        if (labels) {
            xlab = ""
            ylab = "Weights Factor"
            main = "Weights Recommendation"
        } else {
            xlab = ""
            ylab = ""
            main = ""
        }
        ts.plot(rbind(naWeights, emaWeights), xlab = xlab, ylab = ylab, 
            ylim = c(0, 1), col = 2:(nAssets+1), main = main)
        if (labels) {
            text = paste(
                "Horizon = ", horizon, 
                "Months | Smoothing:", smoothing, "Months")
            mtext(text, line = 0.5, cex = 0.7)
            grid()
        }
    }
    if(which == 2 || which == "all")
        plot.2(emaWeights, naWeights, nAssets, horizon, smoothing)
        
    # Plot 3 - Plot Weights:
    plot.3 = function(x, emaWeights, naWeights, nAssets)
    {
        naWeights = rbind(naWeights, rep(NA, times = nAssets))
        diffWeights = rbind(naWeights, diff(emaWeights))
        absSum <- function(x) { sum(abs(x)) }
        diffWeights = apply(diffWeights, 1, FUN = absSum)       
        diffWeights = cbind(diffWeights, rbind(naWeights, diff(emaWeights)))
        if (labels) {
            xlab = ""
            ylab = "Weights Changes"
            main = "Weights Rearrangement"
        } else {
            xlab = ""
            ylab = ""
            main = ""
        }
        ts.plot(diffWeights, xlab = xlab, ylab = ylab,
            col = 1:(nAssets+1), main = main)
        if(labels) {
            text = paste(
                "Horizon = ", horizon, 
                "Months | Smoothing:", smoothing, "Months")
            mtext(text, line = 0.5, cex = 0.7)
            grid()
        }
    }
    if (which == 3 || which == "all") 
        plot.3(x, emaWeights, naWeights, nAssets)
    
    # Collect Portfolio Returns:
    monthlyAssets = applySeries(x[, 1:nAssets], FUN = colSums)
    pfReturns = getTargetReturn(tg[[1]])
    for (i in 2:length(tg)) {
        weights = emaWeights[i, ] 
        mu = as.vector(monthlyAssets[as.character(to[i]), ]@Data)
        pfReturns = c(pfReturns, weights %*% mu)   
    }
    pfReturns = matrix(pfReturns, ncol = 1)
    colnames(pfReturns) = "Portfolio"
    rownames(pfReturns) = as.character(to)
    pfReturns.tS = timeSeries(data = pfReturns, charvec = to)   
    
    # Collect Portfolio Risk:
    monthlyAssets = applySeries(x[, 1:nAssets], FUN = colSums)
    pfRisk = getTargetRisk(tg[[1]])
    pfRiskNames = names(pfRisk)
    for (i in 2:length(tg)) {  
        pfRisk = rbind(pfRisk, getTargetRisk(tg[[i]]))
    }
    pfRisk = matrix(pfRisk, ncol = 3)
    colnames(pfRisk) = pfRiskNames
    rownames(pfRisk) = as.character(to)
    pfRisk.tS = timeSeries(data = pfRisk, charvec = to)  
        
    # Collect Benchmark Returns:
    monthlyBenchmark = applySeries(x[, benchmark], FUN = colSums)
    bmReturns = NULL
    for (i in 1:length(tg)) { 
        mu = monthlyBenchmark[as.character(to[i]), ]@Data
        bmReturns = c(bmReturns, mu)     
    }
    bmReturns = matrix(bmReturns, ncol = 1)
    colnames(bmReturns) = benchmark
    rownames(bmReturns) = as.character(to)
    bmReturns.tS = timeSeries(data = bmReturns, charvec = to)    
      
    # Figure 4 - Plot Returns:  
    plot.4 = 
    function(x, benchmark, pfReturns.tS, bmReturns.tS)
    {
        MAX = max(cumsum(x[, benchmark]@Data), cumsum(pfReturns.tS@Data))
        MIN = min(cumsum(x[, benchmark]@Data), cumsum(pfReturns.tS@Data))
        if (labels) {
            ylab = "Total Percentage Return"
            main = "Portfolio versus Benchmark"
        } else {
            ylab = ""
            main = ""
        }
        plot (cumsum(x[, benchmark]), type= "l", col = "black", 
            ylab = ylab, ylim = c(MIN, MAX))
        lines(cumsum(pfReturns.tS), type = "l", col = "red", lwd = 2)
        lines(cumsum(bmReturns.tS), col = "orange", lwd = 2)
        if (labels) {
            title(main = main)
            mtext("red - black", line = 0.5, cex = 0.7)  
            grid()
        }
    }
    if (which == 4 || which == "all")
        plot.4(x, benchmark, pfReturns.tS, bmReturns.tS)
    
    # Result:
    ans = list(tg = tg,
        pfReturns = pfReturns.tS, 
        pfRisk = pfRisk.tS,
        bmReturns = bmReturns.tS, 
        pfWeights = emaWeights)
    attr(ans, "control") <- formula
    class(ans) = c("list", "portfolioBacktest")

    
    # Return Value:
    invisible(ans)
}


# ------------------------------------------------------------------------------


print.portfolioBacktest =
function(x, ...)
{
    # Under Construction ...
    .portfolioBacktestingStats(x)
}


# ------------------------------------------------------------------------------


summary.portfolioBacktest =
function(object, ...)
{
    # Under Construction ...
    .portfolioBacktestingStats(object)
}


################################################################################


.portfolioBacktestingOld =   
function(formula, data, spec = portfolioSpec(), constraints = NULL, 
portfolio = "minvariancePortfolio", horizon = "12m", smoothing = "6m", 
trace = TRUE)   
{   # A function implemented by Rmetrics

    # Description:
    #   Does backtesting on a simple rolling portfolio strategy
    
    # Arguments:
    #   formula - a formula expression which tells which assets from the
    #       data set have to be analyzed against a given Benchmark, e.g.
    #       LP40 ~ SBI + SPI + SWIIT backtests a portfolio composed of 
    #       the Swiss Bond Index SBI, the Swiss Performance Index SPI, and
    #       the Swiss Immofunds Index, against the Pictet Benhmark Index 
    #       LP40
    #   data - a multivariate 'timeSeries' object which at least contains
    #       the columns refefrenced in the formula expression.
    #   horizon - the historical investment horizon given in multiples of
    #       months. This is the size of the rolling window on which 
    #       perfolio optimization will be performed.
    #   smoothing - the smoothing period of weights. Weights are exponentially
    #       smoothed given by this period measured in multiples of months.
    #   trace - a logical value. Should the backtesting procedure be traced?
    
    # Details:
    #   The rolling backtesting strategy is the following. 
    #       1.  Consider a rolling window of financial returns of length 
    #           'horizon'. 
    #       2.  Compute the target Return for the "safe" portfolio based
    #           on this window and the 'data' listed in the 'formula' 
    #           expression.
    #       3.  If the benchmark return is higher than the target return
    #           of the "safe" portfolio, then replace the "safe"
    #           portfolio by the efficient portfolio with a target return
    #           given by the benchmark. We call the resulting portfolio
    #           the "optimal" portfolio
    #       4.  Extract the weights from the optimal portfolio and
    #           perform an exponential moving average with a smoothing 
    #           period defined by the value 'smoothing'.
    #       5.  Do an investiment with the obtained weights for the next
    #           month and calculate in the next time step the resulting
    #           gain or loss.
    
    # FUNCTION:
    
    # Settings:
    doplot = TRUE
    
    # Get Horizon Window Parameter:
    horizonLength = as.numeric(substr(horizon, 1, nchar(horizon)-1))
    horizonUnit = substr(horizon, nchar(horizon), nchar(horizon))
    stopifnot(horizonUnit == "m")
    horizon = horizonLength
    
    # Get Smoothing Window Parameter:
    smoothingLength = as.numeric(substr(smoothing, 1, nchar(smoothing)-1))
    smoothingUnit = substr(smoothing, nchar(smoothing), nchar(smoothing))
    stopifnot(smoothingUnit == "m")
    smoothing = smoothingLength
    
    # The Benchmark Label:
    benchmark = as.character(formula)[2]
    
    # The Asset Labels:
    assets = strsplit(gsub(" ", "", as.character(formula)[3]), "\\+")[[1]]
    
    # Trace the Specifications:
    if(trace) {
        cat("\nPortfolio Backtesting:\n")
        cat("\nPortfolio Strategy: ", portfolio)
        cat("\nPortfolio Type:     ", getType(spec))
        cat("\nPortfolio Solver:   ", getSolver(spec))
        cat("\nAssets:             ", assets)
        cat("\nBenchmark:          ", benchmark)
        cat("\nInvestment Horizon: ", horizon, horizonUnit)
        cat("\nUpdate Period:      ", "1 m")
    }
    
    # The Data:
    x = data
    
    # Figure 1 - Plot Asset Series and Benchmark Series:
    nAssets = length(assets)
    MAX = -1e99
    for (i in 1:nAssets) MAX = max(c(MAX, cumsum(x[, assets[i]]@Data)) )
    MAX = max(MAX, cumsum(x[, benchmark]@Data))
    MIN = 1e99
    for (i in 1:nAssets) MIN = min(MIN, cumsum(x[, assets[i]]@Data))
    MIN = min(MIN, cumsum(x[, benchmark]@Data))
    plot(cumsum(x[, benchmark]), type = "l", ylim = c(MIN, MAX), 
        ylab = "Series", col = "black")   
    for (i in 1:nAssets) lines( cumsum(x[, assets[i]]), type = "l", col = i+1)
    assetsTitle = paste(assets, collapse = " - ", sep = "")
    title(main = paste(benchmark, "~", assetsTitle))
    grid()
    legend("topleft", legend = c(benchmark, assets), bty = "n", 
        text.col = 1:(nAssets+1))
    
    # Create Rolling Windows:
    rW = rollingWindows(x, paste(horizon, "m", sep = ""), "1m")
    from = rW$from 
    to = rW$to 
    if(trace) {
        cat("\nStart Series:       ", as.character(start(x)))
        cat("\nEnd Series:         ", as.character(end(x)))
    }   
 
    # We invest in the "safe" or Efficient Portfolio:
    if(trace) {
        cat("\n\nPortfolio Optimization:")
        cat("\nOptimization Period\tTarget\tBenchmark\t Weights\n")
    }
    Spec = spec
    setEstimator(Spec) = c("mean", "shrink")
    Constraints = constraints
    tg = .rollingBacktestPortfolio(
        data = x[, assets], 
        spec = Spec, 
        constraints = Constraints,
        from = from, 
        to = to,
        benchmark = x[, benchmark],
        portfolio = portfolio,
        action = NULL,
        trace = trace)
            
    # Extract Portfolio Status:
    Status = (tg[[1]])@portfolio$status
    for (i in 2:length(tg)) {
        status = (tg[[i]])@portfolio$status
        Status = rbind(Status, status)      
    }
    rownames(Status) = as.character(to)
    colnames(Status) = "Status"
      
    # Extract Portfolio Investment Weights:
    #   Performance will be measured at time "i"
    #   Note, we have invested at period i-1 with the weights from period i-1
    Weights = getWeights(tg[[1]])
    for (i in 2:length(tg)) {
        weights = getWeights(tg[[i-1]])
        Weights = rbind(Weights, weights)      
    }
    rownames(Weights) = as.character(to)
    colnames(Weights) = assets
    
    # Extract RiskBudgets:
    RiskBudgets = getCovRiskBudgets(tg[[1]])
    for (i in 2:length(tg)) {
        riskBudgets = getCovRiskBudgets(tg[[i-1]])
        RiskBudgets = rbind(RiskBudgets, riskBudgets)      
    }
    rownames(RiskBudgets) = as.character(to)
    colnames(RiskBudgets) = assets
    
    # Extrakt Shrinkage Lambda:
    Lambda = attr(getStatistics(tg[[1]])$Sigma, "lambda")
    for (i in 2:length(tg)) {
        lambda = attr(getStatistics(tg[[i]])$Sigma, "lambda")
        Lambda = rbind(Lambda, lambda)      
    }
    rownames(Lambda) = as.character(to)
    colnames(Lambda) = "Lambda"
    
    # Extrakt Eigenvalue Ratio:
    Values = eigen(getStatistics(tg[[1]])$Sigma)$values
    Eigen = Values[1]/Values[length(assets)]
    for (i in 2:length(tg)) {
        Values = eigen(getStatistics(tg[[i]])$Sigma)$values
        Eigen = rbind(Eigen, Values[1]/Values[length(assets)])    
    }
    rownames(Eigen) = as.character(to)
    colnames(Eigen) = "Eigen"
     
    # Exponentially Smoothed Weights:
    emaWeights = NULL
    naWeights = matrix(rep(NA, times = horizon*nAssets), ncol = nAssets)
    for (i in 1:nAssets) {
        emaWeights = cbind(emaWeights, emaTA(Weights[,i], lambda = smoothing))
    }
    # Just to be sure that we are fully invested ...
    emaWeights = emaWeights / apply(emaWeights, 1, sum)
    rownames(emaWeights) = as.character(to)
    colnames(emaWeights) = assets

    # Trace Investment Period and Investment Weights:
    if(trace) {
        cat("\n\nInvestment Period\tPortfolio Weights - Recommendet Weights:\n")
        printWeights = round(cbind(Weights, emaWeights), digits = 3)
        rownames(printWeights) = paste(
            as.character(timeFirstDayInMonth(to)), as.character(to))
        print(printWeights) 
    }
    
    # Figures 2/3 - Plot Weights:
    if(doplot) {
        # Plot 2:
        ts.plot(rbind(naWeights, emaWeights), xlab = "", 
            ylab = "Weights Factor", ylim = c(0, 1),
            col = 2:(nAssets+1), main = "Weights Recommendation")
        text = paste(
            "Horizon = ", horizon, 
            "Months | Smoothing:", smoothing, "Months")
        mtext(text, line = 0.5, cex = 0.7)
        grid()
        naWeights = rbind(naWeights, rep(NA, times = nAssets))
        diffWeights = rbind(naWeights, diff(emaWeights))
        absSum <- function(x) { sum(abs(x)) }
        diffWeights = apply(diffWeights, 1, FUN = absSum)       
        diffWeights = cbind(diffWeights, rbind(naWeights, diff(emaWeights)))
        # Plot 3:
        ts.plot(diffWeights, xlab = "", 
            ylab = "Weights Changes",
            col = 1:(nAssets+1), main = "Weights Rearrangement")
        mtext(text, line = 0.5, cex = 0.7)
        grid()
    }
    
    # Collect Portfolio Returns:
    monthlyAssets = applySeries(x[, 1:nAssets], FUN = colSums)
    pfReturns = getTargetReturn(tg[[1]])
    for (i in 2:length(tg)) {
        weights = emaWeights[i, ] 
        mu = as.vector(monthlyAssets[as.character(to[i]), ]@Data)
        pfReturns = c(pfReturns, weights %*% mu)   
    }
    pfReturns = matrix(pfReturns, ncol = 1)
    colnames(pfReturns) = "Portfolio"
    rownames(pfReturns) = as.character(to)
    pfReturns.tS = timeSeries(data = pfReturns, charvec = to)   
    
    # Collect Portfolio Risk:
    monthlyAssets = applySeries(x[, 1:nAssets], FUN = colSums)
    pfRisk = getTargetRisk(tg[[1]])
    pfRiskNames = names(pfRisk)
    for (i in 2:length(tg)) {  
        pfRisk = rbind(pfRisk, getTargetRisk(tg[[i]]))
    }
    pfRisk = matrix(pfRisk, ncol = 3)
    colnames(pfRisk) = pfRiskNames
    rownames(pfRisk) = as.character(to)
    pfRisk.tS = timeSeries(data = pfRisk, charvec = to)  
        
    # Collect Benchmark Returns:
    monthlyBenchmark = applySeries(x[, benchmark], FUN = colSums)
    bmReturns = NULL
    for (i in 1:length(tg)) { 
        mu = monthlyBenchmark[as.character(to[i]), ]@Data
        bmReturns = c(bmReturns, mu)     
    }
    bmReturns = matrix(bmReturns, ncol = 1)
    colnames(bmReturns) = benchmark
    rownames(bmReturns) = as.character(to)
    bmReturns.tS = timeSeries(data = bmReturns, charvec = to)    
      
    # Figure 4 - Plot Returns:  
    MAX = max(cumsum(x[, benchmark]@Data), cumsum(pfReturns.tS@Data))
    MIN = min(cumsum(x[, benchmark]@Data), cumsum(pfReturns.tS@Data))
    plot (cumsum(x[, benchmark]), type= "l", col = "black", 
        ylab = "Total Percentage Return", ylim = c(MIN, MAX))
    lines(cumsum(pfReturns.tS), type = "l", col = "red", lwd = 2)
    title(main = "Portfolio versus Benchmark")
    mtext("red - black", line = 0.5, cex = 0.7) 
    lines(cumsum(bmReturns.tS), col = "orange", lwd = 2)
    grid()
    
    # Result:
    ans = list(tg = tg,
        pfReturns = pfReturns.tS, 
        pfRisk = pfRisk.tS,
        bmReturns = bmReturns.tS, 
        pfWeights = emaWeights)
    attr(ans, "control") <- formula
    class(ans) = c("list", "portfolioBacktest")
    
    # Statistics:
    if (trace) {
        cat("\n")
        print(.portfolioBacktestingStats(ans))
    }
    
    # Return Value:
    invisible(ans)
}


# ------------------------------------------------------------------------------


.rollingBacktestPortfolio =
function(data, spec, constraints, from, to, benchmark, portfolio = 
"minvariancePortfolio", action = NULL, trace = TRUE, ...)
{   # A function implemented by Rmetrics
  
    # Description:
    #   Computes strategy/efficient Portfolio dependent on a benchmark   
    
    # Notes:
    #   An internal function called by function 'portfolioBacktesting()'
      
    # FUNCTION:
    
    # Settings:
    portfolioFun = match.fun(portfolio)
    
    # Roll the Portfolio and return the Results in a List:
    nAssets = dim(data)[2]
    roll = list()
    for (i in 1:length(from)) {
        
        # Cut Data from the Multivariate timeSeries object "data" of 
        #   assets and from the Univariate "benchmark" time series:
        pfSeries = cut(data, from = from[i], to = to[i]) 
        bmSeries = cut(benchmark, from = from[i], to = to[i])  
        
        # Calculate "safe" Portfolio:
        portfolio = portfolioFun(data = pfSeries, spec, constraints)
        tgReturn = as.vector(getTargetReturn(portfolio))
        
        # If the benchmark return was higher than the "safe" portfolio
        #   target return, then we use the efficient portfolio with the
        #   target return given by the benchmark portfolio:
        bmReturn = mean(bmSeries@Data)
        whichPortfolio = "tg PF"
        if(bmReturn > tgReturn) {
            Spec = spec
            setTargetReturn(Spec) = bmReturn
            efPortfolio = efficientPortfolio(
                data = pfSeries, spec = Spec, constraints = constraints)
            # The efPortfolio may return with Status 1, i.e. it fails
            #   then we return back to the "safe Portfolio"
            Status = efPortfolio@portfolio$status
            if(Status == 0) {
                portfolio = efPortfolio
                whichPortfolio = "ef PF"
            }          
        }
            
        # Save Results:
        portfolio@portfolio$benchmarkReturn = bmReturn
        weights = round(getWeights(portfolio), digits = 3)
        roll[i] = portfolio
        
        # Trace Optionally the Results:
        if (trace) {    
            cat(as.character(from[i]), as.character(to[i]))
            cat("\t", round(tgReturn, digits = 3))
            cat("\t", round(bmReturn, digits = 3))
            cat("\t", whichPortfolio)
            for (i in 1:nAssets) cat("\t", weights[i])
            cat("\n")
        }
        
        # Now you can do any "action" you want to do with the EFs:
        if (!is.null(action)) {
            fun = match.fun(action)
            fun(roll, from, to, ...)
        }         
    }
    
    # Return Value:
    invisible(roll)
}


# ------------------------------------------------------------------------------


.portfolioBacktestingStats =
function(x) 
{   # A function implemented by Rmetrics

    # Description:
    #   Computes some monthly portfolio performance measures
    
    # Arguments:
    #   x - an object as returned by the function portfolioBacktesting
    
    # Notes:
    #   An internal function called by function 'portfolioBacktesting()'
    
    # FUNCTION:
    
    # Portfolio Returns:
    pfReturns = as.vector(x$pfReturns)
    pfStats = c(
        sum(pfReturns) ,
        mean(pfReturns),
        sd(pfReturns),
        quantile(pfReturns, probs = c(0.05, 0.10)),
        mean(pfReturns[pfReturns < quantile(pfReturns, probs = 0.05)]),
        min(pfReturns) )
    
    # Benchmark Returns:
    bmReturns = as.vector(x$bmReturns)
    bmStats = c(
        sum(bmReturns),
        mean(bmReturns),
        sd(bmReturns),
        quantile(bmReturns, probs = c(0.05, 0.10)),
        mean(bmReturns[bmReturns < quantile(bmReturns, probs = 0.05)]),
        min(bmReturns))
    
    # Combined Statistics:
    Stats = round(cbind(pfStats, bmStats), digits = 2)
    colnames(Stats) = c(
        "Portfolio", 
        "Benchmark")
    rownames(Stats) = c(
        "Total Return", 
        "Mean Return", 
        "StandardDev Return",
        "VaR 5% Quantile", 
        "Var 10% Quantile", 
        "5% Expected Shortfall",
        "Minimum Monthly Return")
    
    # Return Value:
    Stats
}


################################################################################

