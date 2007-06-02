
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
#  rollingOptimalPortfolio           Rolls an optimal portfolio
#  portfolioMonthlyStats             Computes monthly portfolio statistics
################################################################################


rollingWindows =
function(x, period = "24m", by = "1m")
{   # A function implemented by Rmetrics

    # Description:
    #   Returns vectors of start and end dates for a rolling time series
    
    # Arguments:
    #   x - a timeSeries object of asset returns
    #   period - a character string denoting the length of the rolling
    #       window, e.g. "24m" means 24 months
    #   by - a character string denoting the shift of the rolling window,
    #       e.g. "1m" means one month
    
    # Note:
    #   Only "monthly" frequencies are currently supported.
    
    # Example:
    #   x = sort(as.timeSeries(data(smallcap.ts))); rollingWindows(x)
    
    # FUNCTION:
    
    # Get Window Parameter:
    periodLength = as.numeric(substr(period, 1, nchar(period)-1))
    periodUnit = substr(period, nchar(period), nchar(period))
    byLength = as.numeric(substr(by, 1, nchar(by)-1))
    byUnit = substr(by, nchar(by), nchar(by))
    stopifnot(periodUnit == "m")
    stopifnot(byUnit == "m")
    
    # Get Window Parameter:
    periodLength = as.numeric(substr(period, 1, nchar(period)-1))
    periodUnit = substr(period, nchar(period), nchar(period))
    byLength = as.numeric(substr(by, 1, nchar(by)-1))
    byUnit = substr(by, nchar(by), nchar(by))
    stopifnot(periodUnit == "m")
    stopifnot(byUnit == "m")
    
    # Make Windows - We expect monthly data records ...
    positions = seriesPositions(x)
    Positions = unique(timeFirstDayInMonth(positions))
    numberOfPositions = length(Positions)
    startDates = Positions[1:(numberOfPositions-periodLength)]
    endDates = Positions[(periodLength+1):numberOfPositions]-24*3600
    
    # Windows:
    windows = list(from = startDates, to = endDates)
    attr(windows, "control") = c(start = start(positions), end = end(positions))
    
    # Return Value:
    windows
}


# ------------------------------------------------------------------------------


rollingCmlPortfolio =
function(data, spec, constraints, from, to, action = NULL, 
title = NULL, description = NULL, ...)
{   # A function implemented by Rmetrics

    # Description:
    #   Computes EF on a rolling timeSeries Windows
    
    # Arguments:
      
    # FUNCTION:
    
    # Roll the Frontier and return it in a list:
    roll = list()
    for (i in 1:length(from)) {
        
        # Data must be a multivariate timeSeries object ...
        series = cut(data, from = from[i], to = to[i]) 
        
        # Calculation efficient frontiers and save them all in a list:
        portfolio = cmlPortfolio(data = series, spec, constraints)
        roll[[i]] = portfolio
        
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


rollingTangencyPortfolio =
function(data, spec, constraints, from, to, action = NULL, 
title = NULL, description = NULL, ...)
{   # A function implemented by Rmetrics

    # Description:
    #   Computes EF on a rolling timeSeries Windows
    
    # Arguments:
    #   windows - a list with two named 'timeDate' entries, "from" and
    #       "to", defining the start and end dates of your windows.
    #   ... - optional parameters which can be directed to the optional
    #       function action().
      
    # FUNCTION:
    
    # Roll the Frontier and return it in a list:
    roll = list()
    for (i in 1:length(from)) {
        
        # Data must be a multivariate timeSeries object ...
        series = cut(data, from = from[i], to = to[i]) 
        
        # Calculation efficient frontiers and save them all in a list:
        portfolio = tangencyPortfolio(data = series, spec, constraints)
        roll[i] = portfolio
        
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


rollingMinvariancePortfolio =
function(data, spec, constraints, from, to, action = NULL, 
title = NULL, description = NULL, ...)
{   # A function implemented by Rmetrics

    # Description:
    #   Computes EF on a rolling timeSeries Windows
    
    # Arguments:
    #   windows - a list with two named 'timeDate' entries, "from" and
    #       "to", defining the start and end dates of your windows.
    #   ... - optional parameters which can be directed to the optional
    #       function action().
      
    # FUNCTION:
    
    # Roll the Frontier and return it in a list:
    roll = list()
    for (i in 1:length(from)) {
        
        # Data must be a multivariate timeSeries object ...
        series = cut(data, from = from[i], to = to[i]) 
        
        # Calculation efficient frontiers and save them all in a list:
        portfolio = minvariancePortfolio(data = series, spec, constraints)
        roll[i] = portfolio
        
        # Now you can do any "action" you want to do with the EFs:
        if (!is.null(action)) {
            fun = match.fun(action)
            fun(roll, from, to, ...)
        }         
    }
    
    # Return Value:
    invisible(roll)
}


################################################################################


rollingPortfolioFrontier =
function(data, spec, constraints, from, to, action = NULL, 
title = NULL, description = NULL, ...)
{   # A function implemented by Rmetrics

    # Description:
    #   Computes EF on a rolling timeSeries Windows
    
    # Arguments:
    #   windows - a list with two named 'timeDate' entries, "from" and
    #       "to", defining the start and end dates of your windows.
    #   ... - optional parameters which can be directed to the optional
    #       function action().
      
    # FUNCTION:
    
    # Roll the Frontier and return it in a list:
    roll = list()
    for (i in 1:length(from)) {
        
        # Data must be a multivariate timeSeries object ...
        series = cut(data, from = from[i], to = to[i]) 
        
        # Calculation efficient frontiers and save them all in a list:
        frontier = portfolioFrontier(data = series, spec, constraints, 
            title = title, description = description)
        roll[i] = frontier
        
        # Now you can do any "action" you want to do with the EFs:
        if (!is.null(action)) {
            fun = match.fun(action)
            fun(roll, from, to, ...)
        }         
    }
    
    # Return Value:
    invisible(roll)
}


################################################################################


portfolioBacktesting =   
function(formula, data, horizon, smoothing, trace = TRUE)   
{
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
    #   trace - alogical value. Should the backtesting procedure be traced?
    
    # Details:
    #   The rolling backtesting strategy is the following. 
    #       1.  Consider a rolling window of financial returns of length 
    #           'horizon'. 
    #       2.  Compute the target Return foor the tangency portfolio based
    #           on this window and the 'data' listed in the 'formula' 
    #           expression.
    #       3.  If the benchmark return is higher than the target return
    #           of the tangency portfolio, then replace the tangency
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
    
    # The Benchmark Label:
    benchmark = as.character(formula)[2]
    
    # The Asset Labels:
    assets = strsplit(gsub(" ", "", as.character(formula)[3]), "\\+")[[1]]
    
    # Trace the Specifications:
    if(trace) {
        cat("\nPortfolio Backtesting\n")
        cat("\nAssets:             ", assets)
        cat("\nBenchmark:          ", benchmark)
        cat("\nInvestment Horizon: ", horizon, "Months")
        cat("\nUpdate Period:      ",    "1 Month")
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
    From = rW$from 
    To = rW$to 
    from = rW$from[-1] 
    to = rW$to[-1]
    if(trace) {
        cat("\nStart Series:       ", as.character(start(x)))
        cat("\nEnd Series:         ", as.character(end(x)))
    }   
 
    # We invest in the Tangency or Efficient Portfolio:
    if(trace) {
        cat("\n\nPortfolio Optimization:")
        cat("\nOptimization Period\tTarget\tBenchmark\t Weights\n")
    }
    Spec = portfolioSpec()
    setEstimator(Spec) = c("mean", "shrink")
    Constraints = NULL # "maxsumW[2:3]=0.7" 
    tg = rollingOptimalPortfolio(
        data = x[, assets], 
        spec = Spec, 
        constraints = Constraints,
        from = from, 
        to = to,
        benchmark = x[, benchmark],
        action = NULL,
        trace = trace)
            
    # Portfolio Investment Weights:
    #   Performance will be measured at time "i"
    #   Note, we have invested at period i-1 with the weights from period i-1
    Weights = getWeights(tg[[1]])
    for (i in 2:length(tg)) {
        weights = getWeights(tg[[i-1]])
        Weights = rbind(Weights, weights)      
    }
    rownames(Weights) = as.character(to)
    colnames(Weights) = assets
     
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
        text = paste("Horizon = ", horizon, 
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
    ans = list(
        pfReturns = pfReturns.tS, 
        bmReturns = bmReturns.tS, 
        pfWeights = emaWeights)
    attr(ans, "control") <- formula
    
    # Statistics:
    if (trace) {
        cat("\n")
        print(portfolioMonthlyStats(ans))
    }
    
    # Return Value:
    invisible(ans)
}


# ------------------------------------------------------------------------------


rollingOptimalPortfolio =
function(data, spec, constraints, from, to, benchmark, action = NULL, 
trace = TRUE, title = NULL, description = NULL, ...)
{   
    # Description:
    #   Computes tangency/efficient Portfolio dependent on a benchmark   
      
    # FUNCTION:
    
    # Roll the Portfolio and return the Results in a List:
    nAssets = dim(data)[2]
    roll = list()
    for (i in 1:length(from)) {
        
        # Cut Data from the Multivariate timeSeries object "data" of 
        #   assets and from the Univariate "benchmark" time series:
        pfSeries = cut(data, from = from[i], to = to[i]) 
        bmSeries = cut(benchmark, from = from[i], to = to[i])  
        
        # Calculate Tangency Portfolio:
        portfolio = tangencyPortfolio(data = pfSeries, spec, constraints)
        tgReturn = as.numeric(getTargetReturn(portfolio))
        
        # If the benchmark return was higher than the tangency portfolio
        #   target return, then we use the efficient portfolio with the
        #   target return given by the benchmark return:
        bmReturn = mean(as.numeric(bmSeries))
        whichPortfolio = "tg PF"
        if(bmReturn > tgReturn) {
            whichPortfolio = "ef PF"
            Spec = spec
            setTargetReturn(Spec) = bmReturn
            portfolio = efficientPortfolio(
                data = pfSeries, spec = Spec, constraints)
                efReturn = as.numeric(getTargetReturn(portfolio))
            efReturn = as.numeric(getTargetReturn(portfolio))
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


portfolioMonthlyStats =
function(x) 
{   # A function implemented by Rmetrics

    # Description:
    #   Computes some monthly portfolio performance measures
    
    # Arguments:
    #   An object returned by the function portfolioBacktesting
    
    # FUNCTION:
    
    # Portfolio Returns:
    pfReturns = as.numeric(x$pfReturns)
    pfStats = c(
        sum(pfReturns) ,
        mean(pfReturns),
        sd(pfReturns),
        quantile(pfReturns, probs = c(0.05, 0.10)),
        mean(pfReturns[pfReturns < quantile(pfReturns, probs = 0.05)]),
        min(pfReturns) )
    
    # Benchmark Returns:
    bmReturns = as.numeric(x$bmReturns)
    bmStats = c(
        sum(bmReturns),
        mean(bmReturns),
        sd(bmReturns),
        quantile(bmReturns, probs = c(0.05, 0.10)),
        mean(bmReturns[bmReturns < quantile(bmReturns, probs = 0.05)]),
        min(bmReturns))
    
    # Combined Statistics:
    Stats = round(cbind(pfStats, bmStats), digits = 2)
    colnames(Stats) = c("Portfolio", "Benchmark")
    rownames(Stats) = c("Total Return", "Mean Return", "StandardDev Return",
        "VaR 5% Quantile", "Var 10% Quantile", "5% Expected Shortfall",
        "Minimum Monthly Return")
    
    # Return Value:
    Stats
}


################################################################################

